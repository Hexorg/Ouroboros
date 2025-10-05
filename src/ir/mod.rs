//! The process of lifting code from machine code to C-like:
//!
//! * Create [`sleigh_compile::SleighLanguageBuilder`] from Ghidra's Processor definition files.
//! * Convert machine code to PCode intermediate representation ([`sleigh_runtime::Instruction`]), using [`from_machine_code`]
//! * Convert PCode blocks to [`BasicBlock`]s by using [`PCodeToBasicBlocks`].
//!     - *Note:* one PCode block may yield 2 or more [`BasicBlock`]s, but only for some instructions.
//! * For each defined function:
//!     - Compose [`BasicBlock`]s together as-if executing the function. Execute only 1 round of loops.
//!     - Generate [`ControlFlowGraph`] by treating [`BasicBlock`]s as nodes.
//!     - Perform dominance and post-dominance analysis of basic blocks to generate [`SingleEntrySingleExit`] (SESE) pairs of the graph
//!     - Use SESE pairs to generate [`ProgramTreeStructure`] - which SESEs are nested within other SESEs. This allows us
//! to decide when to omit else statements in if-else blocks, when to use switch/case or loops.
//!     - Traverse [`BasicBlock`]s in the SESE order - outer to inner, generating [`AbstractSyntaxTree`] of the logic.
//!     - Use [`ProgramTreeStructure`] to keep track of the scope of variables for each program block.
//!

use std::collections::{HashMap, HashSet};

use nodit::interval::ie;
use sleigh_compile::ldef::SleighLanguage;
use sleigh_runtime::{Decoder, Instruction, Lifter as InstructionToPCode};

pub mod abstract_syntax_tree;
pub mod address;
pub mod basic_block;
pub mod control_flow_graph;
pub mod expression;
pub mod high_function;
pub mod program_tree_structure;
pub mod scope;
pub mod type_system;

use address::Address;
use basic_block::{
    BasicBlock, BlockIdentifier, BlockSlot, BlockStorage, CpuState, DestinationKind,
};
use expression::{Expression, ExpressionOp, VariableSymbol};

use crate::ir::basic_block::NextBlock;

pub fn from_machine_code(bytes: &[u8], base_addr: u64, lang: &SleighLanguage) -> Vec<Instruction> {
    let mut decoder = Decoder::new();
    let mut instrs = Vec::new();

    decoder.global_context = lang.initial_ctx;
    decoder.set_inst(base_addr, bytes);

    let mut instr = Instruction::default();

    while lang.sleigh.decode_into(&mut decoder, &mut instr).is_some()
        && ((instr.inst_next - base_addr) as usize) <= bytes.len()
    {
        let i = std::mem::take(&mut instr);
        decoder.set_inst(i.inst_next, &bytes[(i.inst_next - base_addr) as usize..]);
        instrs.push(i);
    }

    instrs
}

pub fn lift(
    instructions: &[Instruction],
    lang: &SleighLanguage,
    storage: Option<BlockStorage>,
) -> BlockStorage {
    let mut pcode_lifter = InstructionToPCode::new();
    let mut my_lifter = PCodeToBasicBlocks::new();
    if let Some(storage) = storage {
        my_lifter.blocks = storage;
    }
    let mut dasm = String::new();
    for instruction in instructions {
        let pcode = pcode_lifter.lift(&lang.sleigh, instruction).unwrap();
        my_lifter.scan_for_block_boundaries(pcode);
    }
    for instruction in instructions {
        let pcode = pcode_lifter.lift(&lang.sleigh, instruction).unwrap();
        if pcode.instructions.len() == 1 {
            break;
        }
        // dasm.clear();
        // lang.sleigh.disasm_into(instruction, &mut dasm);
        // Iterpath node BlockSlot(41), 0x4b165b-0x4b1668, next: Call { origin: 0x4b1664, destination: Concrete(0x4b0760), next_block: Some(BlockSlot(41)) }
        // if instruction.inst_start == 0x401017 {
        //     println!("0x{:x} {dasm}", instruction.inst_start);
        //     for pcode in &pcode.instructions {
        //         println!("\t{:?}\t{:?}", pcode.op, pcode)
        //     }
        // }
        my_lifter.lift(pcode);
    }
    println!("Created {} blocks", my_lifter.blocks.len());
    my_lifter.blocks
}

struct PCodeToBasicBlocks {
    pub blocks: BlockStorage,
    current_block: BasicBlock,
    current_block_start_marker: Option<Address>,
    known_block_boundaries: HashSet<Address>,
}

fn get_state(v: pcode::Value, size: u8, registers: &mut CpuState) -> Expression {
    match v {
        pcode::Value::Var(var_node) => registers.get_or_symbolic(var_node).clone(),
        pcode::Value::Const(value, _size) => Expression::from(value),
    }
}

impl PCodeToBasicBlocks {
    pub fn new() -> Self {
        Self {
            blocks: BlockStorage::new(),
            current_block: BasicBlock::new(),
            current_block_start_marker: None,
            known_block_boundaries: HashSet::new(),
        }
    }

    fn scan_for_block_boundaries(&mut self, pcode_block: &pcode::Block) {
        use pcode::Op::Branch;
        for pcode in &pcode_block.instructions {
            match pcode.op {
                Branch(_) => {
                    let destination =
                        get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    if !destination.is_symbolic() {
                        let dst_address: Address = destination.get_value().into();
                        self.known_block_boundaries.insert(dst_address);
                    }
                }
                _ => (),
            }
        }
    }

    fn lift(&mut self, pcode_block: &pcode::Block) {
        fn add_block(
            current_block: &mut BasicBlock,
            current_block_start_marker: &mut Option<Address>,
            blocks: &mut BlockStorage,
            instruction_pointer: Address,
            next_instruction_pointer: Address,
        ) -> BlockSlot {
            let mut block = std::mem::take(current_block);
            let start = std::mem::take(current_block_start_marker);
            block.clear_temporary_registers();
            if let Some(start_addr) = start {
                let interval = ie(start_addr, next_instruction_pointer);
                block.identifier = BlockIdentifier::Physical(interval);

                blocks.insert(block)
            } else {
                block.identifier = BlockIdentifier::Virtual(
                    instruction_pointer,
                    blocks.next_available_multiblock_id_at_address(instruction_pointer),
                );
                blocks.insert(block)
            }
        }

        let mut instruction_pointer = Address::NULL;
        let mut next_instruction_pointer = Address::NULL;
        let mut last_added_block = None;

        // Map of block id to lookup and u16 lable to insert into true branch
        let mut blocks_to_patch_jump: HashMap<BlockSlot, u16> = HashMap::new();
        // map of u16 lables to destination block ids
        let mut pcode_label_map: HashMap<u16, u8> = HashMap::new();

        use expression::SignedOrUnsiged::*;
        use pcode::Op::*;

        for pcode in &pcode_block.instructions {
            match pcode.op {
                InstructionMarker => {
                    if self
                        .known_block_boundaries
                        .contains(&pcode.inputs.first().as_u64().into())
                        && !self.current_block.is_new()
                    {
                        // print!("InstructionMarker::");
                        self.current_block.next = basic_block::NextBlock::Follow(
                            DestinationKind::Concrete(pcode.inputs.first().as_u64().into()),
                        );
                        last_added_block = Some(add_block(
                            &mut self.current_block,
                            &mut self.current_block_start_marker,
                            &mut self.blocks,
                            instruction_pointer,
                            pcode.inputs.first().as_u64().into(),
                        ));
                    }

                    instruction_pointer = pcode.inputs.first().as_u64().into();
                    next_instruction_pointer =
                        instruction_pointer + pcode.inputs.second().as_u64().into();

                    if self.current_block_start_marker.is_none() {
                        self.current_block_start_marker = Some(instruction_pointer)
                    }
                }
                IntAdd => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);

                    left.add(&right, pcode.inputs.first().size());

                    if !pcode.output.is_temp() {
                        let mut assignment =
                            Expression::from(ExpressionOp::DestinationRegister(pcode.output));
                        assignment.assign(&left);
                        self.current_block
                            .key_instructions
                            .insert(instruction_pointer, assignment);
                    }

                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntAnd => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.and(&right);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntXor => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.xor(&right);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntCountOnes => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    left.count_ones();
                    self.current_block.registers.set_state(pcode.output, left);
                }
                Load(space) => {
                    let addr = get_state(pcode.inputs.first(), &mut self.current_block.registers);

                    assert!(pcode.inputs.second().is_invalid());
                    let value = match space {
                        pcode::RAM_SPACE => self
                            .current_block
                            .get_memory_state(addr, pcode.inputs.first().size()),
                        pcode::REGISTER_SPACE => {
                            self.current_block.registers.get_or_symbolic(pcode.output)
                        }
                        a => todo!("Unsupported memory space {a}"),
                    }
                    .clone();
                    if !pcode.output.is_temp() {
                        let mut assignment =
                            Expression::from(ExpressionOp::DestinationRegister(pcode.output));
                        assignment.assign(&value);
                        self.current_block
                            .key_instructions
                            .insert(instruction_pointer, assignment);
                    }
                    self.current_block.registers.set_state(pcode.output, value);
                }
                Store(space) => {
                    let addr = get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let value = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    self.current_block.memory_writes.insert(addr.clone());

                    let mut assignment = addr.clone();
                    assignment.dereference();
                    assignment.assign(&value);
                    self.current_block
                        .key_instructions
                        .insert(instruction_pointer, assignment);

                    match space {
                        pcode::RAM_SPACE => self.current_block.set_memory_state(addr, value),
                        a => todo!("Unsupported memory space {a}"),
                    }
                }
                Copy => {
                    let state = get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    self.current_block.registers.set_state(pcode.output, state);
                }
                IntLess => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.check_less(&right, pcode.inputs.first().size(), Unsigned);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntSignedLess => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.check_less(&right, pcode.inputs.first().size(), Signed);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntSignedBorrow => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.sub(&right, pcode.inputs.first().size());
                    left.overflow(Signed);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntCarry => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.add(&right, pcode.inputs.first().size());
                    left.overflow(Unsigned);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntSignedCarry => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.add(&right, pcode.inputs.first().size());
                    left.overflow(Signed);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntSub => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.sub(&right, pcode.inputs.first().size());

                    if !pcode.output.is_temp() {
                        let mut assignment =
                            Expression::from(ExpressionOp::DestinationRegister(pcode.output));
                        assignment.assign(&left);
                        self.current_block
                            .key_instructions
                            .insert(instruction_pointer, assignment);
                    }

                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntNegate => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    assert!(pcode.inputs.second().is_invalid());
                    left.negate(pcode.inputs.first().size());
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntMul => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.multiply(&right, pcode.inputs.first().size());
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntEqual => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.check_equals(&right, pcode.inputs.first().size(), Unsigned);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntLeft => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers)
                        .get_value();
                    left.bit_shift_left(right, pcode.inputs.first().size());
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntRight => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers)
                        .get_value();
                    left.bit_shift_right(right, pcode.inputs.first().size());
                    self.current_block.registers.set_state(pcode.output, left);
                }
                BoolNot | IntNot | IntNotEqual => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    left.not();
                    self.current_block.registers.set_state(pcode.output, left);
                }
                IntOr | BoolOr => {
                    let mut left =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    let right = get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    left.or(&right);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                PcodeBranch(lbl) => {
                    let condition =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    // lbl will map to BlockId, but we may not know this id yet.
                    let false_branch = DestinationKind::Virtual(
                        instruction_pointer,
                        self.blocks
                            .next_available_multiblock_id_at_address(instruction_pointer)
                            + 1,
                    ); // just next available one is our own
                    self.current_block.next = basic_block::NextBlock::Jump {
                        condition,
                        true_branch: DestinationKind::Virtual(instruction_pointer, 0),
                        false_branch,
                    };
                    // print!("PcodeBranch::");
                    self.current_block_start_marker = None;
                    let id = add_block(
                        &mut self.current_block,
                        &mut self.current_block_start_marker,
                        &mut self.blocks,
                        instruction_pointer,
                        next_instruction_pointer,
                    );
                    blocks_to_patch_jump.insert(id, lbl);
                    last_added_block = Some(id);
                }
                PcodeLabel(lbl) => {
                    let next = self
                        .blocks
                        .next_available_multiblock_id_at_address(instruction_pointer);
                    pcode_label_map.insert(lbl, next);

                    self.current_block.next =
                        basic_block::NextBlock::Follow(DestinationKind::Virtual(
                            instruction_pointer,
                            self.blocks
                                .next_available_multiblock_id_at_address(instruction_pointer),
                        ));
                    // print!("PcodeLabel::");
                    last_added_block = Some(add_block(
                        &mut self.current_block,
                        &mut self.current_block_start_marker,
                        &mut self.blocks,
                        instruction_pointer,
                        next_instruction_pointer,
                    ));
                }
                ZeroExtend => {
                    let left = get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    self.current_block.registers.set_state(pcode.output, left);
                }
                Branch(hint) => {
                    let condition =
                        get_state(pcode.inputs.first(), &mut self.current_block.registers);
                    if condition.is_symbolic() {
                        self.current_block
                            .key_instructions
                            .insert(instruction_pointer, condition.clone());
                    }
                    let destination =
                        get_state(pcode.inputs.second(), &mut self.current_block.registers);
                    let destination = if destination.is_symbolic() {
                        self.current_block
                            .key_instructions
                            .insert(instruction_pointer, destination.clone());
                        DestinationKind::Symbolic(destination)
                    } else {
                        DestinationKind::Concrete(destination.get_value().into())
                    };
                    match hint {
                        pcode::BranchHint::Call => {
                            self.current_block.next = basic_block::NextBlock::Call {
                                origin: instruction_pointer,
                                destination,
                                default_return: next_instruction_pointer,
                            }
                        }
                        pcode::BranchHint::Jump => {
                            let false_branch = DestinationKind::Concrete(next_instruction_pointer);
                            self.current_block.next = basic_block::NextBlock::Jump {
                                condition,
                                true_branch: destination,
                                false_branch,
                            }
                        }
                        pcode::BranchHint::Return => {
                            self.current_block.next = basic_block::NextBlock::Return
                        }
                    }
                    // print!("Branch::");
                    last_added_block = Some(add_block(
                        &mut self.current_block,
                        &mut self.current_block_start_marker,
                        &mut self.blocks,
                        instruction_pointer,
                        next_instruction_pointer,
                    ));
                }
                a => {
                    todo!("Execute {instruction_pointer}: PCode: {a:?}: {pcode:?}")
                }
            }
        }

        if blocks_to_patch_jump.len() > 0 {
            if let Some(last_added_block) = last_added_block {
                match self.blocks[last_added_block].next {
                    basic_block::NextBlock::Follow(DestinationKind::Virtual(_, _)) => {
                        self.blocks[last_added_block].next =
                            NextBlock::Follow(DestinationKind::Concrete(next_instruction_pointer))
                    }
                    _ => panic!("Unexpected virtual block end."),
                }
            }
        }

        for (block_id, lbl) in blocks_to_patch_jump {
            let dst = *pcode_label_map.get(&lbl).unwrap();
            let debug_id = self.blocks[block_id].identifier;
            match &mut self.blocks[block_id].next {
                basic_block::NextBlock::Jump {
                    true_branch,
                    false_branch,
                    ..
                } => match true_branch {
                    DestinationKind::Virtual(addr, idx) => *idx = dst,
                    _ => panic!("Unexpected destination made it into patch block list"),
                },
                _ => panic!("Unexpected block pactch state."),
            }
        }
    }
}
