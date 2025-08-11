//! The process of lifting code from machine code to C-like:
//! 
//! * For each defined function, convert machine code to [`BasicBlock`], where each block tracks symbolic state of execution.
//! * Compose [`BasicBlock`]s together as-if executing the function. Execute only 1 round of loops.
//! * Generate [`ControlFlowGraph`] by treating [`BasicBlock`]s as nodes.
//! * Perform dominance and post-dominance analysis of basic blocks to generate [`SingleEntrySingleExit`] (SESE) pairs of the graph
//! * Use SESE pairs to generate [`ProgramTreeStructure`] - which SESEs are nested within other SESEs. This allows us 
//! to decide when to omit else statements in if-else blocks, when to use switch/case or loops.
//! * Traverse [`BasicBlock`]s in the SESE order - outer to inner, generating [`AST`] - Abstract Syntax Tree of the logic.
//! * Use [`ProgramTreeStructure`] to keep track of the scope of variables for each program block. 
//! 

use std::{collections::HashMap, fmt::Write, hash::Hash, ops::Add};

use iced_x86::{Code, Instruction, OpKind, Register};
use nodit::{interval::ie, Interval, NoditMap, OverlapError};

use crate::symbol_resolver::SymbolTable;


mod expression;
mod address;
mod basic_block;
mod control_flow_graph;
mod utils;
mod high_function;
mod abstract_syntax_tree;
mod program_tree_structure;
mod scope;

pub use expression::{Expression, ExpressionOp, VariableSymbol};
pub use address::Address;
pub use basic_block::BasicBlock;
pub use utils::*;
pub use control_flow_graph::*;
pub use scope::*;
pub use high_function::HighFunction;
pub use abstract_syntax_tree::{AbstractSyntaxTree, AstStatement};

pub type BlockStorage = NoditMap<Address, Interval<Address>, BasicBlock>;

fn op_to_expression(mut state:Option<&mut BasicBlock>, instr:&Instruction, op_index:u8, ) -> Expression {
    let (kind, reg) = match op_index {
        0 => {(
            instr.op0_kind(),
            instr.op0_register()
        )},
        1 => {(
            instr.op1_kind(),
            instr.op1_register()
        )},
        _ => panic!("Unsupported op count!")
    };
    match kind {
        OpKind::Register => state.and_then(|s| Some(s.get_register_state(reg).clone())).unwrap_or(Expression::from(reg)),
        OpKind::Memory => {
            let mut base = if instr.memory_base() != Register::None {
                state.as_mut().and_then(|s| Some(s.get_register_state(instr.memory_base()).clone())).unwrap_or(Expression::from(reg)) 
            } else {
                Expression::from(0)
            };

            let displacement = match instr.memory_displ_size() {
                4 => instr.memory_displacement32() as i64,
                _ => instr.memory_displacement64() as i64,
                // n => todo!("Unexpected memory displacement size {n}.")
            };
            
            if instr.memory_base().is_ip() {
                Expression::from(displacement)
            } else {
                if instr.memory_index() != Register::None {
                    let mut scale_reg = state.and_then(|s| Some(s.get_register_state(instr.memory_index()).clone())).unwrap_or(Expression::from(reg));
                    let index = if instr.memory_index_scale() == 1 {
                        scale_reg
                    } else {
                        let scale_size = Expression::from(instr.memory_index_scale() as i64);
                        scale_reg.multiply(&scale_size);
                        scale_reg
                    };
                    base.add(&index);
                }
                base.add_value(displacement);
                base.dereference();
                base
            }
        },
        OpKind::Immediate32to64 => {
            Expression::from(instr.immediate64() as i64)
        },
        OpKind::Immediate32 => {
            Expression::from(instr.immediate32() as i64)
        }
        OpKind::Immediate8to32 => {
            Expression::from(instr.immediate8to32() as i64)
        }
        OpKind::Immediate8 => {
            Expression::from(instr.immediate8() as i64)
        }
        _ => todo!("Unexpected op0 kind: {kind:?}")
    }
}


fn panic_with_interval_info<'a>(blocks:&'a BlockStorage) -> impl FnOnce(OverlapError<BasicBlock>) + use<'a> {
    |e:OverlapError<BasicBlock>| {
        let mut s = format!("{e:?}:\n");
        for (_, block) in blocks.overlapping(e.value.get_interval()) {
            s.write_fmt(format_args!("\t{block:?}\n")).unwrap();
        }
        panic!("{s}")
    }
}

    pub fn lift(instr:&[Instruction]) -> BlockStorage {
        let mut blocks = BlockStorage::new();
        let mut current_block = BasicBlock::new();
        for instruction in instr {
            let ip = instruction.ip();
            let ipa = Address(ip);
            if current_block.address == Address::NULL {
                current_block.address = ipa;
            }
            if blocks.contains_point(ipa) {
                // the reason blocks already contains this point is because other block jumps here
                if blocks.get_at_point(ipa).unwrap().address == current_block.address {
                    // remove_overlapping first removes everything. No need to consume iterator just to remove.
                    _ = blocks.remove_overlapping(ie(ipa, Address(ipa.0 + 2)));
                } else {
                    current_block.next = basic_block::NextBlock::Unconditional(ipa);
                    blocks.insert_strict(current_block.get_interval(), current_block).unwrap_or_else(panic_with_interval_info(&blocks));
                    current_block = BasicBlock::new();
                    current_block.address = ipa;
                    _ = blocks.remove_overlapping(ie(ipa, Address(ipa.0 + 2)));
                }
            }
            current_block.end = instruction.next_ip().into();
        
        
            match instruction.code() {
                Code::Mov_r32_rm32 | Code::Mov_r32_imm32 | Code::Mov_EAX_moffs32=> {
                    let mut left = op_to_expression(None, instruction, 0);
                    let right = op_to_expression(Some(&mut current_block),instruction, 1);

                    let target = left.get_destination_register();
                    current_block.set_register_state(target, right.clone());

                    left.assign(&right);
                    current_block.instruction_map.insert(ipa, left);
                },
                Code::Mov_rm32_imm32 | Code::Mov_rm32_r32 | Code::Mov_moffs32_EAX => {
                    let mut left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(Some(&mut current_block),instruction, 1);

                    let mut result = left.clone();
                    result.assign(&right);

                    left.cancel_dereference();
                    current_block.set_memory_state(left, right);

                    current_block.instruction_map.insert(ipa, result);
                },
                Code::Add_rm32_imm8 | Code::Add_rm32_imm32=> {
                    let target = op_to_expression(None, instruction, 0).get_destination_register();
                    let mut left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(Some(&mut current_block),instruction, 1).get_value();

                    left.add_value(right);
                    let mut result = Expression::from(target);
                    result.assign(&left);
                    
                    current_block.instruction_map.insert(ipa, result);
                    current_block.set_register_state(target, left);
                },
                Code::Add_r32_rm32 | Code::Add_EAX_imm32=> {
                    let target = op_to_expression(None, instruction, 0).get_destination_register();
                    let mut left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(Some(&mut current_block), instruction, 1);
                    left.add(&right);
                    let mut result = Expression::from(target);
                    result.assign(&left);
                    current_block.instruction_map.insert(ipa, result);
                    current_block.set_register_state(target, left);
                },
                Code::Sub_rm32_imm32  | Code::Sub_r32_rm32 => {
                    let mut target = op_to_expression(None, instruction, 0);
                    let reg = target.get_destination_register();
                    let mut left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(Some(&mut current_block),instruction, 1);
                    left.sub(&right);
                    target.assign(&left);
                    current_block.instruction_map.insert(ipa, target);
                    current_block.set_register_state(reg, left);

                },
                Code::Push_r32 | Code::Pushd_imm32 | Code::Pushd_imm8 => {
                    let pushed_value = op_to_expression(Some(&mut current_block), instruction, 0);
                    let esp = Register::ESP;
                    let esp_state = current_block.get_register_state(esp).clone();
                    let mut new_esp_state = esp_state.clone();
                    new_esp_state.sub_value(4);

                    let mut instruction_result = esp_state.clone();
                    instruction_result.dereference();
                    instruction_result.assign(&pushed_value);

                    
                    current_block.instruction_map.insert(ipa, instruction_result);
                    current_block.set_memory_state(esp_state, pushed_value);
                    
                    current_block.set_register_state(esp, new_esp_state);
                },

                Code::Pop_r32 => {
                    let pop_destination = op_to_expression(None, instruction, 0).get_destination_register();
                    let mut esp_state = current_block.get_register_state(Register::ESP).clone();
                    let memory_state = current_block.get_memory_state(esp_state.clone()).clone();


                    esp_state.add_value(4);
                    let mut instruction_result = Expression::from(pop_destination);
                    instruction_result.assign(&memory_state);

                    current_block.set_register_state(Register::ESP, esp_state);
                    current_block.instruction_map.insert(ipa, instruction_result);
                    current_block.set_register_state(pop_destination, memory_state);
                },
                Code::Lea_r32_m => {
                    let destination = op_to_expression(None, instruction, 0).get_destination_register();
                    let mut value = op_to_expression(Some(&mut current_block),instruction, 1);
                    value.cancel_dereference();
                    
                    let mut instruction_result = Expression::from(destination);
                    instruction_result.assign(&value);

                    current_block.instruction_map.insert(ipa, instruction_result);
                    current_block.set_register_state(destination, value);
                },

                Code::Xor_r32_rm32 => {
                    let left = op_to_expression(None, instruction, 0).get_destination_register();
                    let right = op_to_expression(None, instruction, 1).get_destination_register();
                    if left == right {
                        let mut instruction_result = Expression::from(left);
                        instruction_result.assign_value(0);
                        current_block.instruction_map.insert(ipa, instruction_result);
                        current_block.set_register_state(left, 0);
                    } else {
                        todo!("Make XOR")
                    }
                },
                Code::Shr_rm32_imm8 => {
                    let mut instruction_result = op_to_expression(None, instruction, 0);
                    let register = instruction_result.get_destination_register();
                    let mut left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(None, instruction, 1).get_value();
                    left.bit_shift_right(right);
                    
                    instruction_result.assign(&left);
                    current_block.instruction_map.insert(ipa, instruction_result);
                    current_block.set_register_state(register, left);
                },
                Code::And_rm32_imm8 => {
                    let mut instruction_result = op_to_expression(None, instruction, 0);
                    let register = instruction_result.get_destination_register();
                    let mut left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(None, instruction, 1).get_value();
                    left.and(right);
                    
                    instruction_result.assign(&left);
                    current_block.instruction_map.insert(ipa, instruction_result);
                    current_block.set_register_state(register, left);
                }

                Code::Inc_rm32 => {
                    let mut addr = op_to_expression(Some(&mut current_block), instruction, 0);
                    let mut value = current_block.get_memory_state(addr.clone()).clone();
                    value.add_value(1);
                    let mut instruction_result = addr.clone();
                    instruction_result.assign(&value);

            
                    current_block.instruction_map.insert(ipa, instruction_result);
                    addr.cancel_dereference();
                    current_block.set_memory_state(addr, value);


                },
                Code::Stosd_m32_EAX => {
                    // Store ECX-count dwords of whatever is in EAX into pointer starting at EDI
                    current_block.next = basic_block::NextBlock::Unconditional(ipa);
                    current_block.end = ipa;
                    blocks.insert_strict(current_block.get_interval(), current_block).unwrap_or_else(panic_with_interval_info(&blocks));

                    let mut loop_block = BasicBlock::new();
                    loop_block.address = ipa;
                    loop_block.end = instruction.next_ip().into();
                    let eax = Expression::from(VariableSymbol::Register(Register::EAX));
                    let mut ecx = Expression::from(VariableSymbol::Register(Register::ECX));
                    let mut edi = Expression::from(VariableSymbol::Register(Register::EDI));
                    
                    loop_block.next = basic_block::NextBlock::ConditionalJump { 
                        condition: ecx.clone(), 
                        true_branch: ipa, 
                        false_branch: instruction.next_ip().into()
                    };

                    loop_block.set_memory_state(edi.clone(), eax.clone());
                    edi.add_value(4);
                    loop_block.set_register_state(Register::EDI, edi);
                    ecx.sub_value(1);
                    loop_block.set_register_state(Register::ECX, ecx);

                    blocks.insert_strict(loop_block.get_interval(), loop_block).unwrap_or_else(panic_with_interval_info(&blocks));
                    

                    current_block = BasicBlock::new();
                },
                Code::Stosb_m8_AL => {
                    // Store ECX-count bytes of whatever is in AL into pointer starting at EDI
                    current_block.next = basic_block::NextBlock::Unconditional(ipa);
                    current_block.end = ipa;
                    blocks.insert_strict(current_block.get_interval(), current_block).unwrap_or_else(panic_with_interval_info(&blocks));

                    let mut loop_block = BasicBlock::new();
                    loop_block.address = ipa;
                    loop_block.end = instruction.next_ip().into();
                    let al = Expression::from(VariableSymbol::Register(Register::EAX)); // TODO: Reason about AL vs AX vs EAX
                    let mut ecx = Expression::from(VariableSymbol::Register(Register::ECX));
                    let mut edi = Expression::from(VariableSymbol::Register(Register::EDI));
                    
                    loop_block.next = basic_block::NextBlock::ConditionalJump { 
                        condition: ecx.clone(), 
                        true_branch: ipa, 
                        false_branch: instruction.next_ip().into()
                    };

                    loop_block.set_memory_state(edi.clone(), al.clone());
                    edi.add_value(1);
                    loop_block.set_register_state(Register::EDI, edi);
                    ecx.sub_value(1);
                    loop_block.set_register_state(Register::ECX, ecx);

                    blocks.insert_strict(loop_block.get_interval(), loop_block).unwrap_or_else(panic_with_interval_info(&blocks));
                    

                    current_block = BasicBlock::new();
                },
                Code::Movsd_m32_m32 => {
                    // Store ECX-count dwords from DS:[ESI] to ES:[EDI]
                    current_block.next = basic_block::NextBlock::Unconditional(ipa);
                    current_block.end = ipa;
                    blocks.insert_strict(current_block.get_interval(), current_block).unwrap_or_else(panic_with_interval_info(&blocks));

                    let mut loop_block = BasicBlock::new();
                    loop_block.address = ipa;
                    loop_block.end = instruction.next_ip().into();
                    let mut esi = Expression::from(VariableSymbol::Register(Register::ESI));
                    let mut ecx = Expression::from(VariableSymbol::Register(Register::ECX));
                    let mut edi = Expression::from(VariableSymbol::Register(Register::EDI));
                    
                    // loop_block.next = Expression::from(instruction.next_ip() as i64);
                    // loop_block.conditional_jump = Some((ecx.clone(), loop_block.address));
                    loop_block.next = basic_block::NextBlock::ConditionalJump { 
                        condition: ecx.clone(), 
                        true_branch: ipa, 
                        false_branch: instruction.next_ip().into()
                    };

                    loop_block.set_memory_state(edi.clone(), esi.clone());
                    edi.add_value(4);
                    esi.add_value(4);
                    loop_block.set_register_state(Register::EDI, edi);
                    loop_block.set_register_state(Register::ESI, esi);
                    ecx.sub_value(1);
                    loop_block.set_register_state(Register::ECX, ecx);

                    blocks.insert_strict(loop_block.get_interval(), loop_block).unwrap_or_else(panic_with_interval_info(&blocks));
                    

                    current_block = BasicBlock::new();
                },
                Code::Movsb_m8_m8 => {
                    // Store ECX-count bytes from DS:[ESI] to ES:[EDI]
                    current_block.next = basic_block::NextBlock::Unconditional(ipa);
                    current_block.end = ipa;
                    blocks.insert_strict(current_block.get_interval(), current_block).unwrap_or_else(panic_with_interval_info(&blocks));

                    let mut loop_block = BasicBlock::new();
                    loop_block.address = ipa;
                    loop_block.end = instruction.next_ip().into();
                    let mut esi = Expression::from(VariableSymbol::Register(Register::ESI));
                    let mut ecx = Expression::from(VariableSymbol::Register(Register::ECX));
                    let mut edi = Expression::from(VariableSymbol::Register(Register::EDI));
                    
                    loop_block.next = basic_block::NextBlock::ConditionalJump { 
                        condition: ecx.clone(), 
                        true_branch: ipa, 
                        false_branch: instruction.next_ip().into()
                    };

                    loop_block.set_memory_state(edi.clone(), esi.clone()); // TODO: Only 1 byte is copied
                    edi.add_value(1);
                    esi.add_value(1);
                    loop_block.set_register_state(Register::EDI, edi);
                    loop_block.set_register_state(Register::ESI, esi);
                    ecx.sub_value(1);
                    loop_block.set_register_state(Register::ECX, ecx);

                    blocks.insert_strict(loop_block.get_interval(), loop_block).unwrap_or_else(panic_with_interval_info(&blocks));
                    

                    current_block = BasicBlock::new();
                },
                Code::Cmp_r32_rm32 | Code::Cmp_rm32_r32 => {
                    let mut left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(Some(&mut current_block),instruction, 1);

                    left.sub(&right);
                    
                    current_block.instruction_map.insert(ipa, left.clone());
                    current_block.next = basic_block::NextBlock::ConditionalJump { condition: left, true_branch: Address::NULL, false_branch: Address::NULL };
                },
                Code::Cmp_EAX_imm32 | Code::Cmp_rm32_imm8 => {
                    let mut left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(Some(&mut current_block),instruction, 1).get_value();

                    left.sub_value(right);
                    current_block.instruction_map.insert(ipa, left.clone());
                    current_block.next = basic_block::NextBlock::ConditionalJump { condition: left, true_branch: Address::NULL, false_branch: Address::NULL };
                },
                Code::Test_rm32_r32 => {
                    let left = op_to_expression(Some(&mut current_block), instruction, 0);
                    let right = op_to_expression(Some(&mut current_block),instruction, 1);
                    if left == right {
                        current_block.next = basic_block::NextBlock::ConditionalJump { condition: left, true_branch: Address::NULL, false_branch: Address::NULL };
                    } else {
                        todo!("Implement Expression::AND")
                    }
                },
                Code::Call_rel32_32 | Code::Call_rm32 => {
                    let call_site = match instruction.op0_kind() {
                        OpKind::NearBranch32 => {
                            Expression::from(instruction.near_branch32() as i64)
                        },
                        OpKind::Memory => {
                            op_to_expression(Some(&mut current_block), instruction, 0)
                        }
                        _ => todo!("Unexpected call kind: {:?}", instruction.op0_kind())
                    };

                    current_block.next = basic_block::NextBlock::Call { origin: ipa, destination: call_site, return_instruction:instruction.next_ip().into() };
                    blocks.insert_strict(current_block.get_interval(), current_block).unwrap_or_else(panic_with_interval_info(&blocks));

                    current_block = BasicBlock::new(); 
                    // current_block.address = instruction.next_ip().into();
                    // // next block has pre-set EAX register, assuming cdecl 
                    // current_block.set_register_state(Register::EAX, VariableSymbol::CallResult{call_from:Address(instruction.ip()), call_to:call_site});

                },
                Code::Retnd => {
                    current_block.next = basic_block::NextBlock::Return;
                    blocks.insert_strict(current_block.get_interval(), current_block).unwrap_or_else(panic_with_interval_info(&blocks));
                    current_block = BasicBlock::new();
                }
                c @ (Code::Je_rel8_32 | Code::Jne_rel8_32 | Code::Jl_rel32_32 | Code::Ja_rel32_32) => {
                    let call_site:Address = match instruction.op0_kind() {
                        OpKind::NearBranch32 => {
                            instruction.near_branch32().into()
                        },
                        _ => todo!("Unexpected call kind: {:?}", instruction.op0_kind())
                    };
                    if let basic_block::NextBlock::ConditionalJump { mut condition,.. } = current_block.next {
                        if let Some(block) = blocks.get_at_point(call_site) {
                            if block.address != call_site {
                                todo!("Jump to a middle of a defined block.")
                            }
                        } else {
                            let mut true_block = BasicBlock::new();
                            true_block.address = call_site;
                            true_block.end = Address(call_site.0 + 1);
                            blocks.insert_strict(true_block.get_interval(), true_block).unwrap_or_else(panic_with_interval_info(&blocks));
                        }
                        let mut false_condition = condition.clone();
                        match c {
                            Code::Je_rel8_32 => {
                                condition.check_equals_value(0);
                                false_condition.check_not_equals_value(0);
                            },
                            Code::Jne_rel8_32 => {
                                condition.check_not_equals_value(0);
                                false_condition.check_equals_value(0);
                            },
                            Code::Jl_rel32_32 => {
                                condition.check_less_value(0);
                                false_condition.check_greater_or_equals_value(0);
                            },
                            Code::Ja_rel32_32 => { // jump grater, unsigned, TODO: How to keep track of signed/unsigned/flags logic?
                                condition.check_greater_value(0);
                                false_condition.check_less_or_equals_value(0);
                            },
                            _ => panic!("Unexpected jump code")
                        };
    
                        let no_jump_addr:Address = instruction.next_ip().into();
                        // current_block.conditional_jump = Some((true_branch.clone(), call_site));
                        // current_block.next = Expression::from(no_jump_addr.0 as i64);
                        current_block.next = basic_block::NextBlock::ConditionalJump{ 
                            condition, 
                            true_branch: call_site, 
                            false_branch: no_jump_addr };
    
                        // self.blocks.insert(call_site, jump_state);
                        let mut no_jump_state = BasicBlock::new();
                        no_jump_state.address = no_jump_addr;
                        no_jump_state.end = Address(no_jump_addr.0 + 1);
                        no_jump_state.constraints.push(false_condition);
                        
                        blocks.insert_strict(current_block.get_interval(), current_block).unwrap_or_else(panic_with_interval_info(&blocks));
                        current_block = no_jump_state;

                    } else {
                        panic!("Conditional jump before condition is set");
                    }
                },
                code => {
                    println!("Process instruction {:x}: {code:?}", instruction.ip());
                }
            }
        }
    blocks 
}