use nodit::{interval::ie, DiscreteFinite, InclusiveInterval, Interval, NoditMap};
use pcode::VarNode;
use sleigh_compile::ldef::SleighLanguage;
use std::collections::HashSet;

use super::{
    abstract_syntax_tree::AbstractSyntaxTree,
    basic_block::{BlockStorage, DestinationKind, NextBlock},
    control_flow_graph::ControlFlowGraph,
    program_tree_structure::ProgramTreeStructure,
    Expression, ExpressionOp, VariableSymbol,
};
use crate::{ir::expression::InstructionSize, memory::Memory};

use super::{Address, BasicBlock};

#[derive(Copy, Clone, Debug)]
pub enum CallingConvention {
    Cdecl,
}

/// Accumulation of analysis resutls of a single function
pub struct HighFunction {
    pub start: Address,
    pub calling_convention: CallingConvention,
    pub composed_blocks: BlockStorage,
    pub used_call_results: HashSet<VariableSymbol>,
    pub memory_read: HashSet<Expression>,
    pub memory_written: HashSet<Expression>,
    pub function_calls: HashSet<DestinationKind>,
    pub cfg: ControlFlowGraph,
    pub pts: ProgramTreeStructure,
}

fn analysis(
    composed_block: &BasicBlock,
    sp: VarNode,
    used_call_results: &mut HashSet<VariableSymbol>,
    memory_read: &mut HashSet<Expression>,
    memory_written: &mut HashSet<Expression>,
    function_calls: &mut HashSet<DestinationKind>,
) {
    for (addr, value) in composed_block.memory.iter() {
        mark_call_return_used(addr, used_call_results);
        mark_call_return_used(value, used_call_results);
        memory_written.insert(addr.clone());
        for op in value.iter() {
            if let ExpressionOp::Dereference(d) = op {
                let value = value.get_sub_expression(*d);
                // value.dereference();
                memory_read.insert(value);
            }
        }
    }
    match &composed_block.next {
        NextBlock::Jump { condition, .. } => {
            mark_call_return_used(condition, used_call_results);
            for op in condition.iter() {
                if let ExpressionOp::Dereference(d) = op {
                    let value = condition.get_sub_expression(*d);
                    // value.dereference();
                    memory_read.insert(value);
                }
            }
        }
        NextBlock::Call { destination, .. } => {
            function_calls.insert(destination.clone());
        }
        NextBlock::Return => {
            let stack_state = composed_block.registers.get(sp).unwrap();
            if stack_state.get_entry_point() != 0
                || !matches!(stack_state.get(0), ExpressionOp::Variable(_))
            {
                println!(
                    "TODO: Function returns at {} Non-initial stack pointer: ESP = {}",
                    composed_block.identifier, stack_state
                );
                if composed_block.identifier.contains(0x4b1724_u64) {
                    println!("\t this is because MessageBoxA follows stdcall calling convention, not cdecl.")
                }
            }
        }
        _ => (),
    }
}

impl HighFunction {
    pub fn from_mem(addr: impl Into<Address>, mem: &Memory) -> Self {
        let calling_convention = CallingConvention::Cdecl;
        let addr = addr.into();

        let block_id = mem
            .ir
            .slot_by_address(addr)
            .expect("Unable to get IR at function start");
        let block = &mem.ir[block_id];

        let mut composed_blocks = BlockStorage::new();
        let mut used_call_results = HashSet::new();
        let mut memory_read = HashSet::new();
        let mut memory_written = HashSet::new();
        let mut function_calls = HashSet::new();

        composed_blocks.insert(block.clone());
        analysis(
            block,
            mem.lang.sp,
            &mut used_call_results,
            &mut memory_read,
            &mut memory_written,
            &mut function_calls,
        );
        let mut visited = std::collections::HashSet::new();
        let mut stack = vec![block_id];
        let mut copy_vec = Vec::with_capacity(10);

        while let Some(current_block_id) = stack.pop() {
            if !visited.contains(&current_block_id) {
                visited.insert(current_block_id);
                let composed_block = composed_blocks
                    .get_by_identifier(mem.ir[current_block_id].identifier)
                    .unwrap();

                for neighbor_block_id in mem.ir.iter_neighbors(current_block_id) {
                    stack.push(neighbor_block_id);
                    let neighbor_block = &mem.ir[neighbor_block_id];
                    let composed = if let NextBlock::Call {
                        origin,
                        destination,
                        ..
                    } = &composed_block.next
                    {
                        let mut after_call = composed_block.clone();
                        match calling_convention {
                            CallingConvention::Cdecl => {
                                let eax = mem
                                    .lang
                                    .sleigh
                                    .get_reg("EAX")
                                    .and_then(|v| v.get_var())
                                    .unwrap();
                                after_call.registers.set_state(
                                    eax,
                                    Expression::from(VariableSymbol::CallResult {
                                        call_from: *origin,
                                        call_to: Box::new(destination.clone()),
                                    }),
                                );
                            }
                        }
                        // all ret instructions pop return pointer off the stack
                        let mut esp_state = after_call
                            .registers
                            .get_or_symbolic(mem.lang.sp)
                            .into_owned();
                        esp_state.add_value(4, InstructionSize::U32);
                        after_call.registers.set_state(mem.lang.sp, esp_state);
                        neighbor_block.inherit_state_from(&after_call)
                    } else {
                        neighbor_block.inherit_state_from(composed_block)
                    };

                    copy_vec.push(composed);
                }
                while let Some(composed) = copy_vec.pop() {
                    // different paths lead to this composed block.
                    // the states will be potentially different.
                    if let Some(other) = composed_blocks.get_by_identifier(composed.identifier) {
                        if other.registers != composed.registers && !composed.is_return() {
                            println!("Diverging states detected to block {}", composed.identifier);
                            // diverging states to return block can be either different error handling
                            // or getting different cases of a result
                            // for (k, v) in other.registers.iter() {
                            //     if let Some(cv) = composed.registers.get_vec(k) {
                            //         if v != cv {
                            //             println!("Todo: Registers differ, add multiequals")
                            //             // let mut e = v.clone();
                            //             // e.multiequals(cv);
                            //             // printed_regs.insert(*k, e);
                            //         }
                            //     } else {
                            //         // printed_regs.push(*k);
                            //         todo!("\tRegister {k:?} OR Not set");
                            //     }
                            // }
                            // for (k, v) in &composed.registers {
                            //     if !printed_regs.contains_key(k) && !other.registers.contains_key(k) && v != &Expression::from(VariableSymbol::Register(*k)) {
                            //         todo!("\tRegister {k:?} Not set OR {v}")
                            //     }
                            // }
                            // for (reg, state) in printed_regs {
                            //     other.registers.insert(reg, state);
                            // }
                        }
                    } else {
                        analysis(
                            &composed,
                            mem.lang.sp,
                            &mut used_call_results,
                            &mut memory_read,
                            &mut memory_written,
                            &mut function_calls,
                        );
                        //println!("Inserting block {}", composed.identifier);
                        composed_blocks.insert(composed);
                    }
                }
            }
        }

        assert_eq!(composed_blocks.len(), visited.len());
        let cfg = ControlFlowGraph::new(addr, &composed_blocks);
        let pts = ProgramTreeStructure::new(&cfg, &composed_blocks);
        Self {
            start: addr,
            calling_convention,
            composed_blocks,
            cfg,
            pts,
            used_call_results,
            memory_written,
            memory_read,
            function_calls,
        }
    }

    pub fn build_ast(&self, mem: &Memory) -> AbstractSyntaxTree {
        AbstractSyntaxTree::new(self, mem)
    }

    pub fn take_interval_ownership(&self, map: &mut NoditMap<Address, Interval<Address>, Address>) {
        for block in self
            .composed_blocks
            .iter_function(self.composed_blocks.slot_by_address(self.start).unwrap())
        {
            match &self.composed_blocks[block].identifier {
                super::basic_block::BlockIdentifier::Physical(interval) => {
                    map.insert_merge_touching_if_values_equal(*interval, self.start)
                        .unwrap();
                }
                super::basic_block::BlockIdentifier::Virtual(address, _) => {
                    if let Err(e) = map.insert_merge_touching_if_values_equal(
                        ie(*address, address.up().unwrap()),
                        self.start,
                    ) {
                        if e.value != self.start {
                            panic!("{e:?}")
                        }
                    }
                }
                super::basic_block::BlockIdentifier::Unset => (),
            };
        }
    }

    pub fn fill_global_symbols(&self, mem: &mut Memory) {
        // for (_interval, block) in self.composed_blocks.iter() {
        //     mem.ir.get_at_point_mut(block.address).and_then(|b| Some(b.parent_function = self.cfg.start));
        // }
        let symbols = &mut mem.symbols;
        symbols.add(self.start, 4, format!("FUN_{:X}", self.start.0));
        for e in &self.memory_read {
            if let Some(ExpressionOp::Value(v)) = e.root_op() {
                symbols.add(*v, 4, format!("DAT_{:X}", v));
            }
        }
        for e in &self.memory_written {
            if let Some(ExpressionOp::Value(v)) = e.root_op() {
                symbols.add(*v, 4, format!("DAT_{:X}", v));
            }
        }
        for e in &self.function_calls {
            if let DestinationKind::Concrete(v) = e {
                symbols.add(*v, 4, format!("FUN_{:X}", v.0));
            }
        }
    }

    // pub fn is_state_valid(&self, state:&Expression) -> bool {
    //     match self.calling_convention {
    //         CallingConvention::Cdecl => {
    //             state.iter().find(|p| if let ExpressionOp::Variable(v) = p {
    //                 match v {
    //                     VariableSymbol::Register(r) => { *r != Register::ESP},
    //                     _ => false
    //                 }
    //             } else { false }).is_none()
    //         }
    //     }
    // }
}

fn mark_call_return_used(e: &Expression, used_call_sites: &mut HashSet<VariableSymbol>) {
    for op in e.iter() {
        if let ExpressionOp::Variable(v) = op {
            match v {
                VariableSymbol::CallResult { .. } => _ = used_call_sites.insert(v.clone()),
                VariableSymbol::Ram(e, _size) => mark_call_return_used(e, used_call_sites),
                _ => (),
            }
        }
    }
}
