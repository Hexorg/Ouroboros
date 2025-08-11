use std::collections::{HashMap, HashSet};

use iced_x86::Register;
use nodit::{Interval, NoditMap};

use crate::{ir::{basic_block::NextBlock, program_tree_structure::ProgramTreeStructure, AbstractSyntaxTree, BlockStorage, Expression, ExpressionOp, Scope, VariableSymbol}, memory::Memory};

use super::{Address, BasicBlock, ControlFlowGraph, SingleEntrySingleExit};


#[derive(Copy, Clone, Debug)]
pub enum CallingConvention {
    Cdecl,
}

/// Accumulation of analysis resutls of a single function
pub struct HighFunction {
    pub calling_convention: CallingConvention,
    pub composed_blocks: BlockStorage,
    pub used_call_results: HashSet<VariableSymbol>,
    pub memory_read:HashSet<Expression>,
    pub memory_written:HashSet<Expression>,
    pub function_calls:HashSet<Expression>,
    pub cfg: ControlFlowGraph,
    pub pts: ProgramTreeStructure,
}

fn analysis(composed_block:&BasicBlock, used_call_results:&mut HashSet<VariableSymbol>, memory_read:&mut HashSet<Expression>, memory_written:&mut HashSet<Expression>, function_calls:&mut HashSet<Expression>) {
    for (addr, value) in composed_block.memory.iter() {
        mark_call_return_used(addr,  used_call_results);
        mark_call_return_used(value,  used_call_results);
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
        NextBlock::ConditionalJump{condition,..} => {
            mark_call_return_used(condition,  used_call_results);
            for op in condition.iter() {
                if let ExpressionOp::Dereference(d) = op {
                    let value = condition.get_sub_expression(*d);
                    // value.dereference();
                    memory_read.insert(value);
                }
            }
        },
        NextBlock::Call {destination, .. } => {
            function_calls.insert(destination.clone());
        },
        NextBlock::Return => {
            
            let stack_state = composed_block.get_register_state_or_none(&Register::ESP).unwrap();
            if stack_state.get_entry_point() != 0 || !matches!(stack_state.get(0), ExpressionOp::Variable(_)) {
                println!("TODO: Function returns at {} Non-initial stack pointer: ESP = {}", composed_block.address, stack_state);
                if composed_block.address.0 == 0x4b1724 {
                    println!("\t this is because MessageBoxA follows stdcall calling convention, not cdecl.")
                }
            }
        }
        _ => ()
    }
}

impl HighFunction {
    pub fn from_mem(addr:impl Into<Address>, mem:&Memory) -> Self {
        let calling_convention = CallingConvention::Cdecl;
        let addr = addr.into();
        

        let block = mem.ir.get_at_point(addr).expect("Unable to get IR at function start");
        let mut composed_blocks = BlockStorage::new();
        let mut used_call_results = HashSet::new();
        let mut memory_read = HashSet::new();
        let mut memory_written = HashSet::new();
        let mut function_calls = HashSet::new();

        composed_blocks.insert_strict(block.get_interval(), block.clone()).unwrap();
        analysis(block, &mut used_call_results, &mut memory_read, &mut memory_written, &mut function_calls);
        let mut visited = std::collections::HashSet::new();
        let mut stack = vec![block];
        let mut copy_vec = Vec::with_capacity(10);

        while let Some(block) = stack.pop() {
            if !visited.contains(block) {
                visited.insert(block);
                let composed_block = composed_blocks.get_at_point(block.address).unwrap();

                


                
                for nbr in block.iter_neighbors(&mem.ir) {
                    stack.push(nbr);
                    let composed = if let NextBlock::Call { origin, destination, .. } = &composed_block.next {
                        let mut after_call = composed_block.clone();
                        match calling_convention {
                            CallingConvention::Cdecl => _ = after_call.registers.insert(Register::EAX, Expression::from(VariableSymbol::CallResult { call_from: *origin, call_to:destination.clone() })),
                        }
                        nbr.inherit_state_from(&after_call)
                    } else {
                        nbr.inherit_state_from(composed_block)
                    };
                    
                    copy_vec.push(composed);
                }
                while let Some(composed) = copy_vec.pop() {
                    if composed_blocks.contains_point(composed.address) {
                        // different paths lead to this composed block. 
                        // the states will be potentially different.
                        let other = composed_blocks.get_at_point_mut(composed.address).unwrap();
                        if other.registers != composed.registers && !composed.is_return() {
                            // diverging states to return block can be either different error handling
                            // or getting different cases of a result
                            let mut printed_regs = HashMap::new();
                            for (k, v) in &other.registers {
                                if let Some(cv) = composed.registers.get(k) {
                                    if v != cv {
                                        let mut e = v.clone();
                                        e.multiequals(cv);
                                        printed_regs.insert(*k, e);
                                    }
                                } else {
                                    // printed_regs.push(*k);
                                    todo!("\tRegister {k:?}: {v} OR Not set");
                                }
                            }
                            for (k, v) in &composed.registers {
                                if !printed_regs.contains_key(k) && !other.registers.contains_key(k) && v != &Expression::from(VariableSymbol::Register(*k)) {
                                    todo!("\tRegister {k:?} Not set OR {v}")
                                }
                            }
                            for (reg, state) in printed_regs {
                                other.registers.insert(reg, state);
                            }
                        }
                    } else {
                        analysis(&composed, &mut used_call_results, &mut memory_read, &mut memory_written, &mut function_calls);
                        composed_blocks.insert_strict(composed.get_interval(), composed).unwrap();
                    }
                }
            }
        }

        assert_eq!(composed_blocks.len(), visited.len());
        let cfg = ControlFlowGraph::new(addr, &composed_blocks);
        let pts = ProgramTreeStructure::new(&cfg, &composed_blocks);
        Self{calling_convention, composed_blocks, cfg, pts, used_call_results, memory_written, memory_read, function_calls}
    }

    pub fn build_ast(&self, mem:&Memory) -> AbstractSyntaxTree {
        AbstractSyntaxTree::new(self, mem)
    }

    pub fn fill_global_symbols(&self, mem:&mut Memory) {

        for (_interval, block) in self.composed_blocks.iter() {
            mem.ir.get_at_point_mut(block.address).and_then(|b| Some(b.parent_function = self.cfg.start));
        }
        let symbols = &mut mem.symbols;
        symbols.add(self.cfg.start.0, format!("FUN_{:X}", self.cfg.start.0));
        for e in &self.memory_read {
            if let Some(ExpressionOp::Value(v)) = e.last_op() {
                symbols.add(*v as u64, format!("DAT_{:X}", v));
            }
        }
        for e in &self.memory_written{
            if let Some(ExpressionOp::Value(v)) = e.last_op() {
                symbols.add(*v as u64, format!("DAT_{:X}", v));
            }
        }
        for e in &self.function_calls{
            if let Some(ExpressionOp::Value(v)) = e.last_op() {
                symbols.add(*v as u64, format!("FUN_{:X}", v));
            }
        }
    }

    pub fn is_state_valid(&self, state:&Expression) -> bool {
        match self.calling_convention {
            CallingConvention::Cdecl => {
                state.iter().find(|p| if let ExpressionOp::Variable(v) = p { 
                    match v {
                        VariableSymbol::Register(r) => { *r != Register::ESP},
                        _ => false
                    }
                } else { false }).is_none()
            }
        }
    }

}


fn mark_call_return_used(e:&Expression, used_call_sites:&mut HashSet<VariableSymbol>) {
    for op in e.iter() {
        if let ExpressionOp::Variable(v) = op {
            match v {
                VariableSymbol::CallResult { .. } => _ = used_call_sites.insert(v.clone()),
                VariableSymbol::Ram(e) => mark_call_return_used(e, used_call_sites),
                _ => (),
            }
        }
    }
}
