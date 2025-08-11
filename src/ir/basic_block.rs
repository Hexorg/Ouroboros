
use std::{borrow::Cow, collections::HashMap};
use core::hash::Hash;

use nodit::{interval::ie, Interval};

use crate::ir::{high_function::CallingConvention, BlockPathIterator};

use super::{Address, Expression, Register, VariableSymbol, BlockStorage, BlockFunctionIterator, BlockNeighborsIterator};

#[derive(Clone)]
pub enum NextBlock {
    ConditionalJump{
        condition:Expression,
        true_branch:Address,
        false_branch:Address
    },
    Call{
        origin:Address,
        destination:Expression,
        return_instruction:Address,
    },
    Return,
    ReturnDifferentSite(Expression),
    Unconditional(Address)
}

impl Default for NextBlock {
    fn default() -> Self {
        Self::Return
    }
}


/// A basic block is a straight-line sequence of instructions with only one entry point and one exit point.
/// * One entry → control can only enter the block at its first instruction (no jumps into the middle).
/// * One exit → control leaves only at the last instruction.
/// * Inside the block, instructions always execute sequentially, no branching.
#[derive(Clone)]
pub struct BasicBlock {
    /// Start of a block
    pub address:Address,
    /// Non-inclusive address where the block ends
    pub end:Address,
    /// Address of a function that uses this block.
    /// Used in drawing composed data flow in instruction view.
    pub parent_function: Address,
    /// Stores false branch or return destination.
    pub next:NextBlock,
    /// Symbolic state of registers at the end of this block
    pub registers:HashMap<Register, Expression>,
    /// Symbolic state of memory at the end of this block
    pub memory:HashMap<Expression, Expression>,
    /// Constraints on any symbolic expression in `registers` or `memory` 
    /// These are assigned during conditional jump
    pub constraints:Vec<Expression>,
    /// How does each instruction affect IR
    pub instruction_map: HashMap<Address, Expression>,
}

impl Hash for BasicBlock {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.address.hash(state);
    }
}

impl PartialEq for BasicBlock {
    fn eq(&self, other: &Self) -> bool {
        self.address == other.address
    }
}

impl Eq for BasicBlock {}


impl std::fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Block({}-{})", self.address, self.end))
    }
}

impl BasicBlock {
    pub fn new() -> Self {
        Self { 
            registers: HashMap::new(), 
            memory:HashMap::new(), 
            address:Address::default(), 
            parent_function: Address::default(),
            end:Address::default(), 
            next:NextBlock::default(), 
            constraints:Vec::new(), 
            instruction_map: HashMap::new(),
        }
    }

    pub fn get_register_state(&mut self, reg:Register) -> &Expression {
        self.registers.entry(reg).or_insert(Expression::from(VariableSymbol::Register(reg)))
    }

    pub fn get_register_state_or_none(&self, reg:&Register) -> Option<&Expression> {
        self.registers.get(reg)
    }   

    pub fn get_memory_state<E:Into<Expression>>(&mut self, addr:E) -> &Expression {
        let addr = addr.into();
        self.memory.entry(addr.clone()).or_insert(Expression::from(VariableSymbol::Ram(addr)))
    }

    pub fn get_memory_state_or_none<'e, E:Into<&'e Expression>>(&self, addr:E) -> Option<&Expression> {
        let addr = addr.into();
        self.memory.get(addr)
    }

    pub fn set_register_state<E:Into<Expression>>(&mut self, reg:Register, state:E) {
        self.registers.insert(reg, state.into());
    }

    pub fn set_memory_state<E:Into<Expression>>(&mut self, addr:E, state:E) {
        self.memory.insert(addr.into(), state.into());
    }
    pub fn get_interval(&self) -> Interval<Address> {
        ie(self.address, self.end)
    }

    /// Iterate over every block in a function, starting from this one.
    pub fn iter_function<'i>(&'i self, blocks:&'i BlockStorage) -> BlockFunctionIterator<'i> {
        BlockFunctionIterator::new(self, blocks)
    }

    /// Iterate over this block's direct neighbors. 
    pub fn iter_neighbors<'i>(&'i self, blocks:&'i BlockStorage) -> BlockNeighborsIterator<'i> {
        BlockNeighborsIterator::new(self, blocks)
    }

    pub fn iter_path<'i>(&'i self, blocks:&'i BlockStorage) -> BlockPathIterator<'i> {
        BlockPathIterator::new(self, blocks)
    }

    pub fn is_return(&self) -> bool {
        match self.next {
            NextBlock::Return | NextBlock::ReturnDifferentSite(_) => true,
            _ => false
        }
    }

    /// "execute" this block right after `other` - inheriting `other`'s state and modifying our own state as if the execution continued.
    pub fn inherit_state_from(&self, other:&Self) -> Self {
        let mut registers = other.registers.clone();
        // if self.address == Address(0x4b1642) {
        //     println!("I'm {} Inheriting from previous block {}:", self.address, other.address);
        //     for (r, e) in &registers {
        //         println!("\t{r:?} = {e}");
        //     }
        // }
        let mut memory = other.memory.clone();


        

        fn replace<'a>(other:&'a BasicBlock) -> impl Fn(&VariableSymbol) -> Option<Cow<'a, Expression>> {
            |var:&VariableSymbol|
            match var {
                VariableSymbol::Register(r) => other.registers.get(r).and_then(|v| Some(Cow::Borrowed(v))),
                VariableSymbol::CallResult{..} => None,
                VariableSymbol::Ram(d) => {
                    let mut d = d.clone();
                    d.replace_variable_with(replace(other));
                    if let Some(value) = other.memory.get(&d) {
                        Some(Cow::Borrowed(value))
                    } else {
                        println!("Use without def {d}");
                        let e = Expression::from(VariableSymbol::Ram(d));
                        Some(Cow::Owned(e))
                    }
                }
            }
        }
        
        let mut instruction_map = self.instruction_map.clone();
        for (_addr, expression) in instruction_map.iter_mut() {
            expression.replace_variable_with(replace(other));
        }

       

        for (addr, value) in &self.memory {
            let mut addr = addr.clone();
            addr.replace_variable_with(replace(other));
            let mut value = value.clone();
            value.replace_variable_with(replace(other));
            memory.insert(addr, value);
        }

        let mut constraints = other.constraints.clone();
        for c in &self.constraints {
            let mut c = c.clone();
            c.replace_variable_with(replace(other));
            constraints.push(c);
        }

        let next = match &self.next {
            NextBlock::ConditionalJump { condition, true_branch, false_branch } => {
                let mut condition = condition.clone();
                condition.replace_variable_with(replace(other));
                NextBlock::ConditionalJump { condition, true_branch: *true_branch, false_branch: *false_branch }
            },
            NextBlock::Call { origin, destination, return_instruction } => {
                let mut destination = destination.clone();
                destination.replace_variable_with(replace(other));
                NextBlock::Call { origin:*origin, destination, return_instruction:*return_instruction }
            },
            NextBlock::ReturnDifferentSite(expression) => {
                let mut expression = expression.clone();
                expression.replace_variable_with(replace(other));
                NextBlock::ReturnDifferentSite(expression)
            },
            otherwise => otherwise.clone()
        };

        for (r, state) in &self.registers {
            let mut state = state.clone();
            // if we're referencing a register that previous block didn't have (it won't be in registers) then its variable is an unknown anyway
            // and we don't need to replace it with anything.
            state.replace_variable_with(replace(other));
            registers.insert(*r, state);
        }


        Self { 
            address: self.address,
            end: self.end,
            parent_function: self.parent_function,
            next,
            registers,
            memory,
            instruction_map,
            constraints,
        }
    }
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self::new()
    }
}

