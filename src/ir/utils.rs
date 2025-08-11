use std::collections::{HashMap, HashSet};

use nodit::{Interval, NoditMap};

use crate::ir::ControlFlowGraph;

use super::{BasicBlock, Address, BlockStorage};

pub struct BlockFunctionIterator<'i>{
    blocks:&'i BlockStorage,
    stack:Vec<&'i BasicBlock>,
    visited:HashSet<&'i BasicBlock>,
}

impl<'i> BlockFunctionIterator<'i> {
    pub fn new(block:&'i BasicBlock, blocks:&'i BlockStorage) -> Self {
        Self { blocks, stack: vec![block], visited: HashSet::new() }
    }
}

impl<'i> Iterator for BlockFunctionIterator<'i> {
    type Item = &'i BasicBlock;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(b) = self.stack.pop() {

            self.visited.insert(b);
            for nbr in b.iter_neighbors(self.blocks) {
                    if !self.visited.contains(nbr) {
                    self.stack.push(nbr);
                    self.visited.insert(nbr);
                }
            }
            Some(b)
        } else {
            None
        }
    }
}




pub struct BlockPathIterator<'i>{
    blocks:&'i BlockStorage,
    block:Option<&'i BasicBlock>,
}

impl<'i> BlockPathIterator<'i> {
    pub fn new(block:&'i BasicBlock, blocks:&'i BlockStorage) -> Self {
        Self { blocks, block:Some(block) }
    }
}

impl<'i> Iterator for BlockPathIterator<'i> {
    type Item = &'i BasicBlock;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(block) = self.block {
            let mut i = block.iter_neighbors(self.blocks);
            if let Some(result) = i.next() {
                if i.next().is_none() {
                    self.block = Some(result);
                    return Some(result)
                } else {
                    self.block = None;
                    return None;
                }
            }
        } 

        None
            
    }
}






pub struct BlockNeighborsIterator<'i> {
    block:&'i BasicBlock,
    blocks:&'i BlockStorage,
    yielded_true_branch:bool,
    yielded_all:bool,
}

impl<'i> BlockNeighborsIterator<'i> {
    pub fn new(block:&'i BasicBlock, blocks:&'i BlockStorage) -> Self {
        Self { blocks, block, yielded_true_branch:false, yielded_all:false}
    }
}

impl<'i> Iterator for BlockNeighborsIterator<'i> {
    type Item = &'i BasicBlock;

    fn next(&mut self) -> Option<Self::Item> {
        use super::basic_block::NextBlock::*;
        if self.yielded_all {
            return None
        }
        match &self.block.next {
            ConditionalJump { true_branch, false_branch, .. } => {
                if !self.yielded_true_branch {
                    self.yielded_true_branch = true;
                    self.blocks.get_at_point(*true_branch)
                } else {
                    self.yielded_all = true;
                    self.blocks.get_at_point(*false_branch)
                }
            },
            Call { return_instruction, ..} => {
                self.yielded_all = true;
                self.blocks.get_at_point(*return_instruction)
            },
            Unconditional(address) => {
                self.yielded_all = true;
                self.blocks.get_at_point(*address)
            },
            Return |
            ReturnDifferentSite(_) => None
        }
    }
}

