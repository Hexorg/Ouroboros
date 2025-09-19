use std::collections::HashMap;
use std::sync::Arc;

use nodit::{InclusiveInterval, Interval, NoditMap};
use nodit::interval::ie;
use pcode::Block;
use sleigh_compile::ldef::SleighLanguage;
use sleigh_runtime::{Decoder, Instruction};

use crate::ir::{
    address::Address, 
    basic_block::BlockStorage, 
    high_function::HighFunction, 
    abstract_syntax_tree::AbstractSyntaxTree
};
use crate::symbol_resolver::SymbolTable;

pub enum DataKind{

}

pub enum LiteralKind {
    Data(DataKind),
    Instruction(Vec<Instruction>)
}

impl std::fmt::Debug for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Data(arg0) => f.debug_tuple("Data").finish(),
            Self::Instruction(arg0) => f.debug_tuple("Instruction").finish(),
        }
    }
}

pub struct LiteralState<'s> {
    pub addr:Address,
    pub bytes: &'s [u8],
    pub kind: LiteralKind
}

impl std::fmt::Debug for LiteralState<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LiteralState").field("addr", &self.addr).field("size", &self.bytes.len()).field("kind", &self.kind).finish()
    }
}

pub struct Memory<'s>{
    // We have a choice of granularity, a small state per large interval, or large state per small interval.
    pub literal: NoditMap<Address, nodit::Interval<Address>, LiteralState<'s>>,
    pub ir:BlockStorage,
    /// All analyzed functions
    pub functions:HashMap<Address, HighFunction>,
    /// All decompiled functions
    pub ast:HashMap<Address, AbstractSyntaxTree>,
    /// Global symbols
    pub symbols: SymbolTable,
}

impl<'s> LiteralState<'s> {
    pub fn from_machine_code(bytes:&'s [u8], base_addr:u64, lang:&SleighLanguage) -> Self {
        let mut decoder = Decoder::new();
        let mut instrs = Vec::new();

        decoder.global_context = lang.initial_ctx;
        decoder.set_inst(base_addr, bytes);

        let mut instr = Instruction::default();

        while lang.sleigh.decode_into(&mut decoder, &mut instr).is_some() && ((instr.inst_next - base_addr) as usize) <= bytes.len() {
            let i = std::mem::take(&mut instr);
            decoder.set_inst(i.inst_next, &bytes[(i.inst_next - base_addr) as usize..]);
            instrs.push(i);
        }
        
        Self { addr:base_addr.into(), bytes, kind: LiteralKind::Instruction(instrs)}
    }

    pub fn get_interval(&self) -> Interval<Address> {
        ie(self.addr, self.addr + self.bytes.len().into())
    }

    pub fn get_instructions(&self) -> &[Instruction] {
        match &self.kind {
            LiteralKind::Instruction(v) => v,
            _ => panic!("State is not instructions")
        }
    }
}

impl<'s> Memory<'s> {
    pub fn new() -> Self {
        Self { 
            literal: NoditMap::new(), 
            ir: BlockStorage::new(), 
            functions:HashMap::new(),
            ast: HashMap::new(),
            symbols:SymbolTable::new(), 
        }
    }

    // pub fn get_symbol_resolver(&self) -> Option<Box<dyn SymbolResolver>> {
    //     Some(Box::new(RefSymbolTable::new(self.symbols.clone())))
    // }
}