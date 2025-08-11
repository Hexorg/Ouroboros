use std::collections::HashMap;
use std::sync::Arc;

use nodit::{InclusiveInterval, Interval, NoditMap};
use nodit::interval::ie;
use iced_x86::{Code, Decoder, DecoderOptions, Formatter, Instruction, NasmFormatter, OpKind, Register, SymbolResolver};

use crate::ir::{AbstractSyntaxTree, Address, BasicBlock, BlockStorage, Expression, HighFunction};
use crate::symbol_resolver::{RefSymbolTable, SymbolTable};

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
    pub symbols: SymbolTable, // Need Arc to feed it to iced_x86 disassembly formatter
}

impl<'s> LiteralState<'s> {
    pub fn from_machine_code(bytes:&'s [u8], bitness:u32, ip:u64) -> Self {
        let mut decoder =
            Decoder::with_ip(bitness, bytes, ip, DecoderOptions::NONE);
        let mut instrs = Vec::new();
        while decoder.can_decode() {
            let i = decoder.decode();
            if i.is_invalid() {
                break;
            } else {
                instrs.push(i);
            }
        }
        let bytes = &bytes[0..(decoder.ip()-ip) as usize];
        Self { addr:ip.into(), bytes, kind: LiteralKind::Instruction(instrs)}
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
            ir: NoditMap::new(), 
            functions:HashMap::new(),
            ast: HashMap::new(),
            symbols:SymbolTable::new(), 
        }
    }

    // pub fn get_symbol_resolver(&self) -> Option<Box<dyn SymbolResolver>> {
    //     Some(Box::new(RefSymbolTable::new(self.symbols.clone())))
    // }
}