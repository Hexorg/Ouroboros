use navigation::Navigation;
use nodit::interval::ie;
use nodit::{Interval, NoditMap};

use sleigh_compile::ldef::SleighLanguage;
use sleigh_runtime::{Decoder, Instruction, Lifter as InstructionToPcode};
use std::borrow::Cow;
use std::collections::HashMap;

pub mod navigation;

use crate::ir::{
    abstract_syntax_tree::AbstractSyntaxTree, address::Address, basic_block::BlockStorage,
    high_function::HighFunction,
};
use crate::symbol_resolver::SymbolTable;

pub enum LiteralKind {
    Data(Vec<u8>),
    Instruction(usize, Vec<Instruction>),
}

impl std::fmt::Debug for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Data(_) => f.debug_tuple("Data").finish(),
            Self::Instruction(_, _) => f.debug_tuple("Instructions").finish(),
        }
    }
}

impl LiteralKind {
    pub fn size(&self) -> usize {
        match self {
            Self::Data(data) => data.len(),
            Self::Instruction(size, _) => *size,
        }
    }
}

pub struct LiteralState {
    pub addr: Address,
    pub kind: LiteralKind,
}

impl std::fmt::Debug for LiteralState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LiteralState")
            .field("addr", &self.addr)
            .field("kind", &self.kind)
            .finish()
    }
}

pub struct Memory {
    // We have a choice of granularity, a small state per large interval, or large state per small interval.
    pub literal: NoditMap<Address, nodit::Interval<Address>, LiteralState>,
    pub lang: SleighLanguage,
    pub ir: BlockStorage,
    pub navigation: Navigation,
    /// All analyzed functions
    pub functions: HashMap<Address, HighFunction>,
    /// All decompiled functions
    pub ast: HashMap<Address, AbstractSyntaxTree>,
    /// Global symbols
    pub symbols: SymbolTable,
}

impl LiteralState {
    pub fn from_bytes<A: Into<Address>>(addr: A, bytes: Vec<u8>) -> Self {
        Self {
            addr: addr.into(),
            kind: LiteralKind::Data(bytes),
        }
    }

    pub fn from_machine_code(
        bytes: Cow<[u8]>,
        base_addr: u64,
        lang: &SleighLanguage,
    ) -> Option<Self> {
        let mut decoder = Decoder::new();
        let mut instrs = Vec::new();
        let mut lifter = InstructionToPcode::new();

        decoder.global_context = lang.initial_ctx;
        decoder.set_inst(base_addr, bytes.as_ref());

        let mut instr = Instruction::default();

        while lang.sleigh.decode_into(&mut decoder, &mut instr).is_some()
            && ((instr.inst_next - base_addr) as usize) < bytes.len()
        {
            let i = std::mem::take(&mut instr);
            if let Ok(block) = lifter.lift(&lang.sleigh, &i) {
                if block.instructions.len() > 1 {
                    let start = (i.inst_next - base_addr) as usize;
                    decoder.set_inst(i.inst_next, &bytes[start..]);
                    instrs.push(i);
                    if bytes[start..(start + 16)].iter().all(|b| *b == 0) {
                        // Assuming no valid instructions compose 16 zero bytes.
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if let Some(last_instruction) = instrs.last() {
            let size = last_instruction.inst_next - base_addr;
            Some(Self {
                addr: base_addr.into(),
                kind: LiteralKind::Instruction(size as usize, instrs),
            })
        } else {
            None
        }
    }

    pub fn get_interval(&self) -> Interval<Address> {
        ie(self.addr, self.addr + self.kind.size().into())
    }

    pub fn get_instructions(&self) -> &[Instruction] {
        match &self.kind {
            LiteralKind::Instruction(_, v) => v,
            _ => panic!("State is not instructions"),
        }
    }
}

impl Memory {
    pub fn new(lang: SleighLanguage) -> Self {
        Self {
            lang,
            literal: NoditMap::new(),
            ir: BlockStorage::new(),
            functions: HashMap::new(),
            navigation: Navigation::new(),
            ast: HashMap::new(),
            symbols: SymbolTable::new(),
        }
    }

    // pub fn get_symbol_resolver(&self) -> Option<Box<dyn SymbolResolver>> {
    //     Some(Box::new(RefSymbolTable::new(self.symbols.clone())))
    // }
}
