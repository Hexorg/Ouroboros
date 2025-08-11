use std::{collections::HashMap, sync::Arc};

use iced_x86::*;

use crate::ir::{Expression, VariableDefinition, VariableSymbol, VariableType};

pub struct SymbolTable {
    pub map: HashMap<u64, VariableDefinition>
}

pub struct RefSymbolTable(Arc<SymbolTable>);

impl SymbolResolver for RefSymbolTable {
    fn symbol(
            &mut self, instruction: &Instruction, operand: u32, instruction_operand: Option<u32>, address: u64, address_size: u32,
        ) -> Option<SymbolResult<'_>> {
        if let Some(symbol_string) = self.0.map.get(&address) {
            // The 'address' arg is the address of the symbol and doesn't have to be identical
            // to the 'address' arg passed to symbol(). If it's different from the input
            // address, the formatter will add +N or -N, eg. '[rax+symbol+123]'
            Some(SymbolResult::with_str(address, symbol_string.name.as_str()))
        } else {
            None
        }
    }
}

impl Into<RefSymbolTable> for Arc<SymbolTable> {
    fn into(self) -> RefSymbolTable {
        RefSymbolTable(self.clone())
    }
}

impl RefSymbolTable {
    pub fn new(st: Arc<SymbolTable>) -> Self {
        st.into()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn add(&mut self, address:u64, symbol:String) {
        self.map.insert(address, VariableDefinition { 
            kind: VariableType { name: String::new() }, 
            name: symbol, 
            variable: VariableSymbol::Ram(Expression::new()) });
    }

    pub fn resolve(&self, e:&VariableSymbol) -> Option<&VariableDefinition> {
        self.map.get(&e.get_memory_address_or_null())
    }

    pub fn resolve_exp(&self, e:&Expression) -> Option<&VariableDefinition> {
        self.map.get(&e.get_memory_address_or_null())
    }
}