use std::collections::HashMap;

use crate::ir::{
    address::Address, 
    basic_block::DestinationKind,
    expression::{Expression, ExpressionOp, VariableSymbol},
    scope::VariableDefinition,
    type_system::VariableType
};

pub struct SymbolTable {
    pub map: HashMap<Address, VariableDefinition>
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn add<A:Into<Address>>(&mut self, address:A, size:u8, symbol:String) {
        let address = address.into();
        self.map.insert(address, VariableDefinition { 
            kind: VariableType::default(), 
            name: symbol, 
            variable: VariableSymbol::Ram(Box::new(Expression::from(address)), size)});
    }

    pub fn resolve(&self, e:&VariableSymbol) -> Option<&VariableDefinition> {
        match e {
            VariableSymbol::Varnode(_) |
            VariableSymbol::CallResult{..} => None,
            VariableSymbol::Ram(e, _) => self.resolve_exp(e),
        }
    }

    pub fn resolve_mut(&mut self, e:&VariableSymbol) ->Option<&mut VariableDefinition> {
        match e {
            VariableSymbol::Varnode(_) |
            VariableSymbol::CallResult{..} => None,
            VariableSymbol::Ram(e, _) => {
                get_expresson_value_or_dereference_value(e, e.get_entry_point()).and_then(|addr| self.map.get_mut(&addr))
            }
        }
    }

    pub fn resolve_exp(&self, e:&Expression) -> Option<&VariableDefinition> {
        get_expresson_value_or_dereference_value(e, e.get_entry_point()).and_then(|addr| self.map.get(&addr))
    }

    pub fn resolve_destination(&self, dst:&DestinationKind) -> Option<&VariableDefinition> {
        match dst {
            DestinationKind::Symbolic(e) => self.resolve_exp(e),
            DestinationKind::Concrete(address) => self.map.get(&address),
            DestinationKind::Virtual(_,_) => None,
        }
    }
}

fn get_expresson_value_or_dereference_value(e:&Expression, pos:usize) -> Option<Address> {
    match &e[pos] {
        ExpressionOp::Value(v) => Some(Address(*v)),
        ExpressionOp::Dereference(pos) => get_expresson_value_or_dereference_value(e, *pos),
        _ => None,
    }
}