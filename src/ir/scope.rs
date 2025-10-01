use std::collections::HashMap;

use super::{
    basic_block::BlockSlot, control_flow_graph::SingleEntrySingleExit, expression::VariableSymbol,
    program_tree_structure::ProgramTreeStructure, type_system::VariableType,
};

#[derive(Clone, Debug)]
pub struct VariableDefinition {
    pub kind: VariableType,
    pub name: String,
    pub variable: VariableSymbol,
}

impl VariableDefinition {
    pub fn new(kind: VariableType, name: String, value: VariableSymbol) -> Self {
        Self {
            kind,
            name,
            variable: value,
        }
    }
}

pub type SymbolMap = HashMap<VariableSymbol, VariableDefinition>;

pub struct Scope {
    /// Map of SESEs to what variables are defined at that level
    map: HashMap<SingleEntrySingleExit<BlockSlot>, SymbolMap>,
    pub parents: HashMap<SingleEntrySingleExit<BlockSlot>, Vec<SingleEntrySingleExit<BlockSlot>>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            parents: HashMap::new(),
        }
    }

    pub fn fill_parents(
        &mut self,
        pts: &ProgramTreeStructure,
        root: SingleEntrySingleExit<BlockSlot>,
    ) {
        if let Some(children) = pts.get_children(root) {
            for child in children {
                self.parents.entry(*child).or_default().push(root);
                self.fill_parents(pts, *child);
            }
        }
    }

    pub fn insert(
        &mut self,
        key: SingleEntrySingleExit<BlockSlot>,
        value: SymbolMap,
    ) -> Option<SymbolMap> {
        self.map.insert(key, value)
    }

    pub fn add(
        &mut self,
        section: SingleEntrySingleExit<BlockSlot>,
        key: VariableSymbol,
        value: VariableDefinition,
    ) -> Option<VariableDefinition> {
        self.map.entry(section).or_default().insert(key, value)
    }

    pub fn get(&self, section: SingleEntrySingleExit<BlockSlot>) -> Option<&SymbolMap> {
        self.map.get(&section)
    }

    pub fn find_owning_section(
        &self,
        symbol: &VariableSymbol,
    ) -> Option<SingleEntrySingleExit<BlockSlot>> {
        for (child, parents) in &self.parents {
            if self.map.get(child).and_then(|s| s.get(symbol)).is_some() {
                return Some(*child);
            }
            for parent in parents {
                if self.map.get(parent).and_then(|s| s.get(symbol)).is_some() {
                    return Some(*parent);
                }
            }
        }
        None
    }

    pub fn get_symbol_recursive(
        &self,
        section: SingleEntrySingleExit<BlockSlot>,
        key: &VariableSymbol,
    ) -> Option<&VariableDefinition> {
        if let Some(scope) = self.map.get(&section) {
            if let Some(def) = scope.get(key) {
                return Some(def);
            }
        }
        if let Some(parents) = self.parents.get(&section) {
            for parent in parents {
                if let Some(def) = self.get_symbol_recursive(*parent, key) {
                    return Some(def);
                }
            }
        }
        None
    }

    pub fn get_symbol_mut(
        &mut self,
        section: SingleEntrySingleExit<BlockSlot>,
        key: &VariableSymbol,
    ) -> Option<&mut VariableDefinition> {
        if let Some(scope) = self.map.get_mut(&section) {
            if let Some(def) = scope.get_mut(key) {
                return Some(def);
            }
        }
        None
    }

    pub fn pretty_print(&self, pts: &ProgramTreeStructure) -> String {
        use std::io::Write;
        let mut buffer = std::io::Cursor::new(Vec::new());
        pts.pretty_print(&mut buffer, &|buffer, depth, sese| {
            let mut has_written = false;
            if let Some(vars) = self.get(sese) {
                for (key, value) in vars.iter() {
                    if !has_written {
                        buffer.write_fmt(format_args!("\n"))?;
                        has_written = true;
                    }
                    write!(
                        buffer,
                        "{}{key} = {}\n",
                        " ".repeat(depth as usize),
                        value.name
                    )?;
                    // if idx + 1 < vars.len() {
                    //     buffer.write_char('\n')?;
                    // }
                }
            }
            Ok(has_written)
        })
        .expect("Unable to generate pretty string for pts.");
        String::from_utf8(buffer.into_inner()).unwrap()
    }
}
