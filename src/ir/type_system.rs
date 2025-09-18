use nodit::{Interval, NoditMap};


#[derive(Clone, Debug)]
pub enum VariableType{
    Byte,
    Char,
    S16,
    U16,
    S32,
    U32,
    F32,
    F64,
    Pointer(Box<VariableType>),
    /// Offset into [`KnownStructs::storage`]
    Struct(usize)
}

impl Default for VariableType {
    fn default() -> Self {
        Self::S32
    }
}

pub struct Struct {
    fields: NoditMap<Interval<usize>, usize, VariableType>
}

pub struct KnownStructs{
    storage:Vec<Struct>
}