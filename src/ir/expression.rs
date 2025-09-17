use std::{borrow::Cow, collections::HashMap, ops::Index};

use iced_x86::Register;


use super::{Address, BlockStorage};


/// SLEIGH defines serveral "Memory Spaces", though usually it uses Memory, Register, and Unique.
/// We too define several memory spaces but in a different way. Eventually I'd like to express overlapping memory spaces
/// E.g. AL is byte of RAX. Or this call is stored on stack at Ram 0xXXX
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum VariableSymbol{
    Register(Register),
    CallResult{
        call_from:Address,
        call_to:Expression,
    },
    Ram(Expression),
}

impl std::fmt::Debug for VariableSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(arg0) => f.write_fmt(format_args!("?{arg0:?}")),
            Self::CallResult{call_to,..} => f.write_fmt(format_args!("call_{call_to:?}_result")),
            Self::Ram(arg0) => f.write_fmt(format_args!("[{arg0:?}]")),
        }
    }
}

impl std::fmt::Display for VariableSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(arg0) => f.write_fmt(format_args!("?{arg0:?}")),
            Self::CallResult{call_to,..} => f.write_fmt(format_args!("call_{call_to}_result")),
            Self::Ram(arg0) => f.write_fmt(format_args!("{arg0}")),
        }
    }
}

impl VariableSymbol {
    pub fn is_above_stack_frame(&self) -> Option<bool> {
        if let Self::Ram(r) = self {
            let result = r.assume(Register::ESP, 0);
            if let Some(ExpressionOp::Value(v)) = result.last_op() {
                return Some(*v > 0)
            } 
        } 
        None
    }

    pub fn get_memory_address_or_null(&self) -> u64 {
        match self {
            VariableSymbol::Register(_) |
            VariableSymbol::CallResult{..} => 0,
            VariableSymbol::Ram(expression) => expression.get_memory_address_or_null(),
        }
    }
}

impl Index<OpIdx> for Expression {
    type Output = ExpressionOp;

    fn index(&self, index: OpIdx) -> &Self::Output {
        self.0.index(index)
    }
}


/// Basic building block of any code. This expression can be symbolic
/// (one of the [`ExpressionOp`] variants is a [`VariableSymbol`]).
/// Stores first operation to do as the last element of the vector - 
/// `Expression::get_entry_point()` returns the index to the last element.
/// 
/// Implements smart in-place patching routines which perform common algebra operations
/// as soon as those operations are added - e.g. stores `?ESP+8` instead of `?ESP+4+4`
/// This approach checks only last operation - `?ESP+4+?EBP+4` will not be simplified,
/// though that can be a nice addition.
/// 
/// `std::fmt::Display` trait prints the expression recursively, e.g. `[?ESP+4]`
/// while `std::fmt::Debug` trait prints the list of [`ExpressionOp`], e.g. `[Variable(ESP), Value(4), Add(0, 1), Dereference(2)]`
#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct Expression(Vec<ExpressionOp>);
type OpIdx = usize;

impl From<i64> for Expression {
    fn from(value: i64) -> Self {
        Self(vec![ExpressionOp::Value(value)])
    }
}

impl From<Address> for Expression {
    fn from(value: Address) -> Self {
        Self(vec![ExpressionOp::Value(value.0 as i64)])
    }
}

impl From<VariableSymbol> for Expression {
    fn from(variable: VariableSymbol) -> Self {
        Self(vec![ExpressionOp::Variable(variable)])
    }
}

impl From<Vec<ExpressionOp>> for Expression {
    fn from(value: Vec<ExpressionOp>) -> Self {
        Self(value)
    }
}

impl From<Register> for Expression {
    fn from(register: Register) -> Self {
        Self(vec![ExpressionOp::DestinationRegister(register)])
    }
}
impl From<ExpressionOp> for Expression {
    fn from(op: ExpressionOp) -> Self {
        Self(vec![op])
    }
}


fn remap_operands<T>(src:&[ExpressionOp], pos:OpIdx, vec:&mut Vec<ExpressionOp>, mut map:T)
where T:Copy + FnMut(&ExpressionOp, &[ExpressionOp]) -> ExpressionOp {
    match &src[pos] {
        e @ ExpressionOp::Variable(_) |
        e @ ExpressionOp::DestinationRegister(_) |
        e @ ExpressionOp::Value(_) => vec.push(map(e, vec)),
        e @ ExpressionOp::Dereference(p) => {
            remap_operands(src, *p, vec, map);
            vec.push(ExpressionOp::Dereference(vec.len() - 1));
        },
        ExpressionOp::Assign(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Assign(l, r));
        },
        ExpressionOp::Multiequals(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Multiequals(l, r));
        },
        ExpressionOp::Add(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Add(l, r));
        },
        ExpressionOp::Sub(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Sub(l, r));
        },
        ExpressionOp::Multiply(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Multiply(l, r));
        },
        ExpressionOp::LessOrEquals(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::LessOrEquals(l, r));
        },
        ExpressionOp::Less(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Less(l, r));
        },
        ExpressionOp::GreaterOrEquals(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::GreaterOrEquals(l, r));
        },
        ExpressionOp::Greater(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Greater(l, r));
        },
        ExpressionOp::Equals(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Equals(l, r));
        },
        ExpressionOp::NotEquals(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::NotEquals(l, r));
        },
        ExpressionOp::BitShiftRight(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::BitShiftRight(l, r));
        },
        ExpressionOp::BitShiftLeft(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::BitShiftLeft(l, r));
        },
        ExpressionOp::And(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::And(l, r));
        },
    }
}

impl Expression {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn last_op(&self) -> Option<&ExpressionOp> {
        self.0.last()
    }

    pub fn get(&self, idx:OpIdx) -> &ExpressionOp {
        &self.0[idx]
    }

    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, ExpressionOp> {
        self.0.iter() 
    }

    pub fn get_sub_expression(&self, idx:OpIdx) -> Expression {
        let mut result = Expression::new();
        remap_operands(&self.0, idx, &mut result.0, 
            |e, _| e.clone());
        result
    }

    pub fn multiply(&mut self, other:&Self) {
        if other.0.len() == 1 {
            if let ExpressionOp::Value(v) = other.0[0] {
                self.multiply_value(v);
                return
            }
        }
        let left = self.get_entry_point();
        self.copy_other_to_end(&other.0);
        let right = self.get_entry_point();
        self.0.push(ExpressionOp::Multiply(left, right))
    }

    pub fn add(&mut self, other:&Self) {
        if other.0.len() == 1 {
            if let ExpressionOp::Value(v) = other.0[0] {
                self.add_value(v);
                return
            }
        }
        let left = self.get_entry_point();
        self.copy_other_to_end(&other.0);
        let right = self.get_entry_point();
        self.0.push(ExpressionOp::Add(left, right))
    }

    pub fn sub(&mut self, other:&Self) {
        if other.0.len() == 1 {
            if let ExpressionOp::Value(v) = other.0[0] {
                self.sub_value(v);
                return
            }
        }
        let left = self.get_entry_point();
        self.copy_other_to_end(&other.0);
        let right = self.get_entry_point();
        self.0.push(ExpressionOp::Sub(left, right))
    }

    pub fn add_value(&mut self, value:i64) {
        let expr = self.get_entry_point();
        self.add_value_at(expr, value);
    }

    /// Returns how many instructions have been added to `self.0`
    fn add_value_at(&mut self, expr:OpIdx, value:i64) -> usize {
        use ExpressionOp::*;
        if value == 0 {
            return 0;
        }
        match self.0[expr] {
            Add(l, r) => {
                if let Value(v) = &mut self.0[r] {
                    *v += value;
                    if *v < 0 {
                        *v = -*v;
                        self.0[expr] = Sub(l, r);
                    }
                } else if let Value(v) = &mut self.0[l] {
                    *v += value;
                }
                0
            },
            Sub(l, r) => {
                if let Value(v) = &mut self.0[r] {
                    *v -= value;
                    if *v < 0 {
                        *v = -*v;
                        self.0[expr] = Add(l, r);
                    }
                } else if let Value(v) = &mut self.0[l] {
                    *v += value;
                }
                0
            },
            Value(v) => {
                self.0[expr] = Value(v + value);
                0
            }
            _ => {
                self.0.push(ExpressionOp::Value(value));
                let pos = self.get_entry_point();
                self.0.push(ExpressionOp::Add(expr, pos));
                2
            }
        }
    }

    pub fn sub_value(&mut self, value:i64) {
        let expr = self.get_entry_point();
        self.sub_value_at(expr, value, false);
    }

    fn sub_value_at(&mut self, expr:OpIdx, value:i64, is_invert:bool) -> usize {
        use ExpressionOp::*;
        if value == 0 {
            return 0;
        }
        match self.0[expr] {
            Add(l, r) => {
                if let Value(v) = &mut self.0[r] {
                    *v -= value;
                    if *v < 0 {
                        *v = -*v;
                        self.0[expr] = Sub(l, r);
                    }
                } else if let Value(v) = &mut self.0[l] {
                    *v -= value;
                }
                0
            },
            Sub(l, r) => {
                if let Value(v) = &mut self.0[r] {
                    *v += value;
                    if *v < 0 {
                        *v = -*v;
                        self.0[expr] = Add(l, r);
                    }
                } else if let Value(v) = &mut self.0[l] {
                    *v -= value;
                }
                0
            },
            Value(v) => {
                self.0[expr] = Value(v - value);
                0
            }
            _ => {
                self.0.push(ExpressionOp::Value(value));
                let pos = self.get_entry_point();
                if is_invert {
                    self.0.push(ExpressionOp::Sub(pos, expr));
                } else {
                    self.0.push(ExpressionOp::Sub(expr, pos));
                }
                2
            }
        }
    }

    pub fn multiply_value(&mut self, value:i64) {
        let expr = self.get_entry_point();
        self.multiply_value_at(expr, value);
    }

    fn multiply_value_at(&mut self, expr:OpIdx, value:i64) -> usize{
        use ExpressionOp::*;
        if value == 1 {
            return 0;
        }
        match self.0[expr] {
            Multiply(l, r) => {
                if let Value(v) = &mut self.0[r] {
                    *v *= value;
                } else if let Value(v) = &mut self.0[l] {
                    *v *= value;
                }
                0
            },
            Value(v) => {
                self.0[expr] = Value(v * value);
                0
            }
            _ => {
                self.0.push(ExpressionOp::Value(value));
                let pos = self.get_entry_point();
                self.0.push(ExpressionOp::Multiply(expr, pos));
                2
            }
        }
    }

    pub fn dereference(&mut self) {
        let val = self.get_entry_point();
        self.0.push(ExpressionOp::Dereference(val));
    }

    pub fn bit_shift_right(&mut self, value:i64) {
        let val = self.get_entry_point();
        self.0.push(ExpressionOp::Value(value));
        self.0.push(ExpressionOp::BitShiftRight(val, val+1));
    }

    pub fn bit_shift_left(&mut self, value:i64) {
        let val = self.get_entry_point();
        self.0.push(ExpressionOp::Value(value));
        self.0.push(ExpressionOp::BitShiftLeft(val, val+1));
    }

    pub fn and(&mut self, value:i64) {
        let val = self.get_entry_point();
        self.0.push(ExpressionOp::Value(value));
        self.0.push(ExpressionOp::And(val, val+1));
    }

    pub fn cancel_dereference(&mut self) {
        assert!(matches!(self.0.pop(), Some(ExpressionOp::Dereference(_))));
    }

    pub fn assign(&mut self, other:&Self) {
        let left = self.get_entry_point();
        self.copy_other_to_end(&other.0);
        let right = self.get_entry_point();
        self.0.push(ExpressionOp::Assign(left, right))
    }

    pub fn multiequals(&mut self, other:&Self) {
        let left = self.get_entry_point();
        self.copy_other_to_end(&other.0);
        let right = self.get_entry_point();
        self.0.push(ExpressionOp::Multiequals(left, right))
    }

    pub fn assign_value(&mut self, value:i64) {
        let left = self.get_entry_point();
        self.0.push(ExpressionOp::Value(value));
        self.0.push(ExpressionOp::Assign(left, left+1))
    }

    pub fn check_equals_value(&mut self, value:i64) {
        self.add_value(value);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r) => {
                self.0[left] = ExpressionOp::Equals(l, r)
            },
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::Equals(left, left+1))
            }
        }
    }

    pub fn check_not_equals_value(&mut self, value:i64) {
        self.add_value(value);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r) => {
                self.0[left] = ExpressionOp::NotEquals(l, r)
            },
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::NotEquals(left, left+1))
            }
        }
    }

    pub fn check_less_value(&mut self, value:i64) {
        self.add_value(value);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r) => {
                self.0[left] = ExpressionOp::GreaterOrEquals(l, r)
            },
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::Less(left, left+1))
            }
        }
    }

    pub fn check_greater_value(&mut self, value:i64) {
        self.add_value(value);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r) => {
                self.0[left] = ExpressionOp::LessOrEquals(l, r)
            },
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::Greater(left, left+1))
            }
        }
    }

    pub fn check_less_or_equals_value(&mut self, value:i64) {
        self.add_value(value);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r) => {
                self.0[left] = ExpressionOp::Greater(l, r)
            },
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::LessOrEquals(left, left+1))
            }
        }
    }

    pub fn check_greater_or_equals_value(&mut self, value:i64) {
        self.add_value(value);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r) => {
                self.0[left] = ExpressionOp::Less(l, r)
            },
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::GreaterOrEquals(left, left+1))
            }
        }
    }

    pub fn not(&mut self) {
        let pos = self.get_entry_point();
        match self.0[pos] {
            ExpressionOp::Equals(l, r) => {
                self.0[pos] = ExpressionOp::NotEquals(l, r);
            },
            ExpressionOp::Greater(l, r) => {
                self.0[pos] = ExpressionOp::LessOrEquals(l, r);
            },
            ExpressionOp::GreaterOrEquals(l, r) => {
                self.0[pos] = ExpressionOp::Less(l, r);
            },
            ExpressionOp::Less(l, r) => {
                self.0[pos] = ExpressionOp::GreaterOrEquals(l, r);
            },
            ExpressionOp::LessOrEquals(l, r) => {
                self.0[pos] = ExpressionOp::Greater(l, r);
            }
            ExpressionOp::NotEquals(l, r) => {
                self.0[pos] = ExpressionOp::Equals(l, r);
            }

            _ => {todo!("Invert {:?}", self.0[pos])},
        }
    }




    fn recursive_print(&self, idx:OpIdx, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0[idx] {
            ExpressionOp::Variable(variable) => f.write_fmt(format_args!("{variable}")),
            ExpressionOp::DestinationRegister(register) => f.write_fmt(format_args!("{register:?}")),
            ExpressionOp::Value(v) => if *v > 0xffff { f.write_fmt(format_args!("0x{v:x}")) } else { f.write_fmt(format_args!("{v}")) },
            ExpressionOp::Dereference(idx) => {f.write_str("[")?; self.recursive_print(*idx, f)?; f.write_str("]")},
            ExpressionOp::Assign(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" := ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::Multiequals(l_idx, r_idx) => {f.write_str("(")?; self.recursive_print(*l_idx, f)?; f.write_str(") OR (")?; self.recursive_print(*r_idx, f)?; f.write_str(")")},
            ExpressionOp::Add(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" + ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::Sub(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" - ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::Multiply(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" * ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::LessOrEquals(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" <= ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::Less(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" < ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::GreaterOrEquals(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" >= ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::Greater(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" > ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::Equals(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" == ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::NotEquals(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" != ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::BitShiftLeft(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" << ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::BitShiftRight(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" >> ")?; self.recursive_print(*r_idx, f)},
            ExpressionOp::And(l_idx, r_idx) => {self.recursive_print(*l_idx, f)?; f.write_str(" & ")?; self.recursive_print(*r_idx, f)},
        }
    }

    /// Given a set of tail instructions after substituting a variable - append this tail to
    /// `self.0` while patching pointers to point to the right new values
    fn copy_other(&mut self, new_pos:usize, ignore_under:usize, other:&[ExpressionOp]) {
        use ExpressionOp::*;
        // helper function to calculate new pointer position
        // if the pointer is between `[0; ignore_under]` - it's unchanged, because the expression hasn't been touched there.
        // otherwise the pointer is past the replaced variable and needs to be changed. 
        let s = |p:&usize| if *p >= ignore_under {  p - ignore_under + new_pos } else { *p };
        for op in other {
            self.0.push(match op {
                Dereference(p) => Dereference(s(p)),
                Assign(l, r) => Assign(s(l), s(r)),
                Multiequals(l, r) => Multiequals(s(l), s(r)),
                Add(l, r) => Add(s(l), s(r)),
                Sub(l, r) => Sub(s(l), s(r)),
                Multiply(l, r) => Multiply(s(l), s(r)),
                LessOrEquals(l, r) => LessOrEquals(s(l), s(r)),
                Less(l, r) => Less(s(l), s(r)),
                GreaterOrEquals(l, r) => GreaterOrEquals(s(l), s(r)),
                Greater(l, r) => Greater(s(l), s(r)),
                Equals(l, r) => Equals(s(l), s(r)),
                NotEquals(l, r) => NotEquals(s(l), s(r)),
                BitShiftLeft(l, r) => BitShiftLeft(s(l), s(r)),
                BitShiftRight(l, r) => BitShiftRight(s(l), s(r)),
                And(l, r) => And(s(l), s(r)),
                a => a.clone()
            })
        }
    }

    pub fn top_kind(&self) -> &ExpressionOp {
        self.0.last().unwrap()
    }

    fn copy_other_to_end(&mut self, other:&[ExpressionOp]) {
        self.copy_other(self.0.len(), 0, other);
    }

    pub fn get_destination_register(&self) -> Register {
        match &self.0[0] {
            ExpressionOp::DestinationRegister(r) => *r,
            _ => panic!("Expected a register, got {self:?}")
        }
    }

    pub fn iter_vars<'a>(&'a self) -> impl Iterator<Item = &VariableSymbol> + 'a {
        self.0.iter().filter_map(|p| if let ExpressionOp::Variable(v) = p { Some(v) } else { None })
    }

    pub fn get_value(&self) -> i64 {
        match &self.0[0] {
            ExpressionOp::Value(v) => *v,
            _ => panic!("Exptected a value, got {self:?}")
        }
    }

    pub fn get_memory_address_or_null(&self) -> u64 {
        match &self.0[0] {
            ExpressionOp::Dereference(v) => {
                match &self.0[*v] {
                    ExpressionOp::Value(v) => *v as u64,
                    _ => 0,
                }
            },
            ExpressionOp::Value(v) => {
                *v as u64
            }
            _ => 0,
        }
    }


    pub fn is_symbolic(&self) -> bool {
        self.0.iter().find(|p| matches!(p, ExpressionOp::Variable(_))).is_some()
    }


    /// Returns how many new instructions have been added
    pub fn replace_variable_with_expression(&mut self, var_index:OpIdx, expr:&Expression) -> i32 {
        assert!(matches!(self.0[var_index], ExpressionOp::Variable(_)));
        if expr == self { 
            return 0
        }
        // split the instruction vector
        let shift =  var_index + (expr.0.len() - 1);
        let remainder = self.0.split_off(var_index);
        // copy new instructions instead of the variable
        self.copy_other_to_end(&expr.0);
        // fix old instruction pointers
        if true { // is flattening expressions or not?
            let saved = self.apply(shift as i32, var_index, &remainder[1..]);
            expr.0.len() as i32 - 1 + saved
        } else {
            // If we just do self.copy_other we will crate correct expression, but it'll be expanded - with lots of + and - ops that can be simplified.
            self.copy_other(shift, var_index, &remainder[1..]);
            (expr.0.len() - 1) as i32
        }
    }

    pub fn replace_variable_with<'r, F>(&mut self, replace:F) where F:Fn(&VariableSymbol) -> Option<(Cow<'r, Expression>)> {
        // let mut extended_by = 0;

        let mut pos = 0;
        while pos < self.0.len() {
            if let ExpressionOp::Variable(v) = &self.0[pos] {
                if let Some(expr) = replace(v) {
                    let r = self.replace_variable_with_expression(pos, &expr);
                    if r > 0 { // it is possible we remove more instructions than add in case of nop-algebra. 
                        // in that case position of current variable stays the same. 
                        pos = (pos as i32 + r) as usize;
                    }
                }
            }
            pos += 1;
        }
    }

    pub fn assume(&self, reg:Register, value:i64) -> Expression {
        let mut result = self.clone();
        result.replace_variable_with(|v| {
                match v {
                    VariableSymbol::Register(r) => {
                        if *r == reg {
                            Some(Cow::Owned(Expression::from(value)))
                        } else {
                            None
                        }
                    },
                    _ => None,
                }
            });
        result
    }

    fn decrement_offsets(&mut self, from:usize) {
        for op in &mut self.0[from..] {
            match op {
                ExpressionOp::Dereference(l) => {
                    if *l >= from {
                        *l -= 1
                    }
                },
                ExpressionOp::Assign(l, r) |
                ExpressionOp::Multiequals(l, r) |
                ExpressionOp::Add(l, r) |
                ExpressionOp::Sub(l, r) |
                ExpressionOp::Multiply(l, r) |
                ExpressionOp::LessOrEquals(l, r) |
                ExpressionOp::Less(l, r) |
                ExpressionOp::GreaterOrEquals(l, r) |
                ExpressionOp::Greater(l, r) |
                ExpressionOp::Equals(l, r) |
                ExpressionOp::BitShiftLeft(l, r) |
                ExpressionOp::BitShiftRight(l, r) |
                ExpressionOp::And(l, r) |
                ExpressionOp::NotEquals(l, r) => { 
                    if *l >= from {
                        *l -= 1;
                     }
                    if *r >= from { *r -= 1}
                },
                _ => ()
            }
        }
    }

    fn replace_offsets(&mut self, from:usize, original:usize, new:usize) {
        for op in &mut self.0[from..] {
            match op {
                ExpressionOp::Dereference(l) => {
                    if *l == original {
                        *l = new
                    }
                },
                ExpressionOp::Assign(l, r) |
                ExpressionOp::Multiequals(l, r) |
                ExpressionOp::Add(l, r) |
                ExpressionOp::Sub(l, r) |
                ExpressionOp::Multiply(l, r) |
                ExpressionOp::LessOrEquals(l, r) |
                ExpressionOp::Less(l, r) |
                ExpressionOp::GreaterOrEquals(l, r) |
                ExpressionOp::Greater(l, r) |
                ExpressionOp::Equals(l, r) |
                ExpressionOp::BitShiftLeft(l, r) |
                ExpressionOp::BitShiftRight(l, r) |
                ExpressionOp::And(l, r) |
                ExpressionOp::NotEquals(l, r) => { 
                    if *l == original {
                        *l = new;
                    }
                    if *r == original {
                        *r = new;
                    }
                },
                _ => ()
            }
        }
    }

    fn remove_nop_algebra(&mut self, mut idx:OpIdx) -> i32 {
        let mut left_over = None;
        match &self.0[idx] {
            &ExpressionOp::Add(l, r) |
            &ExpressionOp::Sub(l, r) => {
                if ExpressionOp::Value(0) == self.0[l] {
                    self.0.remove(l);
                    left_over = Some(r);
                    self.decrement_offsets(l);
                    if l < idx {
                        idx -= 1;
                    }
                } else if ExpressionOp::Value(0) == self.0[r] {
                    self.0.remove(r);
                    left_over = Some(l);
                    self.decrement_offsets(r);
                    if r < idx {
                        idx -= 1;
                    }
                }
            },
            &ExpressionOp::Multiply(l, r) => {
                if ExpressionOp::Value(1) == self.0[l] {
                    self.0.remove(l);
                    left_over = Some(r);
                    self.decrement_offsets(l);
                    if l < idx {
                        idx -= 1;
                    }
                } else if ExpressionOp::Value(1) == self.0[r] {
                    self.0.remove(r);
                    left_over = Some(l);
                    self.decrement_offsets(r);
                    if r < idx {
                        idx -= 1;
                    }
                }
            },
            _ => ()
        }

        if let Some(left_over) = left_over {
            self.0.remove(idx);
            self.decrement_offsets(idx);
            self.replace_offsets(idx, idx-1, left_over);
            -2
        } else {
            0
        }

    }

    /// Given the arguments, of an old add/mul/sub instruction, this function modifies the expression
    /// such that immediate values are added/multiplied/subtracted before adding new add/mul/sub instructions.
    /// 
    /// Returns how many instructions have been added to the list. This value is likely to be `-2` (removed 2 instructions) or `0` (didn't remove any)
    fn patch(&mut self, l:usize, r:usize, new_pos:i32, ignore_under:usize, patch_kind:PatchKind) -> i32 {
        /// helper function to calculate new pointer position
        /// if the pointer is between `[0; ignore_under]` - it's unchanged, because the expression hasn't been touched there.
        /// otherwise the pointer is past the replaced variable and needs to be changed. 
        fn s(p:usize, ignore_under:usize, new_pos:i32) -> usize {
            if p >= ignore_under {  ((p - ignore_under) as i32 + new_pos) as usize } else { p }
        }
        let mut patched_l = s(l, ignore_under, new_pos);
        let mut patched_r = s(r, ignore_under, new_pos);
        if let ExpressionOp::Value(left) = self.0[patched_l] {
            // assert_eq!(patched_l, self.entry());
            self.0.remove(patched_l);
            if patched_l < patched_r {
                patched_r -= 1;
            }
            self.decrement_offsets(patched_l);
            let mut r = match patch_kind {
                // values will be attempted to be added and if they can't be - new instructions will be pushed on the stack
                // the count of those instructions will be returned, so we add them to new_pos
                PatchKind::Add => self.add_value_at(patched_r, left),
                PatchKind::Sub => self.sub_value_at(patched_r, left, true),
                PatchKind::Mul => self.multiply_value_at(patched_r, left),
            } as i32 - 2; // overall we removed one instruction from the list, and saved space by not adding a value, therefore, remove 2 from new_pos
            r += self.remove_nop_algebra(patched_r);
            r
        } else if let ExpressionOp::Value(right) = self.0[patched_r] {
            // assert_eq!(patched_r, self.entry());
            self.0.remove(patched_r);
            if patched_r < patched_l {
                patched_l -= 1;
            }
            self.decrement_offsets(patched_r);
            let mut r = match patch_kind {
                PatchKind::Add => self.add_value_at(patched_l, right),
                PatchKind::Sub => self.sub_value_at(patched_l, right, false),
                PatchKind::Mul => self.multiply_value_at(patched_l, right),
            }as i32 - 2;
            r += self.remove_nop_algebra(patched_l);
            r
        } else {
            self.0.push(match patch_kind {
                PatchKind::Add => ExpressionOp::Add(patched_l, patched_r),
                PatchKind::Sub => ExpressionOp::Sub(patched_l, patched_r),
                PatchKind::Mul => ExpressionOp::Multiply(patched_l, patched_r),
            });
            0
        }
    }

    fn apply(&mut self, mut new_pos:i32, ignore_under:usize, other:&[ExpressionOp]) -> i32  {
        use ExpressionOp::*;
        fn s(p:&usize, ignore_under:usize, new_pos:i32) -> usize {
            if *p >= ignore_under {  ((*p - ignore_under) as i32 + new_pos) as usize } else { *p }
        }
        let mut total_saved= 0;
        for op in other {
            match op {
                Dereference(p) => self.0.push(Dereference(s(p, ignore_under, new_pos))),
                Assign(l, r) => self.0.push(Assign(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                Multiequals(l, r) => self.0.push(Multiequals(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                Add(l, r) => { 
                    let saved = self.patch(*l, *r, new_pos, ignore_under, PatchKind::Add); 
                    total_saved += saved;
                    new_pos = new_pos as i32 + saved;
                },
                Sub(l, r) => { 
                    let saved = self.patch(*l, *r, new_pos, ignore_under, PatchKind::Sub); 
                    total_saved += saved;
                    new_pos = new_pos as i32 + saved;
                }
                Multiply(l, r) => { 
                    let saved = self.patch(*l, *r, new_pos, ignore_under, PatchKind::Mul); 
                    total_saved += saved;
                    new_pos = new_pos as i32 + saved;
                }
                LessOrEquals(l, r) => self.0.push(LessOrEquals(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                Less(l, r) => self.0.push(Less(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                GreaterOrEquals(l, r) => self.0.push(GreaterOrEquals(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                Greater(l, r) => self.0.push(Greater(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                Equals(l, r) => self.0.push(Equals(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                NotEquals(l, r) => self.0.push(NotEquals(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                BitShiftLeft(l, r) => self.0.push(BitShiftLeft(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                BitShiftRight(l, r) => self.0.push(BitShiftRight(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                And(l, r) => self.0.push(And(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                a => self.0.push(a.clone())
            }
        }
        total_saved
    }

    pub fn get_entry_point(&self) -> OpIdx {
        return self.0.len() - 1
    }
}

#[derive(Debug)]
enum PatchKind{
    Add,
    Sub,
    Mul
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ExpressionOp{
    Variable(VariableSymbol),
    DestinationRegister(Register), 
    Value(i64),
    Dereference(OpIdx), 
    Assign(OpIdx, OpIdx),
    Multiequals(OpIdx, OpIdx),

    Add(OpIdx, OpIdx),
    Sub(OpIdx, OpIdx),
    Multiply(OpIdx, OpIdx), 
    LessOrEquals(OpIdx, OpIdx),
    Less(OpIdx, OpIdx),
    GreaterOrEquals(OpIdx, OpIdx),
    Greater(OpIdx, OpIdx),
    Equals(OpIdx, OpIdx),
    NotEquals(OpIdx, OpIdx),
    BitShiftRight(OpIdx, OpIdx),
    BitShiftLeft(OpIdx, OpIdx),
    And(OpIdx, OpIdx),
}

impl ExpressionOp {
    fn var_reg(reg:Register) -> Self {
        Self::Variable(VariableSymbol::Register(reg))
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.len() > 0 {
            self.recursive_print(self.get_entry_point(), f)
        } else {
            f.write_str("NOP")
        }
    }
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.0))
    }
}

mod test {
    use iced_x86::Register;

    use crate::ir::{expression::{ExpressionOp, VariableSymbol}, Expression};

    #[inline]
    fn var_reg(r:Register) -> ExpressionOp {
        ExpressionOp::Variable(VariableSymbol::Register(r))
    }

    #[test]
    fn test_multiply() {
        let mut e = Expression::from(1);
        e.add(&Expression::from(VariableSymbol::Register(Register::EAX)));
        e.multiply_value(4);
        assert_eq!(e.0, vec![
            ExpressionOp::Value(1),
            var_reg(Register::EAX),
            ExpressionOp::Add(0, 1),
            ExpressionOp::Value(4),
            ExpressionOp::Multiply(2, 3),
            ]);
    }

    #[test]
    fn test_variable_substitution() {
        use ExpressionOp::{Value, Variable as VarOp, Add, Multiply};
        let mut e = Expression::from(1);
        e.add(&Expression::from(VariableSymbol::Register(Register::EAX)));
        e.multiply_value(4); // e = (1 + ?EAX) * 4

        let mut sub = Expression::from(1);
        sub.add(&Expression::from(VariableSymbol::Register(Register::ESP)));
        sub.multiply_value(2); // sub = (1+?ESP) * 2

        e.replace_variable_with_expression(1, &sub);
        
        // This block is correct if we are NOT using expression flattening
        // assert_eq!(e.0, vec![
        //     // (1 + (1 + ?ESP) * 2 ) * 4
        //     Value(1),
        //     Value(1),
        //     VarOp(Variable::Register(Register::ESP)),
        //     Add(1, 2),
        //     Value(2),
        //     Multiply(3, 4),
        //     Add(0, 5),
        //     Value(4),
        //     Multiply(6, 7)
        // ]);

        // this block is correct if we are using expression flatening
        assert_eq!(e.0, vec![
            // (1 + (1 + ?ESP) * 2 ) * 4
            Value(1),
            VarOp(VariableSymbol::Register(Register::ESP)),
            Add(0, 1),
            Value(2),
            Multiply(2, 3),
            Value(1),
            Add(4, 5),
            Value(4),
            Multiply(6, 7)
        ]);
    }

    #[test]
    fn test_replace() {
        use ExpressionOp::{Value, Add, Dereference, Sub, Variable as VarOp};
        let mut first = Expression::from(VariableSymbol::Register(Register::ESP));
        first.add_value(180);
        first.dereference();
        // first = [?ESP + 180]

        let mut second = Expression::from(VariableSymbol::Register(Register::ESP));
        second.sub_value(12);
        second.dereference();
        // second = [?ESP - 12]

        second.replace_variable_with_expression(0, &first);
        // second = [[?ESP - 12] + 180]
        assert_eq!(second.0, vec![
            VarOp(VariableSymbol::Register(Register::ESP)), Value(180), Add(0, 1), Dereference(2), Value(12), Sub(3, 4), Dereference(5)
        ])
    }

    #[test]
    fn test_replace_ptr_to_var() {
        use ExpressionOp::{Value, Dereference, Assign, Add, Variable as VarOp};
        let mut e = Expression::new();
        e.0 = vec![
            Value(6721424), Dereference(0), ExpressionOp::Variable(VariableSymbol::Register(Register::EAX)), Assign(1, 2)
        ];
        // e = [668f90] := ?EAX

        let mut eax = Expression::new();
        eax.0 = vec![
            VarOp(VariableSymbol::Register(Register::EBP)), Value(100), Add(0, 1), Dereference(2)
        ]; // eax = [?EBP + 100]
        e.replace_variable_with_expression(2, &eax);
        assert_eq!(e.0, vec![
            Value(6721424), Dereference(0), VarOp(VariableSymbol::Register(Register::EBP)), Value(100), Add(2, 3), Dereference(4), Assign(1, 5)
        ])
    }

    #[test]
    fn test_replace_complex() {
        use ExpressionOp::{Value, Dereference, Assign, Add, Variable as VarOp};
        let mut ptr = Expression::new();
        ptr.0 = vec![
            VarOp(VariableSymbol::Register(Register::EAX)), Value(20), Add(0, 1), Dereference(2)
        ];


        // e = [?EAX + 20] := ?data@[?EAX + 20] + 1
        let mut e = Expression::new();
        e.0 = vec![
            VarOp(VariableSymbol::Register(Register::EAX)), Value(20), Add(0, 1), Dereference(2), VarOp(VariableSymbol::Ram(ptr)), Value(1), Add(4, 5), Assign(3, 6)
        ]; // e = [?EAX + 20] := ?data@[?EAX + 20] + 1
        


        let val_expr = Expression::from(4917232);
        let call_result = ExpressionOp::Variable(VariableSymbol::CallResult{call_from:crate::ir::Address::NULL, call_to:val_expr});
        let mut eax = Expression::new();
        eax.0 = vec![ 
            call_result.clone()
        ]; // eax = ?call_4b07f0_result


        e.replace_variable_with(|old_e| {
            match old_e {
                VariableSymbol::Register(r) => {
                    if *r == Register::EAX { Some(std::borrow::Cow::Borrowed(&eax)) } else { None }
                },
                VariableSymbol::Ram(d) => {
                    let mut d = d.clone();
                    d.replace_variable_with_expression(0, &eax);
                    Some(std::borrow::Cow::Owned(Expression::from(VariableSymbol::Ram(d))))
                }
                _ => panic!("No such variables should be here")
            }
        });

        let mut new_ptr = Expression::new();
        new_ptr.0 = vec![
            call_result.clone(), Value(20), Add(0, 1), Dereference(2)
        ];

        assert_eq!(e.0, vec![
            call_result.clone(), Value(20), Add(0, 1), Dereference(2), VarOp(VariableSymbol::Ram(Expression::from(new_ptr))), Value(1), Add(4, 5), Assign(3, 6)
        ])
    }

    #[test]
    fn test_replace_flatten() {
        use ExpressionOp::{Value, Add, Dereference, Sub, Variable as VarOp};
        let mut first = Expression::from(VariableSymbol::Register(Register::ESP));
        first.add_value(180);
        // first = ?ESP + 180

        let mut second = Expression::from(VariableSymbol::Register(Register::ESP));
        second.sub_value(12);
        second.dereference();
        // second = [?ESP - 12]

        second.replace_variable_with_expression(0, &first);
        // second = [?ESP +180 - 12]
        assert_eq!(second.0, vec![
            VarOp(VariableSymbol::Register(Register::ESP)), Value(168), Add(0, 1), Dereference(2)
        ])
    }

    #[test]
    fn test_sub_at_end() {
        use ExpressionOp::{Value, Add, Dereference, Sub};
        use Register::EAX;
        let mut e = Expression::from(vec![var_reg(EAX), Value(10), Sub(0, 1)]);
        e.sub_value_at(2, 9, false);
        assert_eq!(e.0, vec![var_reg(EAX), Value(19), Sub(0,1)])
    }

    #[test]
    fn test_sub_at_end_inverted() {
        use ExpressionOp::{Value, Add, Dereference, Sub};
        use Register::EAX;
        let mut e = Expression::from(vec![var_reg(EAX), Value(10), Sub(1, 0)]);
        e.sub_value_at(2, 9, false);
        assert_eq!(e.0, vec![var_reg(EAX), Value(1), Sub(1, 0)]);

        let mut e = Expression::from(vec![var_reg(EAX), Value(10), Sub(1, 0)]);
        e.sub_value_at(2, 9, true); // Doesn't matter because on-stack Sub() operation gets openned anyway.
        assert_eq!(e.0, vec![var_reg(EAX), Value(1), Sub(1, 0)]);

        let mut e = Expression::from(vec![var_reg(EAX), Value(10), Sub(1, 0), Dereference(2)]);
        e.sub_value_at(3, 9, false);
        assert_eq!(e.0, vec![var_reg(EAX), Value(10), Sub(1, 0), Dereference(2), Value(9), Sub(3, 4)]);

        let mut e = Expression::from(vec![var_reg(EAX), Value(10), Sub(1, 0), Dereference(2)]);
        e.sub_value_at(3, 9, true);
        assert_eq!(e.0, vec![var_reg(EAX), Value(10), Sub(1, 0), Dereference(2), Value(9), Sub(4, 3)]);
    }

    #[test]
    fn test_replace_var_with_value() {
        use ExpressionOp::{Value, Add, Dereference, Sub, Assign};
        use Register::EDI;
        let mut expression = Expression::from(vec![Value(6721484), Dereference(0), var_reg(EDI), Sub(1, 2)]);
        let r = expression.replace_variable_with_expression(2, &Expression::from(0));
        assert_eq!(expression.0, vec![Value(6721484), Dereference(0)]);
        assert_eq!(r, -2);
    }

    #[test]
    fn test_replace_flatten_with_value() {
        use ExpressionOp::{Value, Add, Dereference, Sub, Assign};
        use Register::ESP;
        let mut expression = Expression::from(vec![
            var_reg(ESP),                               // 0                                     
            Value(116),                                // 1         
            Add(0, 1),                                 // 2     
            Dereference(2),                            // 3             
            Value(156),                                // 4         
            var_reg(ESP),                               // 5                                     
            Value(76),                                 // 6     
            Add(5, 6),                                 // 7     
            Dereference(7),                            // 8             
            Sub(4, 8),                                 // 9     
            Assign(3, 9)]);                            // 10         

        let sub = Expression::from(vec![var_reg(ESP), Value(172), Sub(0, 1)]);

        expression.replace_variable_with_expression(0, &sub);

        assert_eq!(expression.0, vec![
            var_reg(ESP), 
            Value(56), 
            Sub(0, 1),
            Dereference(2),
            var_reg(ESP), 
            Value(76), 
            Add(4, 5), 
            Dereference(6), 
            Value(156), 
            Sub(8, 7), 
            Assign(3, 9)
        ])

    }

    // #[test]
    // fn test_to_symbol() {
    //     use ExpressionOp::{Sub, Dereference, Variable as VarOp};
    //     let mut e = Expression::from(VariableSymbol::Register(Register::ESP));
    //     e.sub_value(12);
    //     e.dereference();
    //     // e = [?ESP - 12]

    //     let mut symbol_map = std::collections::HashMap::new();
    //     let mut symbols = Vec::new();
    //     let high_e = e.to_high(&mut symbol_map, &mut symbols, e.get_entry_point());

    //     // assert_eq!(high_e.0, vec![VarOp(Variable::Symbol(0)), Dereference(0)]);
    //     assert_eq!(symbols.len(), 1);
    //     // symbols[0].name = "param_1".into();
    //     // println!("{high_e:?}");

    // }
}