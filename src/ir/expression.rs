//! # Hybrid SSA/Expression Tree System for Decompilation
//!
//! This module implements a hybrid SSA def-use chain and expression tree system specifically
//! designed for binary decompilation. The system serves as a heuristic for quick expression
//! comparison without requiring heavy SMT solvers like Z3.
//!
//! ## Design Philosophy
//!
//! The core insight is that in compiled code, instructions generally operate on one concept
//! at a time. This allows for aggressive inline expression folding at construction time,
//! resulting in expressions that are very likely to end up in exactly the same canonical form.
//! This enables fast hash-based expression equality comparison.
//!
//! ## Architecture
//!
//! ### Flat Storage Structure
//! Expressions are stored as `SmallVec<ExpressionOp>` with indices pointing to operations.
//! This flat structure provides:
//! - **Performance**: [`ExpressionOp`]s are stored contiguously in memory for cache locality
//! - **Quick Scanning**: Code can rapidly scan expressions to detect patterns like
//!   "expression + immediate", "expression * immediate", or even "(e * imm) + (e * imm2)"
//! - **Canonical Forms**: Enables the heuristic optimization that keeps expressions normalized
//!
//! ### Variable Symbols
//! - `VariableSymbol::Ram`: Represents unknown memory state at some address (address itself can be an [`Expression`])
//! - `VariableSymbol::Varnode`: Exactly matches Ghidra's SLEIGH varnode definition, enabling
//!   support for multiple CPU architectures through SLEIGH
//!
//! ### Immediate Optimization
//! Construction-time optimization (like in [`Expression::add_value_at`]) is a key design feature that:
//! - Keeps expressions in canonical form for analysis
//! - Avoids the need for heavy SMT solvers
//! - Provides performance benefits through cache locality and reduced allocations
//!
//! ## Decompiler Integration
//!
//! This system lifts Ghidra's PCode into these expressions by executing pcode and maintaining:
//! - Varnode states as [`Expression`]s
//! - RAM states as [`Expression`]s (where addresses are also [`Expression`]s)
//!
//! Basic blocks in this system don't have direct address mappings - they simply track that
//! "at the end of the block, all varnodes have state `Expression`, and all written RAM addresses
//! have state `Expression`".

use std::{borrow::Cow, ops::Index};

use crate::ir::basic_block::DestinationKind;

use super::Address;

use pcode::{VarNode, VarSize};
use sleigh_compile::ldef::SleighLanguage;
use smallvec::{smallvec, SmallVec};

/// Size of the inline array in SmallVec for ExpressionOp storage.
/// This value is chosen based on empirical observation that most expressions
/// contain fewer than 12 operations, avoiding heap allocation in the common case.
/// Higher values result in less heap allocation but more memory usage.
pub(crate) const SMALLVEC_SIZE: usize = 12;

/// Represents symbolic variables in the decompilation process.
///
/// Variable symbols capture unknown or symbolic state that cannot be resolved
/// at analysis time. These form the leaves of expression trees and represent
/// the fundamental unknowns that expressions are built upon.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum VariableSymbol {
    /// Unknown state of a PCode varnode, typically a CPU register.
    ///
    /// Uses Ghidra's [SLEIGH varnode definition](https://ghidra.re/ghidra_docs/languages/html/sleigh.html#sleigh_varnodes) exactly, enabling support for
    /// multiple CPU architectures through the SLEIGH processor specification language.
    Varnode(VarNode),

    /// Unknown state of a function call return value.
    ///
    /// Tracked separately from other variables as call results often have
    /// associated semantic meaning that's useful for developers during analysis.
    CallResult {
        /// Address where the call instruction is located
        call_from: Address,
        /// Destination of the call (function address or indirect target)
        call_to: Box<DestinationKind>,
    },

    /// Unknown state of memory (RAM) at a specific address and size.
    ///
    /// The address itself can be an Expression, allowing for complex memory
    /// access patterns like `[ESP + offset]` or `[EBX + ECX*4 + 8]`.
    /// The u8 represents the size of the memory access in bytes.
    Ram(Box<Expression>, u8),
}

impl std::fmt::Debug for VariableSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Varnode(arg0) => f.write_fmt(format_args!("?{arg0:?}")),
            Self::CallResult { call_to, .. } => {
                f.write_fmt(format_args!("call_{call_to:?}_result"))
            }
            Self::Ram(arg0, _) => f.write_fmt(format_args!("[{arg0:?}]")),
        }
    }
}

impl std::fmt::Display for VariableSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Varnode(arg0) => f.write_fmt(format_args!("?{arg0:?}")),
            Self::CallResult { call_to, .. } => f.write_fmt(format_args!("call_{call_to}_result")),
            Self::Ram(arg0, _) => f.write_fmt(format_args!("ram[{arg0}]")),
        }
    }
}

impl VariableSymbol {
    // pub fn get_memory_address_or_null(&self) -> Address {
    //     match self {
    //         VariableSymbol::Register(_) |
    //         VariableSymbol::CallResult{..} => Address::NULL,
    //         VariableSymbol::Ram(expression) => expression.get_memory_address_or_null(),
    //     }
    // }
}

impl FormatWithSleighLanguage for VariableSymbol {
    fn display_fmt(
        &self,
        lang: Option<&SleighLanguage>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Varnode(arg0) => {
                if let Some(name) = lang.and_then(|l| l.sleigh.name_of_varnode(*arg0)) {
                    f.write_str("?")?;
                    f.write_str(name)
                } else {
                    f.write_fmt(format_args!("?{arg0:?}"))
                }
            }
            Self::CallResult { call_to, .. } => {
                f.write_str("call_")?;
                call_to.display_fmt(lang, f)?;
                f.write_str("_result")
            }
            Self::Ram(arg0, _) => {
                f.write_str("ram[")?;
                arg0.display_fmt(lang, f)?;
                f.write_str("]")
            }
        }
    }

    fn debug_fmt(
        &self,
        lang: Option<&SleighLanguage>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        todo!()
    }
}

impl Index<OpIdx> for Expression {
    type Output = ExpressionOp;

    fn index(&self, index: OpIdx) -> &Self::Output {
        self.0.index(index)
    }
}

/// Represents the size of an instruction operand or operation result.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum InstructionSize {
    /// 8-bit operation (byte)
    U8,
    /// 16-bit operation (word)
    U16,
    /// 32-bit operation (dword)
    U32,
    /// 64-bit operation (qword)
    U64,
}

/// Indicates whether an operation should be interpreted as signed or unsigned.
///
/// This affects comparison operations and overflow detection, as the same
/// bit pattern can represent different values depending on signedness.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum SignedOrUnsiged {
    /// Treat operands as signed integers (two's complement)
    Signed,
    /// Treat operands as unsigned integers
    Unsigned,
}

impl Into<InstructionSize> for VarSize {
    fn into(self) -> InstructionSize {
        use InstructionSize::*;
        match self {
            1 => U8,
            2 => U16,
            4 => U32,
            8 => U64,
            _ => panic!("Unexpected varnode size"),
        }
    }
}

/// Trait for formatting types with optional SLEIGH language context.
///
/// VarNode expression stores only register numbers, and we need [`SleighLanguage`] to convert register numbers to register names.
///
/// Enables pretty-printing of expressions with proper register names and
/// architecture-specific formatting when SLEIGH language information is available.
pub(crate) trait FormatWithSleighLanguage {
    fn display_fmt(
        &self,
        lang: Option<&SleighLanguage>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;

    fn debug_fmt(
        &self,
        lang: Option<&SleighLanguage>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;
}

/// Individual operations that can appear in an expression tree.
///
/// Each operation stores indices [`OpIdx`] that reference other operations
/// in the same expression's flat storage array. This enables efficient
/// representation while maintaining tree semantics.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ExpressionOp {
    // === Leaf Nodes ===
    /// A symbolic variable (register, memory, or call result)
    Variable(VariableSymbol),
    /// A destination register for assignment operations
    DestinationRegister(VarNode),
    /// A constant integer value, always unsigned
    Value(u64),

    // === Memory Operations ===
    /// Memory dereference: `*operand`
    Dereference(OpIdx),

    // === SSA Operations ===
    /// Assignment: `lhs := rhs` (used for pretty-printing)
    Assign(OpIdx, OpIdx),
    /// Multi-assignment (phi function in SSA): merge of multiple values
    Multiequals(OpIdx, OpIdx),

    // === Arithmetic Operations ===
    /// Addition: `lhs + rhs`
    Add(OpIdx, OpIdx, InstructionSize),
    /// Subtraction: `lhs - rhs`
    Sub(OpIdx, OpIdx, InstructionSize),
    /// Multiplication: `lhs * rhs`
    Multiply(OpIdx, OpIdx, InstructionSize),

    // === Comparison Operations ===
    /// Less than or equal: `lhs <= rhs`
    LessOrEquals(OpIdx, OpIdx, SignedOrUnsiged),
    /// Less than: `lhs < rhs`
    Less(OpIdx, OpIdx, SignedOrUnsiged),
    /// Greater than or equal: `lhs >= rhs`
    GreaterOrEquals(OpIdx, OpIdx, SignedOrUnsiged),
    /// Greater than: `lhs > rhs`
    Greater(OpIdx, OpIdx, SignedOrUnsiged),
    /// Equality: `lhs == rhs`
    Equals(OpIdx, OpIdx, SignedOrUnsiged),
    /// Inequality: `lhs != rhs`
    NotEquals(OpIdx, OpIdx, SignedOrUnsiged),

    // === Bitwise Operations ===
    /// Right bit shift: `lhs >> rhs`
    BitShiftRight(OpIdx, OpIdx, InstructionSize),
    /// Left bit shift: `lhs << rhs`
    BitShiftLeft(OpIdx, OpIdx, InstructionSize),
    /// Bitwise AND: `lhs & rhs`
    And(OpIdx, OpIdx),
    /// Bitwise OR: `lhs | rhs`
    Or(OpIdx, OpIdx),
    /// Bitwise NOT: `~lhs`
    Not(OpIdx),
    /// Bitwise XOR: `lhs ^ rhs`
    Xor(OpIdx, OpIdx),

    // === Special Operations ===
    /// Overflow detection for the operand
    Overflow(OpIdx, SignedOrUnsiged),
    /// Population count (number of 1 bits)
    CountOnes(OpIdx),
}

/// A flat-storage expression tree optimized for decompilation analysis.
///
/// This is the fundamental building block for representing program semantics during
/// decompilation. Expressions can be symbolic (containing [`VariableSymbol`]s) or
/// concrete (containing only values and operations).
///
/// ## Storage Format
///
/// Operations are stored in a flat, inlined [`SmallVec`] with the **root operation as the last element**.
/// Child operations are referenced by index [`OpIdx`] within the same vector. This design
/// enables:
/// - **Cache locality**: All operations in an expression are stored contiguously
/// - **Quick pattern matching**: Easy to scan and detect common patterns
/// - **Efficient copying**: Single allocation for entire expression trees
///
/// ## Immediate Optimization
///
/// The expression system performs aggressive optimization during construction:
/// - `ESP + 4 + 4` becomes `ESP + 8` automatically
/// - `VAR * 1` becomes `VAR`
/// - `VAR + 0` becomes `VAR`
///
/// This keeps expressions in canonical form, enabling fast hash-based equality
/// comparison without requiring complex symbolic reasoning.
///
/// ## Formatting
///
/// - `Display`: Prints recursively in mathematical notation, e.g., `[ESP + 4]`
/// - `Debug`: Shows the flat operation list, e.g., `[Variable(ESP), Value(4), Add(0,1), Deref(2)]`
///
/// ## Examples
///
/// ```ignore
/// let mut expr = Expression::from(VariableSymbol::Varnode(esp_varnode));
/// expr.add_value(4, InstructionSize::U32);  // ESP + 4
/// expr.dereference();                       // [ESP + 4]
/// ```
#[derive(Clone, Hash, PartialEq, Eq, Default)]
pub struct Expression(SmallVec<[ExpressionOp; SMALLVEC_SIZE]>);

/// Index type for referencing operations within an Expression.
///
/// Currently `usize` but could be optimized to `u8`. If SMALLVEC_SIZE stays at 12,
/// This change would save ~100 bytes per expression at the cost of converting `u8` to `usize`.
type OpIdx = usize;

impl From<u64> for Expression {
    fn from(value: u64) -> Self {
        Self(smallvec![ExpressionOp::Value(value)])
    }
}

impl From<Address> for Expression {
    fn from(value: Address) -> Self {
        Self(smallvec![ExpressionOp::Value(value.0 as u64)])
    }
}

impl From<VariableSymbol> for Expression {
    fn from(variable: VariableSymbol) -> Self {
        Self(smallvec![ExpressionOp::Variable(variable)])
    }
}

impl From<SmallVec<[ExpressionOp; SMALLVEC_SIZE]>> for Expression {
    fn from(value: SmallVec<[ExpressionOp; SMALLVEC_SIZE]>) -> Self {
        Self(value)
    }
}

// impl From<Register> for Expression {
//     fn from(register: Register) -> Self {
//         Self(smallvec![ExpressionOp::DestinationRegister(register)])
//     }
// }
impl From<ExpressionOp> for Expression {
    fn from(op: ExpressionOp) -> Self {
        Self(smallvec![op])
    }
}

fn remap_operands<T>(
    src: &[ExpressionOp],
    pos: OpIdx,
    vec: &mut SmallVec<[ExpressionOp; SMALLVEC_SIZE]>,
    mut map: T,
) where
    T: Copy + FnMut(&ExpressionOp, &[ExpressionOp]) -> ExpressionOp,
{
    match &src[pos] {
        e @ ExpressionOp::Variable(_)
        | e @ ExpressionOp::DestinationRegister(_)
        | e @ ExpressionOp::Value(_) => vec.push(map(e, vec)),
        ExpressionOp::Dereference(p) => {
            remap_operands(src, *p, vec, map);
            vec.push(ExpressionOp::Dereference(vec.len() - 1));
        }
        ExpressionOp::Overflow(p, sgn) => {
            remap_operands(src, *p, vec, map);
            vec.push(ExpressionOp::Overflow(vec.len() - 1, *sgn));
        }
        ExpressionOp::CountOnes(p) => {
            remap_operands(src, *p, vec, map);
            vec.push(ExpressionOp::CountOnes(vec.len() - 1));
        }
        ExpressionOp::Assign(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Assign(l, r));
        }
        ExpressionOp::Multiequals(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Multiequals(l, r));
        }
        ExpressionOp::Add(l, r, size) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Add(l, r, *size));
        }
        ExpressionOp::Sub(l, r, size) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Sub(l, r, *size));
        }
        ExpressionOp::Multiply(l, r, size) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Multiply(l, r, *size));
        }
        ExpressionOp::LessOrEquals(l, r, sgn) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::LessOrEquals(l, r, *sgn));
        }
        ExpressionOp::Less(l, r, sgn) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Less(l, r, *sgn));
        }
        ExpressionOp::GreaterOrEquals(l, r, sgn) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::GreaterOrEquals(l, r, *sgn));
        }
        ExpressionOp::Greater(l, r, sgn) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Greater(l, r, *sgn));
        }
        ExpressionOp::Equals(l, r, sgn) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Equals(l, r, *sgn));
        }
        ExpressionOp::NotEquals(l, r, sgn) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::NotEquals(l, r, *sgn));
        }
        ExpressionOp::BitShiftRight(l, r, size) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::BitShiftRight(l, r, *size));
        }
        ExpressionOp::BitShiftLeft(l, r, size) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::BitShiftLeft(l, r, *size));
        }
        ExpressionOp::And(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::And(l, r));
        }
        ExpressionOp::Or(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::And(l, r));
        }
        ExpressionOp::Not(l) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            vec.push(ExpressionOp::Not(l));
        }
        ExpressionOp::Xor(l, r) => {
            remap_operands(src, *l, vec, map);
            let l = vec.len() - 1;
            remap_operands(src, *r, vec, map);
            let r = vec.len() - 1;
            vec.push(ExpressionOp::Xor(l, r));
        }
    }
}

impl Expression {
    /// Create a new empty expression.
    ///
    /// This creates an expression with no operations, which is typically used
    /// as a starting point before building up an expression tree through
    /// method calls like `add_value()`, `dereference()`, etc.
    ///
    /// # Returns
    /// An empty expression ready for operation building
    pub fn new() -> Self {
        Self(SmallVec::new())
    }

    /// Get the last (root) operation in this expression, if any.
    ///
    /// Since expressions store their root operation as the last element,
    /// this effectively returns the last operation of the expression array. Returns
    /// `None` if the expression is empty.
    ///
    /// # Returns
    /// The root operation of the expression, or `None` if empty
    pub fn root_op(&self) -> Option<&ExpressionOp> {
        self.0.last()
    }

    /// Get a reference to the operation at the specified index.
    ///
    /// This provides direct access to individual operations within the
    /// expression's flat storage array. Used primarily for internal
    /// algorithms and debugging.
    ///
    /// # Arguments
    /// * `idx` - Index of the operation to retrieve
    ///
    /// # Returns
    /// Reference to the operation at the specified index
    pub fn get(&self, idx: OpIdx) -> &ExpressionOp {
        &self.0[idx]
    }

    /// Get an iterator over all operations in this expression.
    ///
    /// Iterates through the operations in storage order (not execution order).
    /// The last element returned by the iterator will be the root operation.
    /// Useful for analysis and debugging of expression structure.
    ///
    /// # Returns
    /// An iterator over all `ExpressionOp`s in the expression
    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, ExpressionOp> {
        self.0.iter()
    }

    /// Extract a sub-expression starting at the specified operation index.
    ///
    /// This creates a new, standalone expression containing only the sub-tree
    /// rooted at the given index. All operation indices are remapped to create
    /// a self-contained expression. Useful for extracting parts of complex
    /// expressions for separate analysis.
    ///
    /// # Arguments
    /// * `idx` - Index of the operation to use as the root of the sub-expression
    ///
    /// # Returns
    /// A new expression containing only the specified sub-tree
    pub fn get_sub_expression(&self, idx: OpIdx) -> Expression {
        let mut result = Expression::new();
        remap_operands(&self.0, idx, &mut result.0, |e, _| e.clone());
        result
    }

    /// Multiply this expression by another expression with immediate optimization.
    ///
    /// Performs construction-time optimization when possible:
    /// - If either expression is a constant, delegates to `multiply_value()`
    /// - Automatically reorders operands to put constants in optimal position
    /// - For complex expressions, creates a multiply operation
    ///
    /// # Arguments
    /// * `other` - The expression to multiply this one by
    /// * `size` - The instruction size for overflow handling
    pub fn multiply<S: Into<InstructionSize>>(&mut self, other: &Self, size: S) {
        if let Some(ExpressionOp::Value(v)) = other.root_op() {
            self.multiply_value(*v, size.into());
        } else if let Some(ExpressionOp::Value(v)) = self.root_op() {
            // self is a simple value but other is not - swap for better optimization
            // Since addition is commutative: v + other = other + v
            let temp_v = *v;
            self.0.clear();
            self.copy_other_to_end(&other.0);
            self.multiply_value(temp_v, size.into());
        } else {
            let left = self.get_entry_point();
            self.copy_other_to_end(&other.0);
            let right = self.get_entry_point();
            self.0
                .push(ExpressionOp::Multiply(left, right, size.into()))
        }
    }

    /// Add another expression to this expression with immediate optimization.
    ///
    /// Performs construction-time optimization when possible:
    /// - If either expression is a constant, delegates to `add_value()`
    /// - Automatically reorders operands to put constants in optimal position
    /// - For complex expressions, creates an add operation
    ///
    /// This leverages the commutative property of addition to ensure constants
    /// are positioned for maximum optimization opportunities.
    ///
    /// # Arguments
    /// * `other` - The expression to add to this one
    /// * `size` - The instruction size for overflow handling
    pub fn add<S: Into<InstructionSize>>(&mut self, other: &Self, size: S) {
        if let Some(ExpressionOp::Value(v)) = other.root_op() {
            self.add_value(*v, size);
        } else if let Some(ExpressionOp::Value(v)) = self.root_op() {
            // self is a simple value but other is not - swap for better optimization
            // Since addition is commutative: v + other = other + v
            let temp_v = *v;
            self.0.clear();
            self.copy_other_to_end(&other.0);
            self.add_value(temp_v, size);
        } else {
            let left = self.get_entry_point();
            self.copy_other_to_end(&other.0);
            let right = self.get_entry_point();
            self.0.push(ExpressionOp::Add(left, right, size.into()))
        }
    }

    /// Subtract another expression from this expression with immediate optimization.
    ///
    /// Performs construction-time optimization when possible:
    /// - If the other expression is a constant, delegates to `sub_value()`
    /// - If this expression is a constant, rearranges to optimize the result
    /// - For complex expressions, creates a subtract operation
    ///
    /// Note: Unlike addition, subtraction is not commutative, so the optimization
    /// strategy differs when operands are swapped.
    ///
    /// # Arguments
    /// * `other` - The expression to subtract from this one
    /// * `size` - The instruction size for overflow handling
    pub fn sub<S: Into<InstructionSize>>(&mut self, other: &Self, size: S) {
        if let Some(ExpressionOp::Value(v)) = other.root_op() {
            self.sub_value(*v, size);
        } else if let Some(ExpressionOp::Value(v)) = self.root_op() {
            // self is a simple value but other is not - swap for better optimization
            // Since addition is commutative: v + other = other + v
            let temp_v = *v;
            self.0.clear();
            self.copy_other_to_end(&other.0);
            self.sub_value(temp_v, size);
        } else {
            let left = self.get_entry_point();
            self.copy_other_to_end(&other.0);
            let right = self.get_entry_point();
            self.0.push(ExpressionOp::Sub(left, right, size.into()))
        }
    }

    // Negate current value or calculate 0 - self
    pub fn negate<S: Into<InstructionSize>>(&mut self, size: S) {
        let mut left = Expression::from(0);
        left.sub(self, size);
        *self = left;
    }

    /// Add a constant value to this expression with immediate optimization.
    ///
    /// This method performs construction-time optimization to maintain canonical form:
    /// - `(ESP + 4) + 4` becomes `ESP + 8`
    /// - `5 + 3` becomes `8`
    /// - `VAR + 0` remains `VAR` (no operation added)
    ///
    /// # Arguments
    /// * `value` - The constant value to add
    /// * `size` - The instruction size for overflow handling
    pub fn add_value<S: Into<InstructionSize>>(&mut self, value: u64, size: S) {
        let expr = self.get_entry_point();
        self.add_value_at(expr, value, size);
    }

    /// Add a constant value to a specific sub-expression with immediate optimization.
    ///
    /// This is the core optimization engine that performs algebraic simplification
    /// during expression construction. It examines the operation at `expr` and:
    ///
    /// - If it's `Add(a, Value(v))`, updates the value to `v + value`
    /// - If it's `Sub(a, Value(v))`, handles the subtraction appropriately
    /// - If it's `Value(v)`, replaces with `Value(v + value)`
    /// - Otherwise, creates new `Add` operation
    ///
    /// This aggressive optimization keeps expressions in canonical form, enabling
    /// fast hash-based equality comparison between semantically equivalent expressions.
    ///
    /// # Arguments
    /// * `expr` - Index of the operation to add the value to
    /// * `value` - The constant value to add (no-op if 0)
    /// * `size` - Instruction size for proper overflow behavior
    ///
    /// # Returns
    /// Number of new operations added to the expression (0 if optimized in-place)
    fn add_value_at<S: Into<InstructionSize>>(
        &mut self,
        expr: OpIdx,
        value: u64,
        size: S,
    ) -> usize {
        use ExpressionOp::*;
        if value == 0 {
            return 0;
        }
        fn cant_optimize(
            e: &mut Expression,
            value: u64,
            expr: OpIdx,
            size: InstructionSize,
        ) -> usize {
            e.0.push(ExpressionOp::Value(value));
            let pos = e.get_entry_point();
            e.0.push(ExpressionOp::Add(expr, pos, size));
            2
        }

        let size = size.into();
        match self.0[expr] {
            Add(l, r, other_size) => {
                if other_size == size {
                    if let Value(v) = &mut self.0[r] {
                        match size {
                            InstructionSize::U8 => *v = (*v as u8).wrapping_add(value as u8) as u64,
                            InstructionSize::U16 => {
                                *v = (*v as u16).wrapping_add(value as u16) as u64
                            }
                            InstructionSize::U32 => {
                                *v = (*v as u32).wrapping_add(value as u32) as u64
                            }
                            InstructionSize::U64 => *v = (*v).wrapping_add(value),
                        }
                    } else if let Value(v) = &mut self.0[l] {
                        match size {
                            InstructionSize::U8 => *v = (*v as u8).wrapping_add(value as u8) as u64,
                            InstructionSize::U16 => {
                                *v = (*v as u16).wrapping_add(value as u16) as u64
                            }
                            InstructionSize::U32 => {
                                *v = (*v as u32).wrapping_add(value as u32) as u64
                            }
                            InstructionSize::U64 => *v = (*v).wrapping_add(value),
                        }
                    }
                    0
                } else {
                    cant_optimize(self, value, expr, size)
                }
            }
            Sub(l, r, other_size) => {
                if other_size == size {
                    if let Value(v) = &mut self.0[r] {
                        // adding `value` to `some - v`
                        if *v > value {
                            match size {
                                InstructionSize::U8 => {
                                    *v = (*v as u8).wrapping_sub(value as u8) as u64
                                }
                                InstructionSize::U16 => {
                                    *v = (*v as u16).wrapping_sub(value as u16) as u64
                                }
                                InstructionSize::U32 => {
                                    *v = (*v as u32).wrapping_sub(value as u32) as u64
                                }
                                InstructionSize::U64 => *v = (*v).wrapping_sub(value),
                            }
                        } else {
                            match size {
                                InstructionSize::U8 => {
                                    *v = (value as u8).wrapping_sub(*v as u8) as u64
                                }
                                InstructionSize::U16 => {
                                    *v = (value as u16).wrapping_sub(*v as u16) as u64
                                }
                                InstructionSize::U32 => {
                                    *v = (value as u32).wrapping_sub(*v as u32) as u64
                                }
                                InstructionSize::U64 => *v = (value).wrapping_sub(*v),
                            }
                            self.0[expr] = Add(l, r, size)
                        }
                    } else if let Value(v) = &mut self.0[l] {
                        // adding `value` to `v - some`
                        match size {
                            InstructionSize::U8 => *v = (*v as u8).wrapping_add(value as u8) as u64,
                            InstructionSize::U16 => {
                                *v = (*v as u16).wrapping_add(value as u16) as u64
                            }
                            InstructionSize::U32 => {
                                *v = (*v as u32).wrapping_add(value as u32) as u64
                            }
                            InstructionSize::U64 => *v = (*v).wrapping_add(value),
                        }
                    }
                    0
                } else {
                    cant_optimize(self, value, expr, size)
                }
            }
            Value(v) => {
                self.0[expr] = Value(match size {
                    InstructionSize::U8 => (v as u8).wrapping_add(value as u8) as u64,
                    InstructionSize::U16 => (v as u16).wrapping_add(value as u16) as u64,
                    InstructionSize::U32 => (v as u32).wrapping_add(value as u32) as u64,
                    InstructionSize::U64 => (v).wrapping_add(value),
                });
                0
            }
            _ => cant_optimize(self, value, expr, size),
        }
    }

    /// Subtract a constant value from this expression with immediate optimization.
    ///
    /// Similar to [`Self::add_value`] but performs subtraction with intelligent optimization:
    /// - `(ESP + 8) - 4` becomes `ESP + 4`
    /// - `(ESP - 4) - 4` becomes `ESP - 8`
    /// - `10 - 3` becomes `7`
    /// - `VAR - 0` remains `VAR` (no operation added)
    ///
    /// # Arguments
    /// * `value` - The constant value to subtract
    /// * `size` - The instruction size for overflow handling
    pub fn sub_value<S: Into<InstructionSize>>(&mut self, value: u64, size: S) {
        let expr = self.get_entry_point();
        self.sub_value_at(expr, value, false, size);
    }

    fn sub_value_at<S: Into<InstructionSize>>(
        &mut self,
        expr: OpIdx,
        value: u64,
        is_invert: bool,
        size: S,
    ) -> usize {
        use ExpressionOp::*;
        if value == 0 {
            return 0;
        }

        fn cant_optimize(
            e: &mut Expression,
            value: u64,
            expr: OpIdx,
            is_invert: bool,
            size: InstructionSize,
        ) -> usize {
            e.0.push(ExpressionOp::Value(value));
            let pos = e.get_entry_point();
            if is_invert {
                e.0.push(ExpressionOp::Sub(pos, expr, size));
            } else {
                e.0.push(ExpressionOp::Sub(expr, pos, size));
            }
            2
        }
        let size = size.into();
        match self.0[expr] {
            Add(l, r, other_size) => {
                if size == other_size {
                    if let Value(v) = &mut self.0[r] {
                        // subtracting `value` from `some + v`
                        if *v > value {
                            match size {
                                InstructionSize::U8 => {
                                    *v = (*v as u8).wrapping_sub(value as u8) as u64
                                }
                                InstructionSize::U16 => {
                                    *v = (*v as u16).wrapping_sub(value as u16) as u64
                                }
                                InstructionSize::U32 => {
                                    *v = (*v as u32).wrapping_sub(value as u32) as u64
                                }
                                InstructionSize::U64 => *v = (*v).wrapping_sub(value),
                            }
                        } else {
                            match size {
                                InstructionSize::U8 => {
                                    *v = (value as u8).wrapping_sub(*v as u8) as u64
                                }
                                InstructionSize::U16 => {
                                    *v = (value as u16).wrapping_sub(*v as u16) as u64
                                }
                                InstructionSize::U32 => {
                                    *v = (value as u32).wrapping_sub(*v as u32) as u64
                                }
                                InstructionSize::U64 => *v = (value).wrapping_sub(*v),
                            }
                            self.0[expr] = Sub(l, r, size)
                        }
                    } else if let Value(v) = &mut self.0[l] {
                        // subtracting `value` from `v + some`
                        if *v > value {
                            match size {
                                InstructionSize::U8 => {
                                    *v = (*v as u8).wrapping_sub(value as u8) as u64
                                }
                                InstructionSize::U16 => {
                                    *v = (*v as u16).wrapping_sub(value as u16) as u64
                                }
                                InstructionSize::U32 => {
                                    *v = (*v as u32).wrapping_sub(value as u32) as u64
                                }
                                InstructionSize::U64 => *v = (*v).wrapping_sub(value),
                            }
                        } else {
                            match size {
                                InstructionSize::U8 => {
                                    *v = (value as u8).wrapping_sub(*v as u8) as u64
                                }
                                InstructionSize::U16 => {
                                    *v = (value as u16).wrapping_sub(*v as u16) as u64
                                }
                                InstructionSize::U32 => {
                                    *v = (value as u32).wrapping_sub(*v as u32) as u64
                                }
                                InstructionSize::U64 => *v = (value).wrapping_sub(*v),
                            }
                            self.0[expr] = Sub(r, l, size) // turn this into `some - v`
                        }
                    }
                    0
                } else {
                    cant_optimize(self, value, expr, is_invert, size)
                }
            }
            Sub(l, r, other_size) => {
                if size == other_size {
                    if let Value(v) = &mut self.0[r] {
                        // subtracting `value` from `some - v`
                        match size {
                            InstructionSize::U8 => *v = (*v as u8).wrapping_add(value as u8) as u64,
                            InstructionSize::U16 => {
                                *v = (*v as u16).wrapping_add(value as u16) as u64
                            }
                            InstructionSize::U32 => {
                                *v = (*v as u32).wrapping_add(value as u32) as u64
                            }
                            InstructionSize::U64 => *v = (*v).wrapping_add(value),
                        }
                    } else if let Value(v) = &mut self.0[l] {
                        // subtracting `value` from `v - some`
                        // no side check here because -v is covered by wrapping_sub
                        match size {
                            InstructionSize::U8 => *v = (*v as u8).wrapping_sub(value as u8) as u64,
                            InstructionSize::U16 => {
                                *v = (*v as u16).wrapping_sub(value as u16) as u64
                            }
                            InstructionSize::U32 => {
                                *v = (*v as u32).wrapping_sub(value as u32) as u64
                            }
                            InstructionSize::U64 => *v = (*v).wrapping_sub(value),
                        }
                    }
                    0
                } else {
                    cant_optimize(self, value, expr, is_invert, size)
                }
            }
            Value(v) => {
                self.0[expr] = Value(v.wrapping_sub(value));
                0
            }
            _ => cant_optimize(self, value, expr, is_invert, size),
        }
    }

    /// Multiply this expression by a constant value with immediate optimization.
    ///
    /// Performs construction-time optimization:
    /// - `VAR * 1` remains `VAR` (no operation added)
    /// - `VAR * 0` becomes `0`
    /// - `5 * 3` becomes `15`
    /// - Complex expressions get a multiply operation added
    ///
    /// # Arguments
    /// * `value` - The constant value to multiply by
    /// * `size` - The instruction size for overflow handling
    pub fn multiply_value(&mut self, value: u64, size: InstructionSize) {
        let expr = self.get_entry_point();
        self.multiply_value_at(expr, value, size);
    }

    /// Returns the number of operations stored in this expression.
    ///
    /// This includes all intermediate operations, not just the "height" of the tree.
    /// Useful for understanding expression complexity and memory usage.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    fn multiply_value_at(&mut self, expr: OpIdx, value: u64, size: InstructionSize) -> usize {
        use ExpressionOp::*;
        if value == 1 {
            return 0;
        }

        fn cant_optimize(
            e: &mut Expression,
            value: u64,
            expr: OpIdx,
            size: InstructionSize,
        ) -> usize {
            e.0.push(ExpressionOp::Value(value));
            let pos = e.get_entry_point();
            e.0.push(ExpressionOp::Multiply(expr, pos, size));
            2
        }
        match self.0[expr] {
            Multiply(l, r, other_size) => {
                if other_size == size {
                    if let Value(v) = &mut self.0[r] {
                        *v = v.wrapping_mul(value)
                    } else if let Value(v) = &mut self.0[l] {
                        *v = v.wrapping_mul(value)
                    }
                    0
                } else {
                    cant_optimize(self, value, expr, size)
                }
            }
            Value(v) => {
                self.0[expr] = Value(v * value);
                0
            }
            _ => cant_optimize(self, value, expr, size),
        }
    }
    /// Add a memory dereference operation to this expression.
    ///
    /// Transforms the expression from `E` to `[E]` (memory contents at address E).
    /// This is fundamental for representing memory accesses in decompiled code.
    ///
    /// # Example
    /// ```ignore
    /// let mut expr = Expression::from(esp_varnode);
    /// expr.add_value(4, InstructionSize::U32);  // ESP + 4
    /// expr.dereference();                       // [ESP + 4]
    /// ```
    pub fn dereference(&mut self) {
        let val = self.get_entry_point();
        self.0.push(ExpressionOp::Dereference(val));
    }

    /// Count the number of 1 bits in this expression (population count).
    ///
    /// For constant values, this is computed immediately. For complex expressions,
    /// a CountOnes operation is added to the expression tree.
    ///
    /// This corresponds to CPU instructions like `POPCNT` on x86.
    pub fn count_ones(&mut self) {
        let val = self.get_entry_point();
        if let ExpressionOp::Value(v) = &self.0[val] {
            self.0[val] = ExpressionOp::Value(v.count_ones() as u64);
        } else {
            self.0.push(ExpressionOp::CountOnes(val));
        }
    }

    /// Add an overflow check for this expression.
    ///
    /// Creates an expression that evaluates to 1 if the operation would overflow,
    /// 0 otherwise. The signedness parameter determines whether to check for
    /// signed or unsigned overflow.
    ///
    /// # Arguments
    /// * `sgn` - Whether to check for signed or unsigned overflow
    pub fn overflow(&mut self, sgn: SignedOrUnsiged) {
        let val = self.get_entry_point();
        self.0.push(ExpressionOp::Overflow(val, sgn));
    }

    /// Perform a right bit shift by a constant value.
    ///
    /// Shifts this expression right by `value` bits. This is equivalent to
    /// integer division by 2^value for unsigned values.
    ///
    /// # Arguments
    /// * `value` - Number of bits to shift right
    /// * `size` - Instruction size for proper bit width handling
    pub fn bit_shift_right<S: Into<InstructionSize>>(&mut self, value: u64, size: S) {
        let val = self.get_entry_point();
        self.0.push(ExpressionOp::Value(value));
        self.0
            .push(ExpressionOp::BitShiftRight(val, val + 1, size.into()));
    }

    /// Perform a left bit shift by a constant value.
    ///
    /// Shifts this expression left by `value` bits. This is equivalent to
    /// multiplication by 2^value.
    ///
    /// # Arguments
    /// * `value` - Number of bits to shift left
    /// * `size` - Instruction size for proper bit width handling
    pub fn bit_shift_left<S: Into<InstructionSize>>(&mut self, value: u64, size: S) {
        let val = self.get_entry_point();
        self.0.push(ExpressionOp::Value(value));
        self.0
            .push(ExpressionOp::BitShiftLeft(val, val + 1, size.into()));
    }

    /// Perform bitwise AND with another expression.
    ///
    /// If both expressions are constant values, the AND is computed immediately.
    /// Otherwise, creates an AND operation in the expression tree.
    ///
    /// # Arguments
    /// * `other` - The expression to AND with this one
    pub fn and(&mut self, other: &Expression) {
        if self != other {
            let left = self.get_entry_point();
            if let Some(ExpressionOp::Value(v)) = other.root_op() {
                if let ExpressionOp::Value(me) = &self.0[left] {
                    self.0[left] = ExpressionOp::Value(*v & *me);
                    return;
                } else {
                    self.0.push(ExpressionOp::Value(*v));
                }
            } else {
                self.copy_other_to_end(&other.0);
            }
            let right = self.get_entry_point();
            self.0.push(ExpressionOp::And(left, right));
        }
    }

    /// Perform bitwise OR with another expression.
    ///
    /// If both expressions are constant values, the OR is computed immediately.
    /// Otherwise, creates an OR operation in the expression tree.
    ///
    /// # Arguments
    /// * `other` - The expression to OR with this one
    pub fn or(&mut self, other: &Expression) {
        let left = self.get_entry_point();
        if let Some(ExpressionOp::Value(v)) = other.root_op() {
            if let ExpressionOp::Value(me) = &self.0[left] {
                self.0[left] = ExpressionOp::Value(*v | *me);
                return;
            } else {
                self.0.push(ExpressionOp::Value(*v));
            }
        } else {
            self.copy_other_to_end(&other.0);
        }
        let right = self.get_entry_point();
        self.0.push(ExpressionOp::Or(left, right));
    }

    pub fn xor(&mut self, other: &Expression) {
        if self.eq(&other) {
            self.0.clear();
            self.0.push(ExpressionOp::Value(0))
        } else {
            let left = self.get_entry_point();
            self.copy_other_to_end(&other.0);
            let right = self.get_entry_point();
            self.0.push(ExpressionOp::Xor(left, right));
        }
    }

    /// Remove the most recent dereference operation from this expression.
    ///
    /// This undoes a `dereference()` call, transforming `[E]` back to `E`.
    /// Used when analysis determines that a memory access isn't actually needed, in cases like `lea` instruction in x86.
    ///
    /// # Panics
    /// Panics if the last operation is not a dereference.
    pub fn cancel_dereference(&mut self) {
        assert!(matches!(self.0.pop(), Some(ExpressionOp::Dereference(_))));
    }

    /// Create an assignment expression: `this := other`.
    ///
    /// This represents an SSA assignment operation where the left side (this expression)
    /// is assigned the value of the right side (other expression). Used in pretty-printing instruction level data.
    ///
    /// *Note: Block level data uses HashMap to determine assignment targets.*
    ///
    /// # Arguments
    /// * `other` - The expression to assign to this one
    pub fn assign(&mut self, other: &Self) {
        let left = self.get_entry_point();
        self.copy_other_to_end(&other.0);
        let right = self.get_entry_point();
        self.0.push(ExpressionOp::Assign(left, right))
    }

    /// Create a multi-assignment (phi function) expression.
    ///
    /// This represents an SSA phi function where a variable can have different
    /// values depending on the control flow path taken. Used at merge points
    /// in the control flow graph **only** if execution states are different.
    ///
    /// # Arguments
    /// * `other` - The alternative expression value for this variable
    pub fn multiequals(&mut self, other: &Self) {
        let left = self.get_entry_point();
        self.copy_other_to_end(&other.0);
        let right = self.get_entry_point();
        self.0.push(ExpressionOp::Multiequals(left, right))
    }

    /// Create an equality comparison: `this == other`.
    ///
    /// If the other expression is a constant, delegates to `check_equals_value`
    /// for potential optimization. Otherwise creates an Equals operation.
    ///
    /// # Arguments
    /// * `other` - Expression to compare with
    /// * `size` - Instruction size for the comparison
    /// * `sgn` - Whether to treat operands as signed or unsigned
    pub fn check_equals<S: Into<InstructionSize>>(
        &mut self,
        other: &Expression,
        size: S,
        sgn: SignedOrUnsiged,
    ) {
        if let Some(ExpressionOp::Value(v)) = other.root_op() {
            self.check_equals_value(*v, size, sgn);
        } else if let Some(ExpressionOp::Value(v)) = self.root_op() {
            // self is a simple value but other is not - swap for better optimization
            // Since addition is commutative: v + other = other + v
            let temp_v = *v;
            self.0.clear();
            self.copy_other_to_end(&other.0);
            self.check_equals_value(temp_v, size, sgn);
        } else {
            let l = self.get_entry_point();
            self.copy_other_to_end(&other.0);
            let r = self.get_entry_point();
            self.0.push(ExpressionOp::Equals(l, r, sgn));
        }
    }

    /// Create an equality comparison with a constant: `this == value`.
    ///
    /// Uses algebraic optimization by transforming to `(this - value) == 0`,
    /// which often enables further simplification. If this results in a
    /// constant expression, the comparison is evaluated immediately.
    ///
    /// # Arguments
    /// * `value` - Constant value to compare with
    /// * `size` - Instruction size for the comparison
    /// * `sgn` - Whether to treat operands as signed or unsigned
    pub fn check_equals_value<S: Into<InstructionSize>>(
        &mut self,
        value: u64,
        size: S,
        sgn: SignedOrUnsiged,
    ) {
        self.sub_value(value, size);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r, _size) => self.0[left] = ExpressionOp::Equals(l, r, sgn),
            ExpressionOp::Value(v) => self.0[left] = ExpressionOp::Value((v == 0) as u64),
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::Equals(left, left + 1, sgn))
            }
        }
    }

    /// Create an inequality comparison with a constant: `this != value`.
    ///
    /// Uses algebraic optimization by transforming to `(this - value) != 0`,
    /// which often enables further simplification. If this results in a
    /// constant expression, the comparison is evaluated immediately.
    ///
    /// # Arguments
    /// * `value` - Constant value to compare with
    /// * `size` - Instruction size for the comparison
    /// * `sgn` - Whether to treat operands as signed or unsigned
    pub fn check_not_equals_value(
        &mut self,
        value: u64,
        size: InstructionSize,
        sgn: SignedOrUnsiged,
    ) {
        self.sub_value(value, size);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r, _size) => self.0[left] = ExpressionOp::NotEquals(l, r, sgn),
            ExpressionOp::Value(v) => self.0[left] = ExpressionOp::Value((v != 0) as u64),
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::NotEquals(left, left + 1, sgn))
            }
        }
    }

    /// Create a less-than comparison: `this < other`.
    ///
    /// If the other expression is a constant, delegates to `check_less_value`
    /// for potential optimization. Otherwise creates a Less operation.
    ///
    /// # Arguments
    /// * `other` - Expression to compare with
    /// * `size` - Instruction size for the comparison
    /// * `sgn` - Whether to treat operands as signed or unsigned
    pub fn check_less<S: Into<InstructionSize>>(
        &mut self,
        other: &Expression,
        size: S,
        sgn: SignedOrUnsiged,
    ) {
        if let Some(ExpressionOp::Value(v)) = other.root_op() {
            self.check_less_value(*v, size, sgn);
        } else {
            let l = self.get_entry_point();
            self.copy_other_to_end(&other.0);
            let r = self.get_entry_point();
            self.0.push(ExpressionOp::Less(l, r, sgn));
        }
    }

    /// Create a less-than comparison with a constant: `this < value`.
    ///
    /// Uses algebraic optimization by transforming to `(this - value) < 0`,
    /// which is equivalent to `(this - value) >= 0` for the reverse comparison.
    /// If this results in a constant expression, the comparison is evaluated immediately.
    ///
    /// # Arguments
    /// * `value` - Constant value to compare with
    /// * `size` - Instruction size for the comparison
    /// * `sgn` - Whether to treat operands as signed or unsigned
    pub fn check_less_value<S: Into<InstructionSize>>(
        &mut self,
        value: u64,
        size: S,
        sgn: SignedOrUnsiged,
    ) {
        self.sub_value(value, size);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r, _size) => {
                self.0[left] = ExpressionOp::GreaterOrEquals(l, r, sgn)
            }
            ExpressionOp::Value(v) => self.0[left] = ExpressionOp::Value(((v as i64) < 0) as u64),
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::Less(left, left + 1, sgn))
            }
        }
    }

    /// Create a greater-than comparison with a constant: `this > value`.
    ///
    /// Uses algebraic optimization by transforming to `(this - value) > 0`,
    /// which is equivalent to `(this - value) <= 0` for the reverse comparison.
    /// If this results in a constant expression, the comparison is evaluated immediately.
    ///
    /// # Arguments
    /// * `value` - Constant value to compare with
    /// * `size` - Instruction size for the comparison
    /// * `sgn` - Whether to treat operands as signed or unsigned
    pub fn check_greater_value(&mut self, value: u64, size: InstructionSize, sgn: SignedOrUnsiged) {
        self.sub_value(value, size);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r, _size) => self.0[left] = ExpressionOp::LessOrEquals(l, r, sgn),
            ExpressionOp::Value(v) => self.0[left] = ExpressionOp::Value((v > 0) as u64),
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::Greater(left, left + 1, sgn))
            }
        }
    }

    /// Create a less-than-or-equal comparison with a constant: `this <= value`.
    ///
    /// Uses algebraic optimization by transforming to `(this - value) <= 0`,
    /// which is equivalent to `(this - value) > 0` for the reverse comparison.
    /// If this results in a constant expression, the comparison is evaluated immediately.
    ///
    /// # Arguments
    /// * `value` - Constant value to compare with
    /// * `size` - Instruction size for the comparison
    /// * `sgn` - Whether to treat operands as signed or unsigned
    pub fn check_less_or_equals_value(
        &mut self,
        value: u64,
        size: InstructionSize,
        sgn: SignedOrUnsiged,
    ) {
        self.sub_value(value, size);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r, _size) => self.0[left] = ExpressionOp::Greater(l, r, sgn),
            ExpressionOp::Value(v) => self.0[left] = ExpressionOp::Value(((v as i64) <= 0) as u64),
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0.push(ExpressionOp::LessOrEquals(left, left + 1, sgn))
            }
        }
    }

    /// Create a greater-than-or-equal comparison with a constant: `this >= value`.
    ///
    /// Uses algebraic optimization by transforming to `(this - value) >= 0`,
    /// which is equivalent to `(this - value) < 0` for the reverse comparison.
    /// If this results in a constant expression, the comparison is evaluated immediately.
    ///
    /// # Arguments
    /// * `value` - Constant value to compare with
    /// * `size` - Instruction size for the comparison
    /// * `sgn` - Whether to treat operands as signed or unsigned
    pub fn check_greater_or_equals_value(
        &mut self,
        value: u64,
        size: InstructionSize,
        sgn: SignedOrUnsiged,
    ) {
        self.sub_value(value, size);
        let left = self.get_entry_point();
        match self.0[left] {
            ExpressionOp::Sub(l, r, _size) => self.0[left] = ExpressionOp::Less(l, r, sgn),
            ExpressionOp::Value(v) => self.0[left] = ExpressionOp::Value(((v as i64) >= 0) as u64),
            _ => {
                self.0.push(ExpressionOp::Value(value));
                self.0
                    .push(ExpressionOp::GreaterOrEquals(left, left + 1, sgn))
            }
        }
    }

    /// Create a logical NOT of this expression.
    ///
    /// For complex expressions, attempts to simplify by inverting
    /// comparison operations when possible. TODO: Evaluate constant values.
    ///
    /// # Examples of optimizations:
    /// - `!(A == B)` becomes `A != B`
    /// - `!(A < B)` becomes `A >= B`
    pub fn not(&mut self) {
        let pos = self.get_entry_point();
        match self.0[pos] {
            ExpressionOp::Equals(l, r, sgn) => {
                self.0[pos] = ExpressionOp::NotEquals(l, r, sgn);
            }
            ExpressionOp::Greater(l, r, sgn) => {
                self.0[pos] = ExpressionOp::LessOrEquals(l, r, sgn);
            }
            ExpressionOp::GreaterOrEquals(l, r, sgn) => {
                self.0[pos] = ExpressionOp::Less(l, r, sgn);
            }
            ExpressionOp::Less(l, r, sgn) => {
                self.0[pos] = ExpressionOp::GreaterOrEquals(l, r, sgn);
            }
            ExpressionOp::LessOrEquals(l, r, sgn) => {
                self.0[pos] = ExpressionOp::Greater(l, r, sgn);
            }
            ExpressionOp::NotEquals(l, r, sgn) => {
                self.0[pos] = ExpressionOp::Equals(l, r, sgn);
            }

            _ => self.0.push(ExpressionOp::Not(pos)),
        }
    }

    fn recursive_print(
        &self,
        idx: OpIdx,
        f: &mut std::fmt::Formatter<'_>,
        lang: Option<&SleighLanguage>,
    ) -> std::fmt::Result {
        let my_p = self.get_precesense(idx);
        let is_draw_paren = self.has_higher_precedence(idx, my_p);
        if is_draw_paren {
            f.write_str("(")?;
        }
        match &self.0[idx] {
            ExpressionOp::Variable(variable) => variable.display_fmt(lang, f),
            ExpressionOp::DestinationRegister(register) => {
                if let Some(name) = lang.and_then(|s| s.sleigh.name_of_varnode(*register)) {
                    f.write_str(name)
                } else {
                    f.write_fmt(format_args!("{register:?}"))
                }
            }
            ExpressionOp::Value(v) => {
                if *v > 0xffff {
                    f.write_fmt(format_args!("0x{v:x}"))
                } else {
                    f.write_fmt(format_args!("{v}"))
                }
            }
            ExpressionOp::Dereference(idx) => {
                f.write_str("[")?;
                self.recursive_print(*idx, f, lang)?;
                f.write_str("]")
            }
            ExpressionOp::Overflow(idx, _) => {
                f.write_str("overflow(")?;
                self.recursive_print(*idx, f, lang)?;
                f.write_str(")")
            }
            ExpressionOp::CountOnes(idx) => {
                f.write_str("count1(")?;
                self.recursive_print(*idx, f, lang)?;
                f.write_str(")")
            }
            ExpressionOp::Assign(l_idx, r_idx) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" := ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::Multiequals(l_idx, r_idx) => {
                f.write_str("(")?;
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(") OR (")?;
                self.recursive_print(*r_idx, f, lang)?;
                f.write_str(")")
            }
            ExpressionOp::Add(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" + ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::Xor(l_idx, r_idx) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" ^ ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::Not(l_idx) => {
                f.write_str("~")?;
                self.recursive_print(*l_idx, f, lang)
            }
            ExpressionOp::Sub(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" - ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::Multiply(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" * ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::LessOrEquals(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" <= ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::Less(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" < ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::GreaterOrEquals(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" >= ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::Greater(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" > ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::Equals(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" == ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::NotEquals(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" != ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::BitShiftLeft(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" << ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::BitShiftRight(l_idx, r_idx, _) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" >> ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::And(l_idx, r_idx) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" & ")?;
                self.recursive_print(*r_idx, f, lang)
            }
            ExpressionOp::Or(l_idx, r_idx) => {
                self.recursive_print(*l_idx, f, lang)?;
                f.write_str(" | ")?;
                self.recursive_print(*r_idx, f, lang)
            }
        }?;
        if is_draw_paren {
            f.write_str(")")
        } else {
            Ok(())
        }
    }

    fn get_precesense(&self, pos: OpIdx) -> u8 {
        match &self.0[pos] {
            ExpressionOp::DestinationRegister(_)
            | ExpressionOp::Value(_)
            | ExpressionOp::Overflow(_, _)
            | ExpressionOp::CountOnes(_)
            | ExpressionOp::Assign(_, _)
            | ExpressionOp::Dereference(_)
            | ExpressionOp::Variable(_) => 0,
            ExpressionOp::Multiequals(_, _) => 10,
            ExpressionOp::Add(_, _, _) | ExpressionOp::Sub(_, _, _) => 1,
            ExpressionOp::Multiply(_, _, _) => 2,
            ExpressionOp::Less(_, _, _)
            | ExpressionOp::GreaterOrEquals(_, _, _)
            | ExpressionOp::Greater(_, _, _)
            | ExpressionOp::Equals(_, _, _)
            | ExpressionOp::NotEquals(_, _, _)
            | ExpressionOp::BitShiftRight(_, _, _)
            | ExpressionOp::BitShiftLeft(_, _, _)
            | ExpressionOp::LessOrEquals(_, _, _) => 3,
            ExpressionOp::Or(_, _)
            | ExpressionOp::And(_, _)
            | ExpressionOp::Not(_)
            | ExpressionOp::Xor(_, _) => 4,
        }
    }

    fn has_higher_precedence(&self, start: OpIdx, p: u8) -> bool {
        let my_p = self.get_precesense(start);
        if p > my_p {
            true
        } else {
            match &self.0[start] {
                ExpressionOp::Multiequals(_, _) => true,
                ExpressionOp::DestinationRegister(_)
                | ExpressionOp::Value(_)
                | ExpressionOp::Overflow(_, _)
                | ExpressionOp::CountOnes(_)
                | ExpressionOp::Assign(_, _)
                | ExpressionOp::Dereference(_)
                | ExpressionOp::Variable(_) => false,
                ExpressionOp::Not(l) => self.has_higher_precedence(*l, my_p),
                ExpressionOp::Add(l, r, _)
                | ExpressionOp::Multiply(l, r, _)
                | ExpressionOp::Less(l, r, _)
                | ExpressionOp::GreaterOrEquals(l, r, _)
                | ExpressionOp::Greater(l, r, _)
                | ExpressionOp::Equals(l, r, _)
                | ExpressionOp::NotEquals(l, r, _)
                | ExpressionOp::BitShiftRight(l, r, _)
                | ExpressionOp::BitShiftLeft(l, r, _)
                | ExpressionOp::LessOrEquals(l, r, _)
                | ExpressionOp::Or(l, r)
                | ExpressionOp::And(l, r)
                | ExpressionOp::Xor(l, r)
                | ExpressionOp::Sub(l, r, _) => {
                    self.has_higher_precedence(*l, my_p) || self.has_higher_precedence(*r, my_p)
                }
            }
        }
    }

    /// Given a set of tail instructions after substituting a variable - append this tail to
    /// `self.0` while patching pointers to point to the right new values
    fn copy_other(&mut self, new_pos: usize, ignore_under: usize, other: &[ExpressionOp]) {
        use ExpressionOp::*;
        // helper function to calculate new pointer position
        // if the pointer is between `[0; ignore_under]` - it's unchanged, because the expression hasn't been touched there.
        // otherwise the pointer is past the replaced variable and needs to be changed.
        let s = |p: &usize| {
            if *p >= ignore_under {
                p - ignore_under + new_pos
            } else {
                *p
            }
        };
        for op in other {
            self.0.push(match op {
                Dereference(p) => Dereference(s(p)),
                Overflow(p, sgn) => Overflow(s(p), *sgn),
                CountOnes(p) => CountOnes(s(p)),
                Assign(l, r) => Assign(s(l), s(r)),
                Multiequals(l, r) => Multiequals(s(l), s(r)),
                Add(l, r, size) => Add(s(l), s(r), *size),
                Sub(l, r, size) => Sub(s(l), s(r), *size),
                Multiply(l, r, size) => Multiply(s(l), s(r), *size),
                LessOrEquals(l, r, sgn) => LessOrEquals(s(l), s(r), *sgn),
                Less(l, r, sgn) => Less(s(l), s(r), *sgn),
                GreaterOrEquals(l, r, sgn) => GreaterOrEquals(s(l), s(r), *sgn),
                Greater(l, r, sgn) => Greater(s(l), s(r), *sgn),
                Equals(l, r, sgn) => Equals(s(l), s(r), *sgn),
                NotEquals(l, r, sgn) => NotEquals(s(l), s(r), *sgn),
                BitShiftLeft(l, r, size) => BitShiftLeft(s(l), s(r), *size),
                BitShiftRight(l, r, size) => BitShiftRight(s(l), s(r), *size),
                And(l, r) => And(s(l), s(r)),
                Or(l, r) => Or(s(l), s(r)),
                Not(l) => Not(s(l)),
                Xor(l, r) => Xor(s(l), s(r)),
                a @ Variable(_) | a @ DestinationRegister(_) | a @ Value(_) => a.clone(),
            })
        }
    }

    /// Get the type of the root operation in this expression.
    ///
    /// This returns a reference to the last (root) operation in the expression,
    /// which represents the final instruction type of the entire expression tree.
    ///
    /// # Panics
    /// Panics if the expression is empty.
    pub fn root_kind(&self) -> &ExpressionOp {
        self.0.last().unwrap()
    }

    fn copy_other_to_end(&mut self, other: &[ExpressionOp]) {
        self.copy_other(self.0.len(), 0, other);
    }

    /// Iterate over all symbolic variables used in this expression.
    ///
    /// This provides an iterator over all `VariableSymbol`s contained within
    /// the expression tree, including registers, memory locations, and call results.
    /// This is useful for dependency analysis and variable tracking during decompilation.
    ///
    /// # Returns
    /// An iterator yielding references to all `VariableSymbol`s in the expression
    pub fn iter_vars<'a>(&'a self) -> impl Iterator<Item = &VariableSymbol> + 'a {
        self.0.iter().filter_map(|p| {
            if let ExpressionOp::Variable(v) = p {
                Some(v)
            } else {
                None
            }
        })
    }

    /// Extract the constant value from this expression.
    ///
    /// This method assumes the expression is a simple constant value and extracts it.
    /// Used when analysis has determined that an expression evaluates to a known constant.
    ///
    /// # Panics
    /// Panics if the first operation is not a `Value` variant.
    ///
    /// # Returns
    /// The constant integer value stored in this expression
    pub fn get_value(&self) -> u64 {
        match &self.0[0] {
            ExpressionOp::Value(v) => *v,
            _ => panic!("Exptected a value, got {self:?}"),
        }
    }

    /// Check if this expression contains any symbolic variables.
    ///
    /// Returns `true` if the expression contains any `VariableSymbol`s (registers,
    /// memory locations, or call results), indicating that the expression's value
    /// is not fully known at analysis time. Returns `false` if the expression
    /// consists only of constants and operations on constants.
    ///
    /// This is useful for determining whether further symbolic execution or
    /// constraint solving may be needed to evaluate the expression.
    ///
    /// # Returns
    /// `true` if the expression contains variables, `false` if it's purely concrete
    pub fn is_symbolic(&self) -> bool {
        self.0
            .iter()
            .find(|p| matches!(p, ExpressionOp::Variable(_)))
            .is_some()
    }

    /// Replace a variable at a specific index with another expression.
    ///
    /// This performs substitution of a symbolic variable with a concrete expression,
    /// which is fundamental for symbolic execution and expression simplification.
    /// The method handles the complex task of updating all operation indices after
    /// the substitution and applies algebraic optimizations where possible.
    ///
    /// # Arguments
    /// * `var_index` - Index of the variable operation to replace
    /// * `expr` - The expression to substitute in place of the variable
    ///
    /// # Returns
    /// Number of new operations added to the expression (can be negative if optimizations remove operations)
    ///
    /// # Panics
    /// Panics if the operation at `var_index` is not a `Variable` variant.
    pub fn replace_variable_with_expression(&mut self, var_index: OpIdx, expr: &Expression) -> i32 {
        assert!(matches!(self.0[var_index], ExpressionOp::Variable(_)));
        if expr == self {
            return 0;
        }
        // split the instruction vector
        let shift = var_index + (expr.0.len() - 1);
        let remainder: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> =
            self.0[var_index..].iter().cloned().collect();
        self.0.truncate(var_index);
        // copy new instructions instead of the variable
        self.copy_other_to_end(&expr.0);
        // fix old instruction pointers
        if true {
            // is flattening expressions or not?
            let saved = self.apply(shift as i32, var_index, &remainder[1..]);
            expr.0.len() as i32 - 1 + saved
        } else {
            // If we just do self.copy_other we will crate correct expression, but it'll be expanded - with lots of + and - ops that can be simplified.
            self.copy_other(shift, var_index, &remainder[1..]);
            (expr.0.len() - 1) as i32
        }
    }

    /// Replace variables in this expression using a custom replacement function.
    ///
    /// This method scans the expression for variables and applies the provided
    /// replacement function to each one. If the function returns `Some(expression)`,
    /// that variable is replaced with the given expression. This enables flexible
    /// symbolic execution and variable substitution patterns.
    ///
    /// The replacement is performed in-place and handles index updates automatically.
    /// Algebraic optimizations are applied during replacement when possible.
    ///
    /// # Arguments
    /// * `replace` - Function that maps variables to optional replacement expressions
    ///
    /// # Examples
    /// ```ignore
    /// // Replace all ESP variables with ESP + offset
    /// expr.replace_variable_with(|var| match var {
    ///     VariableSymbol::Varnode(v) if v == esp_varnode => {
    ///         let mut new_expr = Expression::from(*var);
    ///         new_expr.add_value(offset, InstructionSize::U32);
    ///         Some(Cow::Owned(new_expr))
    ///     }
    ///     _ => None
    /// });
    /// ```
    pub fn replace_variable_with<'r, F>(&mut self, replace: F)
    where
        F: Fn(&VariableSymbol) -> Option<Cow<'r, Expression>>,
    {
        // let mut extended_by = 0;

        let mut pos = 0;
        while pos < self.0.len() {
            if let ExpressionOp::Variable(v) = &self.0[pos] {
                if let Some(expr) = replace(v) {
                    let r = self.replace_variable_with_expression(pos, &expr);
                    if r > 0 {
                        // it is possible we remove more instructions than add in case of nop-algebra.
                        // in that case position of current variable stays the same.
                        pos = (pos as i32 + r) as usize;
                    }
                }
            }
            pos += 1;
        }
    }

    /// Create a new expression with a specific varnode assumed to have a constant value.
    ///
    /// This is a specialized form of variable substitution used for "what-if" analysis
    /// during decompilation. It creates a copy of this expression where all occurrences
    /// of the specified varnode are replaced with the given constant value.
    ///
    /// This is particularly useful for analyzing conditional branches and understanding
    /// how expressions simplify under specific assumptions about register or memory values.
    ///
    /// # Arguments
    /// * `var_node` - The varnode (typically a register) to assume a value for
    /// * `value` - The constant value to assume for the varnode
    ///
    /// # Returns
    /// A new expression with the assumption applied and optimizations performed
    ///
    /// # Examples
    /// ```ignore
    /// // If we assume ESP is 0 and expr.assume() returns a positive value, this is likely a function argument
    /// let simplified = expr.assume(esp_varnode, 0);
    /// if !simplified.is_symbolic() && simplified.get_value() > 0 {
    ///     // this is a function argument address
    /// }
    /// ```
    pub fn assume(&self, var_node: VarNode, value: u64) -> Expression {
        let mut result = self.clone();
        result.replace_variable_with(|v| match v {
            VariableSymbol::Varnode(v) => {
                if *v == var_node {
                    Some(Cow::Owned(Expression::from(value)))
                } else {
                    None
                }
            }
            _ => None,
        });
        result
    }

    fn decrement_offsets(&mut self, from: usize) {
        for op in &mut self.0[from..] {
            match op {
                ExpressionOp::Dereference(l)
                | ExpressionOp::Overflow(l, _)
                | ExpressionOp::Not(l)
                | ExpressionOp::CountOnes(l) => {
                    if *l >= from {
                        *l -= 1
                    }
                }
                ExpressionOp::Assign(l, r)
                | ExpressionOp::Multiequals(l, r)
                | ExpressionOp::Add(l, r, _)
                | ExpressionOp::Sub(l, r, _)
                | ExpressionOp::Multiply(l, r, _)
                | ExpressionOp::LessOrEquals(l, r, _)
                | ExpressionOp::Less(l, r, _)
                | ExpressionOp::GreaterOrEquals(l, r, _)
                | ExpressionOp::Greater(l, r, _)
                | ExpressionOp::Equals(l, r, _)
                | ExpressionOp::BitShiftLeft(l, r, _)
                | ExpressionOp::BitShiftRight(l, r, _)
                | ExpressionOp::And(l, r)
                | ExpressionOp::Or(l, r)
                | ExpressionOp::Xor(l, r)
                | ExpressionOp::NotEquals(l, r, _) => {
                    if *l >= from {
                        *l -= 1;
                    }
                    if *r >= from {
                        *r -= 1
                    }
                }
                ExpressionOp::Variable(_)
                | ExpressionOp::DestinationRegister(_)
                | ExpressionOp::Value(_) => (),
            }
        }
    }

    fn replace_offsets(&mut self, from: usize, original: usize, new: usize) {
        for op in &mut self.0[from..] {
            match op {
                ExpressionOp::Dereference(l)
                | ExpressionOp::Overflow(l, _)
                | ExpressionOp::Not(l)
                | ExpressionOp::CountOnes(l) => {
                    if *l == original {
                        *l = new
                    }
                }
                ExpressionOp::Assign(l, r)
                | ExpressionOp::Multiequals(l, r)
                | ExpressionOp::Add(l, r, _)
                | ExpressionOp::Sub(l, r, _)
                | ExpressionOp::Multiply(l, r, _)
                | ExpressionOp::LessOrEquals(l, r, _)
                | ExpressionOp::Less(l, r, _)
                | ExpressionOp::GreaterOrEquals(l, r, _)
                | ExpressionOp::Greater(l, r, _)
                | ExpressionOp::Equals(l, r, _)
                | ExpressionOp::BitShiftLeft(l, r, _)
                | ExpressionOp::BitShiftRight(l, r, _)
                | ExpressionOp::And(l, r)
                | ExpressionOp::Or(l, r)
                | ExpressionOp::Xor(l, r)
                | ExpressionOp::NotEquals(l, r, _) => {
                    if *l == original {
                        *l = new;
                    }
                    if *r == original {
                        *r = new;
                    }
                }
                ExpressionOp::Variable(_)
                | ExpressionOp::DestinationRegister(_)
                | ExpressionOp::Value(_) => (),
            }
        }
    }

    fn remove_nop_algebra(&mut self, mut idx: OpIdx) -> i32 {
        let mut left_over = None;
        match &self.0[idx] {
            &ExpressionOp::Add(l, r, _) | &ExpressionOp::Sub(l, r, _) => {
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
            }
            &ExpressionOp::Multiply(l, r, _) => {
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
            }
            _ => (),
        }

        if let Some(left_over) = left_over {
            self.0.remove(idx);
            self.decrement_offsets(idx);
            self.replace_offsets(idx, idx - 1, left_over);
            -2
        } else {
            0
        }
    }

    /// Given the arguments, of an old add/mul/sub instruction, this function modifies the expression
    /// such that immediate values are added/multiplied/subtracted before adding new add/mul/sub instructions.
    ///
    /// Returns how many instructions have been added to the list. This value is likely to be `-2` (removed 2 instructions) or `0` (didn't remove any)
    fn patch(
        &mut self,
        l: usize,
        r: usize,
        new_pos: i32,
        ignore_under: usize,
        patch_kind: PatchKind,
        size: InstructionSize,
    ) -> i32 {
        /// helper function to calculate new pointer position
        /// if the pointer is between `[0; ignore_under]` - it's unchanged, because the expression hasn't been touched there.
        /// otherwise the pointer is past the replaced variable and needs to be changed.
        fn s(p: usize, ignore_under: usize, new_pos: i32) -> usize {
            if p >= ignore_under {
                ((p - ignore_under) as i32 + new_pos) as usize
            } else {
                p
            }
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
                PatchKind::Add => self.add_value_at(patched_r, left, size),
                PatchKind::Sub => self.sub_value_at(patched_r, left, true, size),
                PatchKind::Mul => self.multiply_value_at(patched_r, left, size),
            } as i32
                - 2; // overall we removed one instruction from the list, and saved space by not adding a value, therefore, remove 2 from new_pos
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
                PatchKind::Add => self.add_value_at(patched_l, right, size),
                PatchKind::Sub => self.sub_value_at(patched_l, right, false, size),
                PatchKind::Mul => self.multiply_value_at(patched_l, right, size),
            } as i32
                - 2;
            r += self.remove_nop_algebra(patched_l);
            r
        } else {
            self.0.push(match patch_kind {
                PatchKind::Add => ExpressionOp::Add(patched_l, patched_r, size),
                PatchKind::Sub => ExpressionOp::Sub(patched_l, patched_r, size),
                PatchKind::Mul => ExpressionOp::Multiply(patched_l, patched_r, size),
            });
            0
        }
    }

    fn apply(&mut self, mut new_pos: i32, ignore_under: usize, other: &[ExpressionOp]) -> i32 {
        use ExpressionOp::*;
        fn s(p: &usize, ignore_under: usize, new_pos: i32) -> usize {
            if *p >= ignore_under {
                ((*p - ignore_under) as i32 + new_pos) as usize
            } else {
                *p
            }
        }
        let mut total_saved = 0;
        for op in other {
            match op {
                Dereference(p) => self.0.push(Dereference(s(p, ignore_under, new_pos))),
                Overflow(p, sgn) => self.0.push(Overflow(s(p, ignore_under, new_pos), *sgn)),
                CountOnes(p) => self.0.push(CountOnes(s(p, ignore_under, new_pos))),
                Assign(l, r) => self.0.push(Assign(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                )),
                Multiequals(l, r) => self.0.push(Multiequals(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                )),
                Add(l, r, size) => {
                    let saved = self.patch(*l, *r, new_pos, ignore_under, PatchKind::Add, *size);
                    total_saved += saved;
                    new_pos = new_pos as i32 + saved;
                }
                Sub(l, r, size) => {
                    let saved = self.patch(*l, *r, new_pos, ignore_under, PatchKind::Sub, *size);
                    total_saved += saved;
                    new_pos = new_pos as i32 + saved;
                }
                Multiply(l, r, size) => {
                    let saved = self.patch(*l, *r, new_pos, ignore_under, PatchKind::Mul, *size);
                    total_saved += saved;
                    new_pos = new_pos as i32 + saved;
                }
                LessOrEquals(l, r, sgn) => self.0.push(LessOrEquals(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                    *sgn,
                )),
                Less(l, r, sgn) => self.0.push(Less(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                    *sgn,
                )),
                GreaterOrEquals(l, r, sgn) => self.0.push(GreaterOrEquals(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                    *sgn,
                )),
                Greater(l, r, sgn) => self.0.push(Greater(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                    *sgn,
                )),
                Equals(l, r, sgn) => self.0.push(Equals(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                    *sgn,
                )),
                NotEquals(l, r, sgn) => self.0.push(NotEquals(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                    *sgn,
                )),
                BitShiftLeft(l, r, size) => self.0.push(BitShiftLeft(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                    *size,
                )),
                BitShiftRight(l, r, size) => self.0.push(BitShiftRight(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                    *size,
                )),
                And(l, r) => self.0.push(And(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                )),
                Xor(l, r) => self.0.push(Xor(
                    s(l, ignore_under, new_pos),
                    s(r, ignore_under, new_pos),
                )),
                Not(l) => self.0.push(Not(s(l, ignore_under, new_pos))),
                Or(l, r) => self
                    .0
                    .push(Or(s(l, ignore_under, new_pos), s(r, ignore_under, new_pos))),
                a @ Variable(_) | a @ DestinationRegister(_) | a @ Value(_) => {
                    self.0.push(a.clone())
                }
            }
        }
        total_saved
    }

    /// Get the index of the root operation in this expression.
    ///
    /// The entry point is always the last element in the operations vector,
    /// representing the final operation of the expression tree. All other operations
    /// are intermediate calculations (to be done first).
    ///
    /// # Returns
    /// Index of the root operation
    ///
    /// Panics if the expression is empty
    pub fn get_entry_point(&self) -> OpIdx {
        return self.0.len() - 1;
    }

    /// Create a wrapper for pretty-printing this expression with SLEIGH language context.
    ///
    /// The returned wrapper can be used with `Display` and `Debug` traits to format
    /// the expression with proper register names and architecture-specific information
    /// from the SLEIGH processor specification.
    ///
    /// # Arguments
    /// * `sleigh` - The SLEIGH language definition for formatting context
    ///
    /// # Returns
    /// A wrapper that implements `Display` and `Debug` with SLEIGH-aware formatting
    pub fn with_sleigh_language<'e>(
        &'e self,
        sleigh: &'e SleighLanguage,
    ) -> WithSleighLanguage<'e, Self> {
        WithSleighLanguage { data: self, sleigh }
    }
}

/// Internal enum for algebraic optimization during expression patching.
///
/// Used by the optimization engine to track what kind of algebraic operation
/// is being performed when combining expressions, enabling appropriate
/// simplification rules to be applied.
#[derive(Debug)]
enum PatchKind {
    /// Addition operation for optimization
    Add,
    /// Subtraction operation for optimization
    Sub,
    /// Multiplication operation for optimization
    Mul,
}

impl ExpressionOp {
    /// Create a variable operation for a register (varnode).
    ///
    /// This is a convenience constructor for creating `ExpressionOp::Variable`
    /// operations that represent CPU registers or other varnodes.
    ///
    /// # Arguments
    /// * `var_node` - The SLEIGH varnode representing the register
    ///
    /// # Returns
    /// An `ExpressionOp::Variable` containing the varnode
    pub fn var_reg(var_node: VarNode) -> Self {
        Self::Variable(VariableSymbol::Varnode(var_node))
    }
}

impl FormatWithSleighLanguage for Expression {
    fn display_fmt(
        &self,
        lang: Option<&SleighLanguage>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        self.recursive_print(self.get_entry_point(), f, lang)
    }

    fn debug_fmt(
        &self,
        lang: Option<&SleighLanguage>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.write_str("[")?;
        for op in &self.0 {
            if let ExpressionOp::Variable(v) = op {
                v.debug_fmt(lang, f)?;
            } else {
                f.write_fmt(format_args!("{op:?}"))?;
            }
        }
        f.write_str("]")
    }
}

/// A wrapper struct for formatting expressions with SLEIGH language context.
///
/// This struct holds a reference to data (typically an Expression or VariableSymbol)
/// along with SLEIGH language information, enabling pretty-printing with proper
/// register names and architecture-specific formatting.
///
/// Created by calling `with_sleigh_language()` on expressions or variables.
///
/// # Type Parameters
/// * `T` - The type being wrapped (must implement `FormatWithSleighLanguage`)
/// `T` will likely contain [`pcode::VarNode`] to need this wrapper.
///
///
/// # Examples
/// ```ignore
/// let sleigh_lang = sleigh_compile::SleighLanguageBuilder::new(
///     "./Ghidra/Processors/x86/data/languages/x86.ldefs",
///     "x86:LE:32:default",
/// )
/// .build().unwrap();
/// let formatted = expression.with_sleigh_language(&sleigh_lang);
/// println!("{}", formatted); // Prints with proper register names
/// ```
pub struct WithSleighLanguage<'e, T> {
    /// Reference to the data being formatted
    data: &'e T,
    /// Reference to the SLEIGH language definition for formatting context
    sleigh: &'e SleighLanguage,
}

impl<'e, T> std::fmt::Display for WithSleighLanguage<'e, T>
where
    T: FormatWithSleighLanguage,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.display_fmt(Some(self.sleigh), f)
    }
}

impl<'e, T> std::fmt::Debug for WithSleighLanguage<'e, T>
where
    T: FormatWithSleighLanguage,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.debug_fmt(Some(self.sleigh), f)
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.len() > 0 {
            self.recursive_print(self.get_entry_point(), f, None)
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
    use pcode::VarNode;
    use smallvec::{smallvec, SmallVec};

    use super::{Expression, ExpressionOp, InstructionSize::U32, VariableSymbol, SMALLVEC_SIZE};

    #[inline]
    fn var_reg(r: VarNode) -> ExpressionOp {
        ExpressionOp::Variable(VariableSymbol::Varnode(r))
    }

    #[inline]
    fn mk_eax() -> VarNode {
        VarNode {
            id: 1,
            offset: 0,
            size: 4,
        }
    }

    #[inline]
    fn mk_esp() -> VarNode {
        VarNode {
            id: 1,
            offset: 4,
            size: 4,
        }
    }

    #[inline]
    fn mk_ebp() -> VarNode {
        VarNode {
            id: 1,
            offset: 8,
            size: 4,
        }
    }

    #[inline]
    fn mk_edi() -> VarNode {
        VarNode {
            id: 1,
            offset: 12,
            size: 4,
        }
    }

    #[test]
    fn test_multiply() {
        let mut e = Expression::from(1);
        e.add(&Expression::from(VariableSymbol::Varnode(mk_eax())), U32);
        e.multiply_value(4, U32);
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            var_reg(mk_eax()),
            ExpressionOp::Value(1),
            ExpressionOp::Add(0, 1, U32),
            ExpressionOp::Value(4),
            ExpressionOp::Multiply(2, 3, U32),
        ];
        assert_eq!(e.0, result);
    }

    #[test]
    fn test_variable_substitution() {
        use ExpressionOp::{Add, Multiply, Value, Variable as VarOp};
        let mut e = Expression::from(1);
        e.add(&Expression::from(VariableSymbol::Varnode(mk_eax())), U32);
        e.multiply_value(4, U32); // e = (1 + ?EAX) * 4

        let mut sub = Expression::from(1);
        sub.add(&Expression::from(VariableSymbol::Varnode(mk_esp())), U32);
        sub.multiply_value(2, U32); // sub = (1+?ESP) * 2

        e.replace_variable_with_expression(0, &sub);

        // This block is correct if we are NOT using expression flattening
        // assert_eq!(e.0, vec![
        //     // (1 + (1 + ?ESP) * 2 ) * 4
        //     Value(1),
        //     Value(1),
        //     VarOp(Variable::Register(mk_esp())),
        //     Add(1, 2),
        //     Value(2),
        //     Multiply(3, 4),
        //     Add(0, 5),
        //     Value(4),
        //     Multiply(6, 7)
        // ]);

        // this block is correct if we are using expression flatening
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            // (1 + (1 + ?ESP) * 2 ) * 4
            VarOp(VariableSymbol::Varnode(mk_esp())),
            Value(1),
            Add(0, 1, U32),
            Value(2),
            Multiply(2, 3, U32),
            Value(1),
            Add(4, 5, U32),
            Value(4),
            Multiply(6, 7, U32)
        ];
        assert_eq!(e.0, result);
    }

    #[test]
    fn test_replace() {
        use ExpressionOp::{Add, Dereference, Sub, Value, Variable as VarOp};
        let mut first = Expression::from(VariableSymbol::Varnode(mk_esp()));
        first.add_value(180, U32);
        first.dereference();
        // first = [?ESP + 180]

        let mut second = Expression::from(VariableSymbol::Varnode(mk_esp()));
        second.sub_value(12, U32);
        second.dereference();
        // second = [?ESP - 12]

        second.replace_variable_with_expression(0, &first);
        // second = [[?ESP - 12] + 180]
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            VarOp(VariableSymbol::Varnode(mk_esp())),
            Value(180),
            Add(0, 1, U32),
            Dereference(2),
            Value(12),
            Sub(3, 4, U32),
            Dereference(5)
        ];
        assert_eq!(second.0, result);
    }

    #[test]
    fn test_replace_ptr_to_var() {
        use ExpressionOp::{Add, Dereference, Value, Variable as VarOp};
        let mut e = Expression::new();
        e.0 = smallvec![
            Value(6721424),
            Dereference(0),
            ExpressionOp::Variable(VariableSymbol::Varnode(mk_eax())),
            Add(1, 2, U32)
        ];
        // e = [668f90] := ?EAX

        let mut eax = Expression::new();
        eax.0 = smallvec![
            VarOp(VariableSymbol::Varnode(mk_ebp())),
            Value(100),
            Add(0, 1, U32),
            Dereference(2)
        ]; // eax = [?EBP + 100]
        e.replace_variable_with_expression(2, &eax);
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            Value(6721424),
            Dereference(0),
            VarOp(VariableSymbol::Varnode(mk_ebp())),
            Value(100),
            Add(2, 3, U32),
            Dereference(4),
            Add(1, 5, U32)
        ];
        assert_eq!(e.0, result)
    }

    #[test]
    fn test_replace_complex() {
        use ExpressionOp::{Add, Dereference, Value, Variable as VarOp};
        let mut ptr = Expression::new();
        ptr.0 = smallvec![
            VarOp(VariableSymbol::Varnode(mk_eax())),
            Value(20),
            Add(0, 1, U32),
            Dereference(2)
        ];

        // e = [?EAX + 20] := ?data@[?EAX + 20] + 1
        let mut e = Expression::new();
        e.0 = smallvec![
            VarOp(VariableSymbol::Varnode(mk_eax())),
            Value(20),
            Add(0, 1, U32),
            Dereference(2),
            VarOp(VariableSymbol::Ram(Box::new(ptr), 4)),
            Value(1),
            Add(4, 5, U32),
            Add(3, 6, U32)
        ]; // e = [?EAX + 20] := ?data@[?EAX + 20] + 1

        let val_expr = crate::ir::basic_block::DestinationKind::Concrete(4917232_u64.into());
        let call_result = ExpressionOp::Variable(VariableSymbol::CallResult {
            call_from: crate::ir::Address::NULL,
            call_to: Box::new(val_expr),
        });
        let mut eax = Expression::new();
        eax.0 = smallvec![call_result.clone()]; // eax = ?call_4b07f0_result

        e.replace_variable_with(|old_e| match old_e {
            VariableSymbol::Varnode(r) => {
                if *r == mk_eax() {
                    Some(std::borrow::Cow::Borrowed(&eax))
                } else {
                    None
                }
            }
            VariableSymbol::Ram(d, size) => {
                let mut d = d.clone();
                d.replace_variable_with_expression(0, &eax);
                Some(std::borrow::Cow::Owned(Expression::from(
                    VariableSymbol::Ram(d, *size),
                )))
            }
            _ => panic!("No such variables should be here"),
        });

        let mut new_ptr = Expression::new();
        new_ptr.0 = smallvec![
            call_result.clone(),
            Value(20),
            Add(0, 1, U32),
            Dereference(2)
        ];
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            call_result.clone(),
            Value(20),
            Add(0, 1, U32),
            Dereference(2),
            VarOp(VariableSymbol::Ram(Box::new(Expression::from(new_ptr)), 4)),
            Value(1),
            Add(4, 5, U32),
            Add(3, 6, U32)
        ];
        assert_eq!(e.0, result);
    }

    #[test]
    fn test_replace_flatten() {
        use ExpressionOp::{Add, Dereference, Sub, Value, Variable as VarOp};
        let mut first = Expression::from(VariableSymbol::Varnode(mk_esp()));
        first.add_value(180, U32);
        // first = ?ESP + 180

        let mut second = Expression::from(VariableSymbol::Varnode(mk_esp()));
        second.sub_value(12, U32);
        second.dereference();
        // second = [?ESP - 12]

        second.replace_variable_with_expression(0, &first);
        // second = [?ESP +180 - 12]
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            VarOp(VariableSymbol::Varnode(mk_esp())),
            Value(168),
            Add(0, 1, U32),
            Dereference(2)
        ];
        assert_eq!(second.0, result);
    }

    #[test]
    fn test_sub_at_end() {
        use ExpressionOp::{Add, Dereference, Sub, Value};
        let mut e = Expression::from(smallvec![var_reg(mk_eax()), Value(10), Sub(0, 1, U32)]);
        e.sub_value_at(2, 9, false, U32);
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> =
            smallvec![var_reg(mk_eax()), Value(19), Sub(0, 1, U32)];
        assert_eq!(e.0, result)
    }

    #[test]
    fn test_sub_at_end_inverted() {
        use ExpressionOp::{Add, Dereference, Sub, Value};
        let mut e = Expression::from(smallvec![var_reg(mk_eax()), Value(10), Sub(1, 0, U32)]);
        e.sub_value_at(2, 9, false, U32);
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> =
            smallvec![var_reg(mk_eax()), Value(1), Sub(1, 0, U32)];
        assert_eq!(e.0, result);

        let mut e = Expression::from(smallvec![var_reg(mk_eax()), Value(10), Sub(1, 0, U32)]);
        e.sub_value_at(2, 9, true, U32); // Doesn't matter because on-stack Sub() operation gets openned anyway.
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> =
            smallvec![var_reg(mk_eax()), Value(1), Sub(1, 0, U32)];
        assert_eq!(e.0, result);

        let mut e = Expression::from(smallvec![
            var_reg(mk_eax()),
            Value(10),
            Sub(1, 0, U32),
            Dereference(2)
        ]);
        e.sub_value_at(3, 9, false, U32);
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            var_reg(mk_eax()),
            Value(10),
            Sub(1, 0, U32),
            Dereference(2),
            Value(9),
            Sub(3, 4, U32)
        ];
        assert_eq!(e.0, result);

        let mut e = Expression::from(smallvec![
            var_reg(mk_eax()),
            Value(10),
            Sub(1, 0, U32),
            Dereference(2)
        ]);
        e.sub_value_at(3, 9, true, U32);
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            var_reg(mk_eax()),
            Value(10),
            Sub(1, 0, U32),
            Dereference(2),
            Value(9),
            Sub(4, 3, U32)
        ];
        assert_eq!(e.0, result);
    }

    #[test]
    fn test_replace_var_with_value() {
        use ExpressionOp::{Add, Dereference, Sub, Value};
        let mut expression = Expression::from(smallvec![
            Value(6721484),
            Dereference(0),
            var_reg(mk_edi()),
            Sub(1, 2, U32)
        ]);
        let r = expression.replace_variable_with_expression(2, &Expression::from(0));
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> =
            smallvec![Value(6721484), Dereference(0)];
        assert_eq!(expression.0, result);
        assert_eq!(r, -2);
    }

    #[test]
    fn test_replace_flatten_with_value() {
        use ExpressionOp::{Add, Dereference, Sub, Value};
        let mut expression = Expression::from(smallvec![
            var_reg(mk_esp()), // 0
            Value(116),        // 1
            Add(0, 1, U32),    // 2
            Dereference(2),    // 3
            Value(156),        // 4
            var_reg(mk_esp()), // 5
            Value(76),         // 6
            Add(5, 6, U32),    // 7
            Dereference(7),    // 8
            Sub(4, 8, U32),    // 9
            Add(3, 9, U32)
        ]); // 10

        let sub = Expression::from(smallvec![var_reg(mk_esp()), Value(172), Sub(0, 1, U32)]);

        expression.replace_variable_with_expression(0, &sub);
        let result: SmallVec<[ExpressionOp; SMALLVEC_SIZE]> = smallvec![
            var_reg(mk_esp()),
            Value(56),
            Sub(0, 1, U32),
            Dereference(2),
            var_reg(mk_esp()),
            Value(76),
            Add(4, 5, U32),
            Dereference(6),
            Value(156),
            Sub(8, 7, U32),
            Add(3, 9, U32)
        ];
        assert_eq!(expression.0, result);
    }
}
