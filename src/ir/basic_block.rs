//! During symbolic execution, we need to be able to check if a
//! [`BasicBlock`] represents the same execution logic as a different [`BasicBlock`]
//! This happens, for example, if two jumps land on the same address.
//! Therefore, we need to store metadata about a basic block - what address range it represents,
//! or in case on PcodeBranch operators - what segment of an instruction it represents.
//! Such metadata should be just stored in [`BasicBlock]` itself

use core::hash::Hash;
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::{Index, IndexMut},
    usize,
};

use nodit::{DiscreteFinite, InclusiveInterval, Interval, NoditMap};

// use crate::ir::{high_function::CallingConvention, };
use pcode::VarNode;
use smallvec::SmallVec;

use super::{
    expression::FormatWithSleighLanguage, Address, Expression, ExpressionOp, VariableSymbol,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockSlot(usize);

impl Default for BlockSlot {
    fn default() -> Self {
        Self(usize::MAX)
    }
}

impl BlockSlot {
    pub fn next(self) -> BlockSlot {
        BlockSlot(self.0 + 1)
    }
}

/// Linear storage that keeps track of address coverage.
///
/// *Note:* some blocks will not belong to an address in cases like `rep.movsd` instruction
/// because that instruction alone emits two blocks.
#[derive(Default)]
pub struct BlockStorage {
    blocks: Vec<BasicBlock>,
    address_map: NoditMap<Address, Interval<Address>, BlockSlot>,
    multiblock_addresses: HashMap<Address, SmallVec<[Option<BlockSlot>; 2]>>,
}

impl Index<BlockSlot> for BlockStorage {
    type Output = BasicBlock;

    fn index(&self, index: BlockSlot) -> &Self::Output {
        &self.blocks[index.0]
    }
}

impl IndexMut<BlockSlot> for BlockStorage {
    fn index_mut(&mut self, index: BlockSlot) -> &mut Self::Output {
        &mut self.blocks[index.0]
    }
}

impl BlockStorage {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            address_map: NoditMap::new(),
            multiblock_addresses: HashMap::new(),
        }
    }

    pub fn get(&self, index: BlockSlot) -> Option<&BasicBlock> {
        self.blocks.get(index.0)
    }
    pub fn get_mut(&mut self, index: BlockSlot) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(index.0)
    }

    /// Get block that encompasses a given address
    pub fn get_by_address<A: Into<Address>>(&self, address: A) -> Option<&BasicBlock> {
        let address = address.into();
        self.address_map
            .get_at_point(address)
            .copied()
            .or_else(|| {
                self.multiblock_addresses
                    .get(&address)
                    .and_then(|v| *v.iter().next().unwrap_or(&None))
            })
            .and_then(|idx| self.blocks.get(idx.0))
    }

    pub fn get_by_identifier(&self, id: BlockIdentifier) -> Option<&BasicBlock> {
        self.slot_by_identifier(id).and_then(|idx| Some(&self[idx]))
    }

    /// Get BlockId of a block that encompasses a given address
    pub fn slot_by_address<A: Into<Address>>(&self, address: A) -> Option<BlockSlot> {
        let address = address.into();
        self.address_map.get_at_point(address).copied().or_else(|| {
            *self
                .multiblock_addresses
                .get(&address)
                .and_then(|s| s.first())
                .unwrap_or(&None)
        })
    }

    pub fn slot_by_identifier(&self, id: BlockIdentifier) -> Option<BlockSlot> {
        match id {
            BlockIdentifier::Unset => None,
            BlockIdentifier::Physical(interval) => {
                self.address_map.get_at_point(interval.start()).copied()
            }
            BlockIdentifier::Virtual(address, idx) => *self
                .multiblock_addresses
                .get(&address)
                .and_then(|v| v.get(idx as usize))
                .unwrap_or(&None),
        }
    }

    pub fn slot_by_destination(&self, destionation: &DestinationKind) -> Option<BlockSlot> {
        match destionation {
            DestinationKind::Symbolic(_) => None,
            DestinationKind::Concrete(address) => self
                .address_map
                .get_at_point(*address)
                .copied()
                .or_else(|| {
                    *self
                        .multiblock_addresses
                        .get(address)
                        .and_then(|s| s.first())
                        .unwrap_or(&None)
                }),
            DestinationKind::Virtual(address, idx) => *self
                .multiblock_addresses
                .get(address)
                .and_then(|store| store.get(*idx as usize))
                .unwrap_or(&None),
        }
    }

    /// Get an interval of addresses that encompasses a given address
    pub fn lookup_interval(&self, address: Address) -> Option<Interval<Address>> {
        self.address_map
            .get_key_value_at_point(address)
            .ok()
            .map(|(i, _b)| *i)
    }

    /// How many blocks we have
    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    /// Get the next unused [`BlockId`]
    pub fn next_available_id(&self) -> BlockSlot {
        BlockSlot(self.blocks.len())
    }

    ///
    pub fn next_available_multiblock_id_at_address(&self, address: Address) -> u8 {
        self.multiblock_addresses
            .get(&address)
            .and_then(|s| Some(s.len() as u8))
            .unwrap_or(0)
    }

    /// Insert a [`BasicBlock`] that encompass a given address interval
    /// See [`BasicBlock`] documentation for when an interval might be None.
    ///
    /// Panics if the block identifier not set.
    /// Or if an identifier interval overlaps with previously inserted block.
    /// Or if adding a single-address multiple-blocks identifier to a region covered by a larger block.
    ///
    /// To set block idenfifier you'll likely need to use [`Self::next_available_multiblock_id_at_address`]
    pub fn insert(&mut self, block: BasicBlock) -> BlockSlot {
        let id = self.next_available_id();
        match block.identifier {
            BlockIdentifier::Unset => panic!("Expected block identifier to be set by now."),
            BlockIdentifier::Physical(interval) => {
                if let Err(e) = self.address_map.insert_strict(interval, id) {
                    use std::fmt::Write;
                    let mut overlaps = String::new();
                    for (k, v) in self.address_map.overlapping(interval) {
                        write!(&mut overlaps, "\t{k:?} - {v:?}\n").unwrap();
                    }
                    panic!("tried inserting new block at {interval:?} but got {e:?}:\n{overlaps}")
                }
            }
            BlockIdentifier::Virtual(address, multiblock_idx) => {
                let store = self.multiblock_addresses.entry(address).or_default();
                while store.len() <= multiblock_idx as usize {
                    store.push(None);
                }
                if let Some(other_id) = store[multiblock_idx as usize] {
                    if other_id != id {
                        panic!("Tried inserting additional block at {address:?}:{multiblock_idx}, but it conflicts with previously set block at the same address/idx")
                    }
                } else {
                    store[multiblock_idx as usize] = Some(id);
                }
            }
        }
        self.blocks.push(block);
        id
    }

    /// Iterate over every block in a function, starting from this one.
    pub fn iter_function<'i>(&'i self, start: BlockSlot) -> BlockFunctionIterator<'i> {
        BlockFunctionIterator::new(self, start)
    }

    /// Iterate over this block's direct neighbors.
    pub fn iter_neighbors<'i>(&'i self, start: BlockSlot) -> BlockNeighborsIterator<'i> {
        BlockNeighborsIterator::new(self, start)
    }

    /// Iterate over a single block path, stopping when reaching a fork in control flow graph
    pub fn iter_path<'i>(&'i self, start: BlockSlot) -> BlockPathIterator<'i> {
        BlockPathIterator::new(self, start)
    }

    /// Iterate over all blocks as (BlockSlot, &BasicBlock) pairs
    pub fn iter(&self) -> impl Iterator<Item = (BlockSlot, &BasicBlock)> {
        self.blocks
            .iter()
            .enumerate()
            .map(|(idx, block)| (BlockSlot(idx), block))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DestinationKind {
    Symbolic(Expression),
    Concrete(Address),
    Virtual(Address, u8),
}

impl DestinationKind {
    pub fn get_symbolic_expression(&self) -> Option<&Expression> {
        match self {
            Self::Symbolic(e) => Some(e),
            _ => None,
        }
    }
}

impl FormatWithSleighLanguage for DestinationKind {
    fn display_fmt(
        &self,
        lang: Option<&sleigh_compile::ldef::SleighLanguage>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        if let Self::Symbolic(e) = self {
            e.display_fmt(lang, f)
        } else {
            std::fmt::Display::fmt(self, f)
        }
    }

    fn debug_fmt(
        &self,
        lang: Option<&sleigh_compile::ldef::SleighLanguage>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        if let Self::Symbolic(e) = self {
            e.debug_fmt(lang, f)
        } else {
            std::fmt::Debug::fmt(self, f)
        }
    }
}

impl std::fmt::Display for DestinationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DestinationKind::Symbolic(expression) => std::fmt::Display::fmt(expression, f),
            DestinationKind::Concrete(address) => std::fmt::Display::fmt(address, f),
            DestinationKind::Virtual(addr, idx) => {
                f.write_fmt(format_args!("Virtual {addr}:{idx}"))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum NextBlock {
    Follow(DestinationKind),
    Jump {
        condition: Expression,
        true_branch: DestinationKind,
        false_branch: DestinationKind,
    },
    Call {
        origin: Address,
        destination: DestinationKind,
        default_return: Address,
    },
    Return,
}

impl Default for NextBlock {
    fn default() -> Self {
        Self::Return
    }
}

#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub enum SpannedItem<T> {
    Item(T, u8),
    ItemAt(u8),
    #[default]
    Empty,
}

impl<T> SpannedItem<T> {
    pub fn unwrap(&self) -> &T {
        match self {
            Self::Item(t, _) => t,
            Self::ItemAt(d) => {
                panic!("Tried to get register, but its value is stored at offset {d}")
            }
            Self::Empty => panic!("unwrapped spanned register state is empty."),
        }
    }
}

impl<T> std::fmt::Display for SpannedItem<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Item(t, _) => write!(f, "{t}"),
            Self::ItemAt(d) => write!(f, "ItemAt({d})"),
            Self::Empty => write!(f, "Empty"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct SpannedStorage<T>(pub SmallVec<[SpannedItem<T>; 8]>);

impl<T> Default for SpannedStorage<T> {
    fn default() -> Self {
        Self(SmallVec::from_buf(std::array::from_fn(|_| {
            SpannedItem::Empty
        })))
    }
}

impl<T> Index<u8> for SpannedStorage<T> {
    type Output = SpannedItem<T>;

    fn index(&self, index: u8) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl<T> IndexMut<u8> for SpannedStorage<T> {
    fn index_mut(&mut self, index: u8) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl<T> SpannedStorage<T> {
    pub fn insert(&mut self, var_node: VarNode, item: T) {
        // cut away overlapping expression
        if let SpannedItem::ItemAt(a) = self[var_node.offset] {
            let new_size = var_node.offset - a;
            if let SpannedItem::Item(e, _) = std::mem::take(&mut self[a]) {
                self[a] = SpannedItem::Item(e, new_size);
            } else {
                panic!("Broken spanned item pointer");
            }
        }
        self[var_node.offset] = SpannedItem::Item(item, var_node.size);
        for offset in 1..var_node.size {
            self[var_node.offset + offset] = SpannedItem::ItemAt(var_node.offset);
        }
        let mut offset = var_node.offset + var_node.size;
        // trim later offsets since this insertion have overwritten a possible expression
        while let SpannedItem::ItemAt(_) = self[offset] {
            self[offset] = SpannedItem::Empty;
            offset += 1;
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct CpuState {
    state: HashMap<i16, SpannedStorage<Expression>>,
}

impl CpuState {
    pub fn new() -> Self {
        Self {
            state: HashMap::new(),
        }
    }
    pub fn get_or_symbolic<'e>(&'e mut self, var_node: VarNode) -> Cow<'e, Expression> {
        let space = self.state.entry(var_node.id).or_default();

        if matches!(&space[var_node.offset], SpannedItem::Empty) {
            space.insert(var_node, Expression::from(ExpressionOp::var_reg(var_node)));
        }

        let size_to_mask = |size: u8| (1u64.unbounded_shl(size as u32 * 8).wrapping_sub(1));

        // What happens when we have data in EAX, but ask for AH, or AX?
        match &space[var_node.offset] {
            SpannedItem::Item(e, size) if var_node.size >= *size => Cow::Borrowed(e), // bigger sizes are padded with 0s which `Expression` can take care of
            SpannedItem::Item(e, size) => {
                // asking for a smaller expression than stored (e.g. asking for AX from EAX)
                let mut output = e.clone();
                let mask = size_to_mask(*size); // just mask upper bytes away
                output.and(&Expression::from(mask));
                Cow::Owned(output)
            }
            SpannedItem::ItemAt(a) => {
                // asking for in-the-middle bytes
                // example: stored EAX, but reading AH
                //     var_node.offset -\
                // [(expression, 4), ItemAt(0), ItemAt(0), ItemAt(0), Empty]
                if let SpannedItem::Item(e, _size) = &space[*a] {
                    let mask = size_to_mask(var_node.size)
                        .unbounded_shl(var_node.offset as u32 - *a as u32); // shift mask to align with the offset
                    let mut output = e.clone();
                    output.and(&Expression::from(mask));
                    Cow::Owned(output)
                } else {
                    unreachable!()
                }
            }
            SpannedItem::Empty => unreachable!(),
        }
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, i16, SpannedStorage<Expression>> {
        self.state.iter()
    }

    pub fn len(&self) -> usize {
        self.state.len()
    }

    pub fn get<'e>(&'e self, var_node: VarNode) -> Option<Cow<'e, Expression>> {
        let storage = self.state.get(&var_node.id)?;
        match &storage[var_node.offset] {
            SpannedItem::Item(e, _) => Some(Cow::Borrowed(e)),
            SpannedItem::ItemAt(a) => match &storage[*a] {
                SpannedItem::Item(e, _) => {
                    let size_to_mask =
                        |size: u8| (1u64.unbounded_shl(size as u32 * 8).wrapping_sub(1));

                    let mask = size_to_mask(var_node.size) << (var_node.offset * 8);

                    let mut modified = e.clone();
                    modified.and(&Expression::from(mask));
                    Some(Cow::Owned(modified))
                }
                i => panic!("Malformed CpuState storageL Item({a}) points to {i:?}"),
            },
            _ => None,
        }
    }

    pub fn get_vec(&self, idx: &i16) -> Option<&SpannedStorage<Expression>> {
        self.state.get(idx)
    }

    pub fn set_state<E: Into<Expression>>(&mut self, var_node: VarNode, expression: E) {
        let expression = expression.into();
        if expression.len() >= super::expression::SMALLVEC_SIZE {
            println!(
                "Heads up, expression {expression} has {} elements",
                expression.len()
            )
        }
        let space = self.state.entry(var_node.id).or_default();
        space.insert(var_node, expression);
    }
}

/// Since PCode instructions may branch inside a single CPU instruction - some basic blocks
/// may not be associated with an address range, but with a single instruction at address -
/// we call them Virtual blocks.
#[derive(Clone, Copy, PartialEq, Default, Hash, Eq, Debug)]
pub enum BlockIdentifier {
    #[default]
    Unset,
    Physical(Interval<Address>),
    Virtual(Address, u8),
}

impl std::fmt::Display for BlockIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockIdentifier::Unset => f.write_str("Unset identifier"),
            BlockIdentifier::Physical(interval) => {
                f.write_fmt(format_args!("{}-{}", interval.start(), interval.end()))
            }
            BlockIdentifier::Virtual(address, block_index) => {
                f.write_fmt(format_args!("{address}:{block_index:?}"))
            }
        }
    }
}

impl BlockIdentifier {
    pub fn contains<A: Into<Address>>(&self, address: A) -> bool {
        match self {
            BlockIdentifier::Unset => false,
            BlockIdentifier::Physical(interval) => interval.contains_point(address.into()),
            BlockIdentifier::Virtual(addr, _) => address.into() == *addr,
        }
    }

    pub fn start_address(&self) -> Address {
        match self {
            BlockIdentifier::Unset => Address::NULL,
            BlockIdentifier::Physical(interval) => interval.start(),
            BlockIdentifier::Virtual(address, _) => *address,
        }
    }
}

/// A basic block is a straight-line sequence of instructions with only one entry point and one exit point.
/// * One entry → control can only enter the block at its first instruction (no jumps into the middle).
/// * One exit → control leaves only at the last instruction.
/// * Inside the block, **PCode** instructions always execute sequentially, no branching.
///
/// * [`BasicBlock`]s can be constructed from PCode using [`super::PCodeToBasicBlocks`]
/// * [`super::HighFunction`] can be used to compose [`BasicBlock`]s together - effectively creating a
/// symbolic execution state.
#[derive(Clone)]
pub struct BasicBlock {
    /// Block identifier that lets us check if two blocks cover the same logic even if
    /// their register and memory states can be different. This lets us
    /// describe semantics of a complex instruction like `rep.movsd`
    pub identifier: BlockIdentifier,
    /// Different cases of information for going to the next block.
    pub next: NextBlock,
    /// Symbolic state of registers at the end of this block
    pub registers: CpuState,
    /// Symbolic state of memory at the end of this block
    pub memory: HashMap<Expression, Expression>,
    /// Changes to memory that this block is responsible for. This is a set of addresses that [`Self::memory`] can be indexed by.
    pub memory_writes: HashSet<Expression>,
    /// Key instructions that contributed to this block (e.g. memory writes, calls, jumps)
    pub key_instructions: HashMap<Address, Expression>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            registers: CpuState::new(),
            memory: HashMap::new(),
            identifier: BlockIdentifier::default(),
            next: NextBlock::default(),
            memory_writes: HashSet::new(),
            key_instructions: HashMap::new(),
        }
    }

    #[inline]
    pub fn is_new(&self) -> bool {
        self.registers.len() == 0 && self.memory.len() == 0
    }

    pub fn get_memory_state<'a, E: Into<Expression>>(
        &'a mut self,
        addr: E,
        size: u8,
    ) -> Cow<'a, Expression> {
        let addr = addr.into();
        Cow::Borrowed(
            self.memory
                .entry(addr.clone())
                .or_insert(Expression::from(VariableSymbol::Ram(Box::new(addr), size))),
        )
    }

    pub fn get_memory_state_or_none<'e, E: Into<&'e Expression>>(
        &self,
        addr: E,
    ) -> Option<&Expression> {
        let addr = addr.into();
        self.memory.get(addr)
    }

    pub fn set_memory_state<E: Into<Expression>>(&mut self, addr: E, state: E) {
        self.memory.insert(addr.into(), state.into());
    }
    // pub fn get_interval(&self) -> Interval<Address> {
    //     ie(self.address, self.end)
    // }

    pub fn clear_temporary_registers(&mut self) {
        self.registers
            .state
            .retain(|k, _v| !VarNode::new(*k, 1).is_temp());
    }

    pub fn is_return(&self) -> bool {
        match self.next {
            NextBlock::Return => true,
            _ => false,
        }
    }

    /// "execute" this block right after `other` - inheriting `other`'s state and modifying our own state as if the execution continued.
    pub fn inherit_state_from(&self, other: &Self) -> Self {
        let mut registers = other.registers.clone();
        let mut memory = other.memory.clone();

        fn replace<'a>(
            other: &'a BasicBlock,
        ) -> impl Fn(&VariableSymbol) -> Option<Cow<'a, Expression>> {
            |var: &VariableSymbol| match var {
                VariableSymbol::Varnode(r) => other.registers.get(*r),
                VariableSymbol::CallResult { .. } => None,
                VariableSymbol::Ram(d, size) => {
                    let mut r = d.clone();
                    r.replace_variable_with(replace(other));
                    if let Some(value) = other.memory.get(&r) {
                        Some(Cow::Borrowed(value))
                    } else {
                        //TODO: Check if symbolic expression with size overlaps this expression with size
                        let e = Expression::from(VariableSymbol::Ram(r, *size));
                        Some(Cow::Owned(e))
                    }
                }
            }
        }

        let mut memory_writes = HashSet::new();

        for (addr, value) in &self.memory {
            let is_setter = self.memory_writes.contains(addr);

            let mut addr = addr.clone();
            addr.replace_variable_with(replace(other));

            if is_setter {
                memory_writes.insert(addr.clone());
            }

            let mut value = value.clone();
            value.replace_variable_with(replace(other));
            memory.insert(addr, value);
        }

        let next = match &self.next {
            NextBlock::Jump {
                condition,
                true_branch,
                false_branch,
            } => {
                let mut condition = condition.clone();
                condition.replace_variable_with(replace(other));
                let true_branch = true_branch
                    .get_symbolic_expression()
                    .and_then(|e| {
                        let mut e = e.clone();
                        e.replace_variable_with(replace(other));
                        Some(DestinationKind::Symbolic(e))
                    })
                    .unwrap_or_else(|| true_branch.clone());
                let false_branch = false_branch
                    .get_symbolic_expression()
                    .and_then(|e| {
                        let mut e = e.clone();
                        e.replace_variable_with(replace(other));
                        Some(DestinationKind::Symbolic(e))
                    })
                    .unwrap_or_else(|| false_branch.clone());
                NextBlock::Jump {
                    condition,
                    true_branch,
                    false_branch,
                }
            }
            NextBlock::Call {
                origin,
                destination,
                default_return,
            } => {
                let destination = destination
                    .get_symbolic_expression()
                    .and_then(|e| {
                        let mut e = e.clone();
                        e.replace_variable_with(replace(other));
                        Some(DestinationKind::Symbolic(e))
                    })
                    .unwrap_or_else(|| destination.clone());
                NextBlock::Call {
                    origin: *origin,
                    destination,
                    default_return: *default_return,
                }
            }
            otherwise => otherwise.clone(),
        };

        for (r, state) in self.registers.state.iter() {
            let mutated = SmallVec::from_buf(std::array::from_fn(|idx| match &state[idx as u8] {
                SpannedItem::Item(e, size) => {
                    let mut e = e.clone();
                    e.replace_variable_with(replace(other));
                    SpannedItem::Item(e, *size)
                }
                SpannedItem::ItemAt(v) => SpannedItem::ItemAt(*v),
                SpannedItem::Empty => SpannedItem::Empty,
            }));
            // if we're referencing a register that previous block didn't have (it won't be in registers) then its variable is an unknown anyway
            // and we don't need to replace it with anything.
            registers.state.insert(*r, SpannedStorage(mutated));
        }

        let mut key_instructions = HashMap::new();
        for (addr, e) in self.key_instructions.iter() {
            let mut e = e.clone();
            e.replace_variable_with(replace(other));
            key_instructions.insert(*addr, e);
        }

        Self {
            identifier: self.identifier,
            memory_writes,
            next,
            registers,
            memory,
            key_instructions,
        }
    }
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self::new()
    }
}

pub struct BlockFunctionIterator<'i> {
    blocks: &'i BlockStorage,
    stack: Vec<BlockSlot>,
    visited: HashSet<BlockSlot>,
}

impl<'i> BlockFunctionIterator<'i> {
    pub fn new(blocks: &'i BlockStorage, start: BlockSlot) -> Self {
        Self {
            blocks,
            stack: vec![start],
            visited: HashSet::new(),
        }
    }
}

impl<'i> Iterator for BlockFunctionIterator<'i> {
    type Item = BlockSlot;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(b) = self.stack.pop() {
            self.visited.insert(b);
            for nbr in self.blocks.iter_neighbors(b) {
                if !self.visited.contains(&nbr) {
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

pub struct BlockPathIterator<'i> {
    blocks: &'i BlockStorage,
    block: Option<BlockSlot>,
}

impl<'i> BlockPathIterator<'i> {
    pub fn new(blocks: &'i BlockStorage, block: BlockSlot) -> Self {
        Self {
            blocks,
            block: Some(block),
        }
    }
}

impl<'i> Iterator for BlockPathIterator<'i> {
    type Item = BlockSlot;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(block) = self.block {
            let mut i = self.blocks.iter_neighbors(block);
            if let Some(result) = i.next() {
                if i.next().is_none() {
                    self.block = Some(result);
                    return Some(result);
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
    start: BlockSlot,
    blocks: &'i BlockStorage,
    yielded_true_branch: bool,
    yielded_all: bool,
}

impl<'i> BlockNeighborsIterator<'i> {
    pub fn new(blocks: &'i BlockStorage, start: BlockSlot) -> Self {
        Self {
            blocks,
            start,
            yielded_true_branch: false,
            yielded_all: false,
        }
    }
}

impl<'i> Iterator for BlockNeighborsIterator<'i> {
    type Item = BlockSlot;

    fn next(&mut self) -> Option<Self::Item> {
        use super::basic_block::NextBlock::*;
        if self.yielded_all || self.blocks.get(self.start).is_none() {
            return None;
        }
        match &self.blocks[self.start].next {
            Jump {
                true_branch,
                false_branch,
                ..
            } => {
                if !self.yielded_true_branch {
                    self.yielded_true_branch = true;
                    match true_branch {
                        DestinationKind::Symbolic(_) => {
                            self.yielded_all = true;
                            self.blocks.slot_by_destination(false_branch)
                        }
                        _ => self.blocks.slot_by_destination(true_branch),
                    }
                } else {
                    self.yielded_all = true;
                    self.blocks.slot_by_destination(false_branch)
                }
            }
            Call { default_return, .. } => {
                self.yielded_all = true;
                self.blocks.slot_by_address(*default_return)
            }
            Follow(next) => {
                self.yielded_all = true;
                self.blocks.slot_by_destination(next)
            }
            Return => None,
        }
    }
}
