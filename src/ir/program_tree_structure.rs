use std::collections::{HashMap, HashSet};

use petgraph::{algo::dominators::Dominators, csr::DefaultIx, visit::IntoNeighbors};

use super::{
    basic_block::{BlockSlot, BlockStorage, NextBlock},
    control_flow_graph::{
        is_ancestor, is_reachable, least_common_ancestor, preorder, ControlFlowGraph,
        SingleEntrySingleExit,
    },
};

/// From Wikipedia: [Program Tree Structure](https://en.wikipedia.org/wiki/Program_structure_tree)
/// is a hierarchical diagram that displays the nesting relationship of [Single-entry single-exit](https://en.wikipedia.org/wiki/Single-entry_single-exit)
///  (SESE) fragments/regions, showing the organization of a computer program. Nodes in this tree represent SESE
/// regions of the program, while edges represent nesting regions.
///
/// The specific implementation is uses [`BlockSlot`] from [`BlockStorage`] as a block identifier.
/// Don't mix [`BlockSlot`] from different storages. If you have to cross-corelate blocks from different
/// [`BlockStorage`]s - use [`BlockStorage::slot_by_identifier`] and [`BasicBlock::identifier`]
pub struct ProgramTreeStructure {
    pub root: SingleEntrySingleExit<BlockSlot>,
    tree: HashMap<SingleEntrySingleExit<BlockSlot>, Vec<SingleEntrySingleExit<BlockSlot>>>,
    block_ownership_table: HashMap<BlockSlot, SingleEntrySingleExit<BlockSlot>>,
}

#[derive(Copy, Clone)]
struct PTSContext<'c> {
    cfg: &'c ControlFlowGraph,
    blocks: &'c BlockStorage,
    pts_tree: &'c HashMap<SingleEntrySingleExit<BlockSlot>, Vec<SingleEntrySingleExit<BlockSlot>>>,
}

impl ProgramTreeStructure {
    pub fn new(cfg: &ControlFlowGraph, blocks: &BlockStorage) -> Self {
        let start_node = cfg.get_node_idx(cfg.start);
        let end_node = cfg.get_node_idx(cfg.single_end());

        let seses = make_sese_pairs(&cfg.dom, &cfg.pdom, &cfg, start_node, end_node)
            .iter()
            .copied()
            .collect::<Vec<_>>();

        let (pts_root, program_tree_structure) =
            build_program_tree_structure(&cfg.dom, &cfg.pdom, &seses, start_node, end_node);

        let mut tree = HashMap::from_iter(program_tree_structure.iter().map(|(k, v)| {
            (
                seseix_to_seseaddr(blocks, *k, &cfg),
                Vec::from_iter(v.iter().map(|i| seseix_to_seseaddr(blocks, *i, &cfg))),
            )
        }));
        let mut pts_root = seseix_to_seseaddr(blocks, pts_root, &cfg);
        let single_end = cfg.single_end();
        tree.insert(SingleEntrySingleExit(cfg.start, single_end), vec![pts_root]); // Add the whole function as a SESE as the used algorithm does not

        pts_root = SingleEntrySingleExit(cfg.start, single_end);
        let mut lookup_table = HashMap::new();
        compute_sese_address_ranges(
            &mut lookup_table,
            PTSContext {
                cfg,
                blocks,
                pts_tree: &tree,
            },
            cfg.start,
            pts_root,
        );
        Self {
            root: pts_root,
            tree,
            block_ownership_table: lookup_table,
        }
    }

    pub fn get_children(
        &self,
        entry: SingleEntrySingleExit<BlockSlot>,
    ) -> Option<&[SingleEntrySingleExit<BlockSlot>]> {
        self.tree.get(&entry).map(|v| &v[..])
    }

    pub fn get_section(&self, block: BlockSlot) -> Option<SingleEntrySingleExit<BlockSlot>> {
        self.block_ownership_table.get(&block).copied()
    }

    /// Pretty print PTS tree to a buffer (string, or a formatter).
    /// If you have additional information to print along the nested SESEs, use the closure to
    /// do so.
    ///
    /// The closure arguments are:
    /// * Writable buffer,
    /// * padding depth (in spaces)
    /// * current SESE that might have additional data to pretty print (e.g. local variables).
    ///
    /// The closure needs to output if it has written anything to the buffer or not
    pub fn pretty_print<F, W>(&self, buffer: &mut W, f: &F) -> std::fmt::Result
    where
        F: Fn(&mut W, u8, SingleEntrySingleExit<BlockSlot>) -> Result<bool, std::fmt::Error>,
        W: std::fmt::Write,
    {
        draw_pts(buffer, self.root, &self.tree, f, 0)
    }
}

fn compute_sese_address_ranges(
    block_ownership_table: &mut HashMap<BlockSlot, SingleEntrySingleExit<BlockSlot>>,
    ctx: PTSContext,
    start: BlockSlot,
    root: SingleEntrySingleExit<BlockSlot>,
) {
    fn assign_path_to_sese(
        block_ownership_table: &mut HashMap<BlockSlot, SingleEntrySingleExit<BlockSlot>>,
        ctx: PTSContext,
        start: BlockSlot,
        root: SingleEntrySingleExit<BlockSlot>,
    ) -> BlockSlot {
        if start == root.1 {
            return start;
        }
        block_ownership_table.insert(start, root);
        let mut last_block = start;
        for node in ctx.blocks.iter_path(start) {
            if node == root.1 {
                break;
            }
            block_ownership_table.insert(node, root);
            last_block = node;
        }
        last_block
    }

    let mut branch_block = assign_path_to_sese(block_ownership_table, ctx, start, root);

    if let Some(children) = ctx.pts_tree.get(&root) {
        while let Some(c_pts) = children.iter().find(|p| p.0 == branch_block) {
            if let NextBlock::Jump {
                true_branch,
                false_branch,
                ..
            } = &ctx.blocks[branch_block].next
            {
                let true_branch_block = ctx
                    .blocks
                    .slot_by_destination(true_branch)
                    .expect("TODO:Handle symbolic branches");
                let false_branch_block = ctx
                    .blocks
                    .slot_by_destination(false_branch)
                    .expect("TODO:Handle symbolic branches");
                compute_sese_address_ranges(block_ownership_table, ctx, true_branch_block, *c_pts);
                compute_sese_address_ranges(block_ownership_table, ctx, false_branch_block, *c_pts);
            } else {
                panic!("Unexpected start of a program segment")
            }

            if c_pts.1 != ctx.cfg.single_end() {
                branch_block = assign_path_to_sese(block_ownership_table, ctx, c_pts.1, root)
            }
            if c_pts.1 == root.1 {
                break;
            }
        }
    }
}

/// Generate single-entry single-exit pairs
fn make_sese_pairs(
    dom: &Dominators<DefaultIx>,
    pdom: &Dominators<DefaultIx>,
    forward_graph: &ControlFlowGraph,
    start: DefaultIx,
    end: DefaultIx,
) -> Vec<SingleEntrySingleExit<DefaultIx>> {
    let mut stack = vec![start];
    let mut seses = HashSet::new();
    let mut visited = HashSet::new();
    while let Some(current) = stack.pop() {
        // candidate immediate post-dominator if the neighbor check fails
        let mut candidate = pdom.immediate_dominator(current);
        for nbr in forward_graph.neighbors(current) {
            if !visited.contains(&nbr) {
                visited.insert(nbr);
                // self.current_node is u
                // nbr is v
                // if current is ansestor if nbr in dom - it dominates nbr
                // and makes a trivial SESE region. We skip those.
                stack.push(nbr);
                if !is_ancestor(current, nbr, &dom) {
                    let a = least_common_ancestor(current, nbr, start, &dom).unwrap();
                    let b = least_common_ancestor(current, nbr, end, &pdom).unwrap();
                    seses.insert(SingleEntrySingleExit(a, b));
                    candidate = None;
                }
            }
        }
        if let Some(candidate) = candidate {
            if forward_graph.neighbors(current).count() > 1 {
                let are_all_neighbors_reachable =
                    forward_graph.neighbors(current).fold(true, |acc, v| {
                        acc && is_reachable(forward_graph, current, v)
                    });
                if are_all_neighbors_reachable {
                    seses.insert(SingleEntrySingleExit(current, candidate));
                }
            }
        }
    }

    // sort SESEs by most encompassing-first
    let dom_pre = preorder(&dom, start);
    let pdom_pre = preorder(&pdom, end);
    let mut seses: Vec<_> = seses.iter().copied().collect();
    seses.sort_by(|l, r| {
        let a_pos = dom_pre[&l.0];
        let c_pos = dom_pre[&r.0];
        if a_pos != c_pos {
            a_pos.cmp(&c_pos)
        } else {
            let b_pos = pdom_pre[&l.1];
            let d_pos = pdom_pre[&r.1];
            // let l_size = b_pos - a_pos;
            // let r_size = d_pos - c_pos;
            b_pos.cmp(&d_pos)
        }
    });
    seses
}

fn seseix_to_seseaddr(
    blocks: &BlockStorage,
    sese: SingleEntrySingleExit<DefaultIx>,
    graph: &ControlFlowGraph,
) -> SingleEntrySingleExit<BlockSlot> {
    let a = blocks
        .slot_by_identifier(graph[sese.0])
        .unwrap_or(graph.single_end());
    let b = blocks
        .slot_by_identifier(graph[sese.1])
        .unwrap_or(graph.single_end());

    SingleEntrySingleExit(a, b)
}

fn build_program_tree_structure<N>(
    dom: &Dominators<N>,
    pdom: &Dominators<N>,
    seses: &Vec<SingleEntrySingleExit<N>>,
    start: N,
    end: N,
) -> (
    SingleEntrySingleExit<N>,
    HashMap<SingleEntrySingleExit<N>, Vec<SingleEntrySingleExit<N>>>,
)
where
    N: Copy + Eq + std::hash::Hash + std::fmt::Debug,
{
    let mut pts = HashMap::new();
    let mut stack: Vec<SingleEntrySingleExit<N>> = Vec::new();
    let largest = seses
        .first()
        .copied()
        .unwrap_or(SingleEntrySingleExit(start, end));
    for sese in seses {
        while let Some(top) = stack.last() {
            let start_top_dominates_sese = is_ancestor(top.0, sese.0, &dom);
            let end_top_postdominates_sese = is_ancestor(top.1, sese.1, &pdom);
            match (start_top_dominates_sese, end_top_postdominates_sese) {
                // top "encloses" sese
                (true, true) => break,
                // top and sese aren't related
                (false, false) => _ = stack.pop(),

                (true, false) => {
                    if !is_ancestor(sese.1, top.1, &pdom) && sese.0 != top.0 {
                        todo!("Cross-over (irreducible) segment detected: {top:?} and {sese:?}");
                    }
                    stack.pop();
                }
                (false, true) => {
                    if is_ancestor(sese.0, top.0, &pdom) && sese.0 != top.0 {
                        todo!("Cross-over (irreducible) segment detected: {top:?} and {sese:?}");
                    }
                    stack.pop();
                }
            }
        }
        if let Some(top) = stack.last() {
            // sese is child of top
            let children = pts.entry(*top).or_insert(Vec::new());
            children.push(*sese);
        } else {
            pts.insert(*sese, Vec::new());
        }
        stack.push(*sese);
    }
    (largest, pts)
}

/// The closure needs to output if it has written anything to the buffer
pub fn draw_pts<W, N, F>(
    buffer: &mut W,
    root: SingleEntrySingleExit<N>,
    pts: &HashMap<SingleEntrySingleExit<N>, Vec<SingleEntrySingleExit<N>>>,
    additional: &F,
    depth: u8,
) -> std::fmt::Result
where
    N: std::fmt::Debug + std::hash::Hash + Eq + Copy,
    W: std::fmt::Write,
    F: Fn(&mut W, u8, SingleEntrySingleExit<N>) -> Result<bool, std::fmt::Error>,
{
    let tab_prefix = " ".repeat((depth * 2) as usize);

    write!(buffer, "{tab_prefix}{root:?} {{")?;
    let mut has_written = additional(buffer, (1 + depth) * 2, root)?;

    if let Some(children) = pts.get(&root) {
        for child in children {
            if !has_written {
                write!(buffer, "\n")?;
            }
            draw_pts(buffer, *child, pts, additional, depth + 1)?;
            has_written = true;
        }
    }

    if has_written {
        write!(buffer, "{tab_prefix}")?;
    }
    write!(buffer, "}}")?;
    if depth > 0 {
        buffer.write_char('\n')
    } else {
        Ok(())
    }
}
