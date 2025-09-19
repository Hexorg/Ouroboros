use std::{collections::{HashMap, HashSet},  ops::Deref};

use enumset::{EnumSet, EnumSetType};

use crate::ir::{basic_block::{BlockIdentifier, BlockSlot, DestinationKind, NextBlock}, Expression};

use super::{Address, BlockStorage};

use petgraph::{algo::{dijkstra, dominators::{simple_fast as build_dominators, Dominators}}, csr::DefaultIx, prelude::{EdgeRef, StableGraph}, visit::{IntoEdgeReferences, IntoNeighbors, IntoNodeReferences}};
use egui_graphs::Graph as EguiGraph;

pub type Graph = petgraph::csr::Csr<BlockIdentifier, LinkKind>;


/// From Wikipedia: [Single-entry single-exit](https://en.wikipedia.org/wiki/Single-entry_single-exit) region
/// in a given graph is an ordered edge pair.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct SingleEntrySingleExit<N>(pub N, pub N);

#[derive(EnumSetType, Debug)]
pub enum CFGProperties {
    NeverReturns,
    /// If this CFG has multiple RET blocks, all of them are linked to a fake return block at `Address::NULL`
    MultipleReturns
}


#[derive(Clone)]
pub enum LinkKind {
    Calls(DestinationKind),
    TrueBranch(Expression),
    FalseBranch(Expression),
    Unconditional,
    Return,
}

impl std::fmt::Display for LinkKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkKind::Calls(expression) => f.write_fmt(format_args!("Calls {expression}")),
            LinkKind::TrueBranch(expression) => f.write_fmt(format_args!("{expression} is true")),
            LinkKind::FalseBranch(expression) => f.write_fmt(format_args!("{expression} is false")),
            LinkKind::Unconditional => f.write_str(""),
            LinkKind::Return => f.write_str("returns")
        }
    }
}



pub struct ControlFlowGraph{
    pub start:BlockSlot,
    pub ends:Vec<BlockSlot>,
    graph_map:HashMap<BlockSlot, DefaultIx>,
    forward_graph: Graph,
    pdom_end: BlockSlot,
    pub distance_to_return:HashMap<BlockSlot, u32>,
    pub properties: EnumSet<CFGProperties>,
    pub dom: Dominators<DefaultIx>,
    pub pdom: Dominators<DefaultIx>,
}

impl Deref for ControlFlowGraph {
    type Target = Graph;

    fn deref(&self) -> &Self::Target {
        &self.forward_graph
    }
}

impl Into<EguiGraph> for &ControlFlowGraph {
    fn into(self) -> EguiGraph {

        let mut g = EguiGraph::new(StableGraph::default());
        let mut graph_map = HashMap::new();
        for (_, &addr) in self.node_references() {
            let idx = g.add_node_with_label((), format!("{addr}"));
            graph_map.insert(addr, idx);
            
        }
        for edge in self.edge_references() {
            let src_addr = self[edge.source()];
            let dst_addr = self[edge.target()];
            let src = graph_map[&src_addr];
            let dst = graph_map[&dst_addr];
            let label = match edge.weight() {
                LinkKind::Calls(expression) => format!("                    Calls {expression}"),
                LinkKind::TrueBranch(expression) => format!("                    {expression} is true"),
                LinkKind::FalseBranch(expression) => format!("                    {expression} is false"),
                LinkKind::Unconditional => String::new(),
                LinkKind::Return => String::from("returns"),
            };
            g.add_edge_with_label(src, dst, (), label);
        }
        g
    }
}

/// Check if `start` is an ancestor of `find` in a `tree`
pub fn is_ancestor<N>(start:N, find:N, dom:&Dominators<N>) -> bool 
where N: Copy + Eq + std::hash::Hash {
    if start == find {
        return true
    }
    let mut all = false;
    for leaf in dom.immediately_dominated_by(start) {
        // for leaf in nbrs {
            if leaf == find {
                return true
            } else {
                all |= is_ancestor(leaf, find, dom);
            }
        }
        all
}

pub fn preorder<N>(dom:&Dominators<N>, root:N) -> HashMap<N, usize> 
where N: Copy + Eq + std::hash::Hash
{
    let mut result = HashMap::new();
    let mut order = 0;
    let mut stack = vec![root];
    while let Some(node) = stack.pop() {
        result.insert(node, order);
        order += 1;
        for nbr in dom.immediately_dominated_by(node) {
            if !result.contains_key(&nbr) {
                stack.push(nbr);
            }
        }
    }
    result
}

pub fn find_path(a:DefaultIx, tree_root:DefaultIx, dom:&Dominators<DefaultIx>) -> Vec<DefaultIx> {
    // if let Some(iterator) =  {
        for nbr in dom.immediately_dominated_by(tree_root) {
            if nbr == a {
                return vec![a]
            } else {
                let mut path = find_path(a, nbr, dom);
                if path.len() > 0 {
                    path.push(nbr);
                    return path
                }
            }
        }
    // } 
    Vec::new()
}


pub fn least_common_ancestor(u:DefaultIx, v: DefaultIx, tree_root:DefaultIx, tree:&Dominators<DefaultIx>) -> Option<DefaultIx> {
    // TODO: Faster methods exist https://en.wikipedia.org/wiki/Lowest_common_ancestor
    let mut path_to_u = find_path(u, tree_root, tree);
    path_to_u.push(tree_root);
    let mut path_to_v = find_path(v, tree_root, tree);
    path_to_v.push(tree_root);
    path_to_u.reverse();
    path_to_v.reverse();
    let max_pos = path_to_u.len().min(path_to_v.len());
    for idx in 0..max_pos {
        if path_to_u[idx] != path_to_v[idx] {
            if idx > 0 {
                return Some(path_to_u[idx - 1])
            } else {
                return None
            }
        }
    }
    Some(path_to_u[max_pos-1])
}

pub fn is_reachable(graph:&Graph, start:DefaultIx, target:DefaultIx) -> bool {
    let mut stack = vec![start];
    let mut visited = HashSet::new();
    while let Some(current) = stack.pop() {
        if !visited.contains(&current) {
            for nbr in graph.neighbors(current) {
                if nbr == target {
                    return true;
                }
                visited.insert(nbr);
            }
        }
    }
    false
}

impl ControlFlowGraph {
    pub fn new(start:Address, blocks:&BlockStorage) -> Self {
        let mut graph_map: HashMap<BlockSlot, DefaultIx> = HashMap::new();
        let mut ends = Vec::new();
        let mut forward_graph:Graph = Graph::new();
        let mut backward_graph:Graph = Graph::new();
        let start = blocks.slot_by_address(start).expect("No IR at address");
        for block_id in blocks.iter_function(start) {
            let node_index = *graph_map.entry(block_id).or_insert_with(|| {forward_graph.add_node(blocks[block_id].identifier); backward_graph.add_node(blocks[block_id].identifier)});
            graph_map.insert(block_id, node_index);

            if blocks[block_id].is_return() {
                ends.push(block_id);
            }
            
            for nbr in blocks.iter_neighbors(block_id) {
                let link_kind = match &blocks[block_id].next {
                    NextBlock::Call { destination, .. } => LinkKind::Calls(destination.clone()),
                    NextBlock::Jump { condition, true_branch, .. } => {
                        if blocks.slot_by_destination(true_branch) == Some(nbr) {
                            LinkKind::TrueBranch(condition.clone())
                        } else {
                            LinkKind::FalseBranch(condition.clone())
                        }
                    },
                    NextBlock::Return  => LinkKind::Return,
                    NextBlock::Follow(_) => LinkKind::Unconditional
                };

                let nbr_index = *graph_map.entry(nbr).or_insert_with(|| {forward_graph.add_node(blocks[nbr].identifier); backward_graph.add_node(blocks[nbr].identifier)});
                forward_graph.add_edge(node_index, nbr_index, link_kind.clone());
                backward_graph.add_edge(nbr_index, node_index, link_kind);
            }
        }

        
        let mut properties = EnumSet::new();
        let pdom_end;
        if ends.len() == 0 {
            pdom_end = blocks.next_available_id();
            properties.insert(CFGProperties::NeverReturns);
        } else if ends.len() > 1 {
            pdom_end = blocks.next_available_id();
            properties.insert(CFGProperties::MultipleReturns);
            forward_graph.add_node(BlockIdentifier::Unset);
            let return_node = backward_graph.add_node(BlockIdentifier::Unset);
            graph_map.insert(pdom_end, return_node);
            for end in &ends {
                // edges.push((end.0, 0));
                forward_graph.add_edge(*graph_map.get(end).unwrap(), return_node, LinkKind::Return);
                backward_graph.add_edge(return_node, *graph_map.get(end).unwrap(), LinkKind::Return);
            }
        } else {
            pdom_end = ends[0];
        }

        // if this fails at graph_map.get that's because function never returns. What to do in that case?
        let distance_to_return = dijkstra(&backward_graph, *graph_map.get(&pdom_end).unwrap(), None, |e| {
            match e.weight() {
                LinkKind::Calls(_) |
                LinkKind::Return |
                LinkKind::Unconditional => 0,
                LinkKind::TrueBranch(_) |
                LinkKind::FalseBranch(_) => 1,
            }
        }).iter().map(|(k, v)| (if let Some(slot) = blocks.slot_by_identifier(backward_graph[*k]) { (slot, *v)} else { (BlockSlot::default(), u32::MAX)})).collect::<HashMap<BlockSlot, u32>>();
        

        let dom = build_dominators(&forward_graph, *graph_map.get(&start).unwrap());
        let pdom = build_dominators(&backward_graph, *graph_map.get(&pdom_end).unwrap());

        ControlFlowGraph{
            start, 
            ends, 
            graph_map,
            forward_graph,
            pdom_end,
            distance_to_return,
            properties, 
            dom,
            pdom,
        }
    }

    #[inline]
    pub fn single_end(&self) -> BlockSlot {
        self.pdom_end
    }

    pub fn get_node_idx(&self, index:BlockSlot) -> DefaultIx {
        self.graph_map[&index]
    }

}


mod test {
    use petgraph::{algo::dominators::simple_fast, csr::DefaultIx};
    
    use super::{Address, Graph, least_common_ancestor};


    // /// Graph from [Wikipedia](https://en.wikipedia.org/wiki/Dominator_(graph_theory))
    // /// ```
    // ///    0
    // ///    |
    // ///  /--1----\
    // ///  |  | \   5
    // ///  2  3  ^
    // ///   \ | /
    // ///     4
    // /// ```
    // /// Returns (start, end, forward_graph, reverse_graph)
    // fn wiki_graph() -> (DefaultIx, DefaultIx, Graph, Graph) {
    //     let edges = Vec::from([(0, 1), (1, 2), (1, 3), (1, 5), (2, 4), (3, 4), (4, 1)]);
    //     let mut graph = Graph::new();
    //     let mut pgraph = Graph::new();
    //     for _ in 0..6 { graph.add_node(Address::NULL); pgraph.add_node(Address::NULL); }
    //     for (a, b) in edges {
    //         graph.add_edge(a, b, super::LinkKind::Return);
    //         pgraph.add_edge(b, a, super::LinkKind::Return);
    //     }
    //     (
    //         0,
    //         5,
    //         graph,
    //         pgraph
    //     )
    // }

    // /// ```
    // /// 0
    // /// |
    // /// 1 --\
    // /// |    3--\
    // /// 2-\  |  7
    // /// | |  6  | 
    // /// 4 5   \ |
    // /// | /     9
    // /// 8      /
    // ///  \    /
    // ///    \ /
    // ///     10
    // /// ```
    // /// Returns (start, end, forward_graph, reverse_graph)
    // fn complex_graph() -> (DefaultIx, DefaultIx, Graph, Graph) {
    //     let edges = Vec::from([(0, 1), (1, 2), (2, 4), (2, 5), (4, 8), (5, 8), (1, 3), (3, 6), (3, 7), (6, 9), (7, 9), (8, 10), (9, 10)]);
    //     let mut graph = Graph::new();
    //     let mut pgraph = Graph::new();
    //     for _ in 0..11 { graph.add_node(Address::NULL); pgraph.add_node(Address::NULL); }
    //     for (a, b) in edges {
    //         graph.add_edge(a, b, super::LinkKind::Return);
    //         pgraph.add_edge(b, a, super::LinkKind::Return);
    //     }
    //     (
    //         0,
    //         10,
    //         graph,
    //         pgraph
    //     )
    // }


    // #[test]
    // fn test_lca() {
    //     let (start, end, graph, pgraph) = wiki_graph();

    //     let dom = simple_fast(&graph, start);
    //     let pdom = simple_fast(&pgraph, end);

    //     println!("{dom:?}");

    //     let addr = least_common_ancestor(2, 3, start, &dom);
    //     assert_eq!(addr, Some(1));

    //     let addr = least_common_ancestor(2, 3, end, &pdom);
    //     assert_eq!(addr, Some(4));
    // }
}