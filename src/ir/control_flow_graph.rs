use std::{collections::{HashMap, HashSet},  ops::Deref};

use enumset::{EnumSet, EnumSetType};

use crate::ir::{basic_block::NextBlock, Expression};

use super::{Address, BlockStorage};

use petgraph::{algo::{dijkstra, dominators::{simple_fast as build_dominators, Dominators}}, csr::DefaultIx, prelude::{EdgeRef, StableGraph}, visit::{IntoEdgeReferences, IntoNeighbors, IntoNodeReferences}};
use egui_graphs::Graph as EguiGraph;

pub type Graph = petgraph::csr::Csr<Address, LinkKind>;

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
    Calls(Expression),
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
    pub start:Address,
    pub ends:Vec<Address>,
    graph_map:HashMap<Address, DefaultIx>,
    forward_graph: Graph,
    pub distance_to_return:HashMap<Address, u32>,
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
        let mut graph_map: HashMap<Address, DefaultIx> = HashMap::new();
        let mut ends = Vec::new();
        let mut forward_graph:Graph = Graph::new();
        let mut backward_graph:Graph = Graph::new();

        for block in blocks.get_at_point(start).expect("No IR at address").iter_function(blocks) {
            let node_index = *graph_map.entry(block.address).or_insert_with(|| {forward_graph.add_node(block.address); backward_graph.add_node(block.address)});
            graph_map.insert(block.address, node_index);

            if block.is_return() {
                ends.push(block.address);
            }
            
            for nbr in block.iter_neighbors(blocks) {
                let link_kind = match &block.next {
                    NextBlock::Call { destination, .. } => LinkKind::Calls(destination.clone()),
                    NextBlock::ConditionalJump { condition, true_branch, .. } => if nbr.address == *true_branch {
                        LinkKind::TrueBranch(condition.clone())
                    } else {
                        LinkKind::FalseBranch(condition.clone())
                    },
                    NextBlock::Return | NextBlock::ReturnDifferentSite(_) => LinkKind::Return,
                    NextBlock::Unconditional(_) => LinkKind::Unconditional
                };

                let nbr_index = *graph_map.entry(nbr.address).or_insert_with(|| {forward_graph.add_node(nbr.address); backward_graph.add_node(nbr.address)});
                forward_graph.add_edge(node_index, nbr_index, link_kind.clone());
                backward_graph.add_edge(nbr_index, node_index, link_kind);
            }
        }

        
        let mut properties = EnumSet::new();
        let mut pdom_end = Address::NULL;
        if ends.len() == 0 {
            properties.insert(CFGProperties::NeverReturns);
        } else if ends.len() > 1 {
            properties.insert(CFGProperties::MultipleReturns);
            forward_graph.add_node(pdom_end);
            let return_node = backward_graph.add_node(Address::NULL);
            graph_map.insert(pdom_end, return_node);
            for end in &ends {
                // edges.push((end.0, 0));
                forward_graph.add_edge(*graph_map.get(end).unwrap(), return_node, LinkKind::Return);
                backward_graph.add_edge(return_node, *graph_map.get(end).unwrap(), LinkKind::Return);
            }
        } else {
            pdom_end = ends[0];
        }

        let distance_to_return = dijkstra(&backward_graph, *graph_map.get(&pdom_end).unwrap(), None, |e| {
            match e.weight() {
                LinkKind::Calls(_) |
                LinkKind::Return |
                LinkKind::Unconditional => 0,
                LinkKind::TrueBranch(_) |
                LinkKind::FalseBranch(_) => 1,
            }
        }).iter().map(|(k, v)| (backward_graph[*k], *v)).collect::<HashMap<Address, u32>>();
        

        let dom = build_dominators(&forward_graph, *graph_map.get(&start).unwrap());
        let pdom = build_dominators(&backward_graph, *graph_map.get(&pdom_end).unwrap());

        ControlFlowGraph{
            start, 
            ends, 
            graph_map,
            forward_graph,
            distance_to_return,
            properties, 
            dom,
            pdom,
        }
    }

    pub fn single_end(&self) -> Address {
        if self.properties.contains(CFGProperties::MultipleReturns) {
            Address(0)
        } else {
            // TODO: Handle functions that don't return.
            self.ends[0]
        }
    }

    pub fn get_node_idx(&self, addr:Address) -> DefaultIx {
        self.graph_map[&addr]
    }

}


mod test {
    use petgraph::{algo::dominators::simple_fast, csr::DefaultIx};
    
    use crate::ir::least_common_ancestor;

    use super::{Address, Graph};


    /// Graph from [Wikipedia](https://en.wikipedia.org/wiki/Dominator_(graph_theory))
    /// ```
    ///    0
    ///    |
    ///  /--1----\
    ///  |  | \   5
    ///  2  3  ^
    ///   \ | /
    ///     4
    /// ```
    /// Returns (start, end, forward_graph, reverse_graph)
    fn wiki_graph() -> (DefaultIx, DefaultIx, Graph, Graph) {
        let edges = Vec::from([(0, 1), (1, 2), (1, 3), (1, 5), (2, 4), (3, 4), (4, 1)]);
        let mut graph = Graph::new();
        let mut pgraph = Graph::new();
        for _ in 0..6 { graph.add_node(Address::NULL); pgraph.add_node(Address::NULL); }
        for (a, b) in edges {
            graph.add_edge(a, b, super::LinkKind::Return);
            pgraph.add_edge(b, a, super::LinkKind::Return);
        }
        (
            0,
            5,
            graph,
            pgraph
        )
    }

    /// ```
    /// 0
    /// |
    /// 1 --\
    /// |    3--\
    /// 2-\  |  7
    /// | |  6  | 
    /// 4 5   \ |
    /// | /     9
    /// 8      /
    ///  \    /
    ///    \ /
    ///     10
    /// ```
    /// Returns (start, end, forward_graph, reverse_graph)
    fn complex_graph() -> (DefaultIx, DefaultIx, Graph, Graph) {
        let edges = Vec::from([(0, 1), (1, 2), (2, 4), (2, 5), (4, 8), (5, 8), (1, 3), (3, 6), (3, 7), (6, 9), (7, 9), (8, 10), (9, 10)]);
        let mut graph = Graph::new();
        let mut pgraph = Graph::new();
        for _ in 0..11 { graph.add_node(Address::NULL); pgraph.add_node(Address::NULL); }
        for (a, b) in edges {
            graph.add_edge(a, b, super::LinkKind::Return);
            pgraph.add_edge(b, a, super::LinkKind::Return);
        }
        (
            0,
            10,
            graph,
            pgraph
        )
    }


    #[test]
    fn test_lca() {
        let (start, end, graph, pgraph) = wiki_graph();

        let dom = simple_fast(&graph, start);
        let pdom = simple_fast(&pgraph, end);

        println!("{dom:?}");

        let addr = least_common_ancestor(2, 3, start, &dom);
        assert_eq!(addr, Some(1));

        let addr = least_common_ancestor(2, 3, end, &pdom);
        assert_eq!(addr, Some(4));
    }

    // #[test]
    // fn test_sese_pairs() {
    //     let (start, end, graph, pgraph) = complex_graph();

    //     let dom = build_dominators(&graph, start);
    //     let pdom = build_dominators(&pgraph, end);

    //     println!("DOM: {dom:?}");

    //     let seses = make_sese_pairs(&dom, &pdom, &graph, start, end);
    //     for sese in &seses {
    //         println!("{sese:?}");
    //     }
    //     assert_eq!(seses, Vec::from([
    //         // SingleEntrySingleExit(0, 10),
    //         SingleEntrySingleExit(1, 10),
    //         SingleEntrySingleExit(2, 8),
    //         SingleEntrySingleExit(3, 9),
    //         ]))
    // }

    // #[test]
    // fn test_sese_pairs_wiki() {
    //     let (start, end, graph, pgraph) = wiki_graph();

    //     let dom = build_dominators(&graph, start);
    //     let pdom = build_dominators(&pgraph, end);

    //     let seses = make_sese_pairs(&dom, &pdom, &graph, start, end);
    //     assert_eq!(seses, Vec::from([
    //         SingleEntrySingleExit(1, 5),
    //         SingleEntrySingleExit(1, 4),
    //         ]))
    // }
}