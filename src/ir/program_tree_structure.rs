
use std::collections::{HashMap, HashSet};

use nodit::{Interval, NoditMap};
use petgraph::{algo::dominators::Dominators, csr::DefaultIx, visit::IntoNeighbors};

use crate::ir::{basic_block::NextBlock, is_ancestor, is_reachable, least_common_ancestor, preorder, BasicBlock, BlockStorage, HighFunction};

use super::{Address, ControlFlowGraph, SingleEntrySingleExit};

pub struct ProgramTreeStructure {
    pub root:SingleEntrySingleExit<Address>,
    tree:HashMap<SingleEntrySingleExit<Address>, Vec<SingleEntrySingleExit<Address>>>,
    lookup_table:NoditMap<Address, Interval<Address>, SingleEntrySingleExit<Address>>
}

impl ProgramTreeStructure {
    pub fn new(cfg:&ControlFlowGraph, blocks:&BlockStorage) -> Self {
        let start_node = cfg.get_node_idx(cfg.start);
        let end_node = cfg.get_node_idx(cfg.single_end());

        let seses = make_sese_pairs(&cfg.dom, &cfg.pdom, &cfg, start_node, end_node).iter().copied().collect::<Vec<_>>();
        
        let (pts_root, program_tree_structure) = build_program_tree_structure(&cfg.dom, &cfg.pdom, &seses, start_node, end_node);
        
        let mut tree =  HashMap::from_iter(program_tree_structure.iter().map(|(k, v)| {
                (
                    seseix_to_seseaddr(*k, &cfg),
                    Vec::from_iter(v.iter().map(|i| seseix_to_seseaddr(*i, &cfg)))
                )
            }));
        let mut pts_root =  seseix_to_seseaddr(pts_root, &cfg);
        tree.insert(SingleEntrySingleExit(cfg.start, cfg.single_end()), vec![pts_root]); // Add the whole function as a SESE as the used algorithm does not

        pts_root = SingleEntrySingleExit(cfg.start, cfg.single_end());
        let mut lookup_table = NoditMap::new();
        compute_sese_address_ranges(&mut lookup_table, cfg.start, pts_root, &tree, blocks);
        Self { root: pts_root, tree, lookup_table }
    }

    pub fn get_children(&self, entry:SingleEntrySingleExit<Address>) -> Option<&[SingleEntrySingleExit<Address>]> {
        self.tree.get(&entry).map(|v| &v[..])
    }

    pub fn get_section(&self, addr:Address) -> Option<SingleEntrySingleExit<Address>> {
        self.lookup_table.get_at_point(addr).copied()
    }

    /// The closure needs to output if it has written anything to the buffer
    pub fn pretty_print<F, W>(&self, buffer:&mut W, f:&F) -> std::fmt::Result
    where 
        F:Fn(&mut W, u8, SingleEntrySingleExit<Address>) -> Result<bool, std::fmt::Error>,
        W:std::fmt::Write
    {
        draw_pts(buffer, self.root, &self.tree, f, 0)
    }
}

fn compute_sese_address_ranges(table:&mut NoditMap<Address, Interval<Address>, SingleEntrySingleExit<Address>>, start:Address, root:SingleEntrySingleExit<Address>, tree:&HashMap<SingleEntrySingleExit<Address>, Vec<SingleEntrySingleExit<Address>>>, blocks:&BlockStorage) {

    fn process_path<'a>(table:&mut NoditMap<Address, Interval<Address>, SingleEntrySingleExit<Address>>, start:Address, root:SingleEntrySingleExit<Address>, blocks:&'a BlockStorage) -> &'a BasicBlock {
        let start = blocks.get_at_point(start).unwrap();
        if start.address == root.1 {
            return start;
        }
        table.insert_merge_touching_if_values_equal(start.get_interval(), root);
        let mut last_block = start;
        for node in start.iter_path(blocks) {
            if node.address == root.1 {
                break;
            }
            table.insert_merge_touching_if_values_equal(node.get_interval(), root);
            last_block = node;
        }
        last_block
    }

    let mut branch_block = process_path(table, start, root, blocks);

    if let Some(children) = tree.get(&root) {
        while let Some(c_pts) = children.iter().find(|p| p.0 == branch_block.address) {
            
            if let NextBlock::ConditionalJump {true_branch, false_branch, .. } = branch_block.next {
                compute_sese_address_ranges(table, true_branch, *c_pts, tree, blocks);
                compute_sese_address_ranges(table, false_branch, *c_pts, tree, blocks);
            } else {
                panic!("Unexpected start of a program segment")
            }

            if c_pts.1 != Address::NULL {
                branch_block = process_path(table, c_pts.1, root, blocks)
            }
            if c_pts.1 == root.1 {
                break;
            }
        }
    }
}

/// Generate single-entry single-exit pairs
fn make_sese_pairs(dom:&Dominators<DefaultIx>, pdom:&Dominators<DefaultIx>, forward_graph:&ControlFlowGraph, start:DefaultIx, end:DefaultIx) -> Vec<SingleEntrySingleExit<DefaultIx>> {
    
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
                let are_all_neighbors_reachable = forward_graph.neighbors(current).fold(true, |acc, v| acc && is_reachable(forward_graph, current, v));
                if are_all_neighbors_reachable {
                    seses.insert(SingleEntrySingleExit(current, candidate));
                }
            }
        }
    }

    // sort SESEs by most encompassing-first
    let dom_pre = preorder(&dom, start);
    let pdom_pre = preorder(&pdom, end);
    let mut seses:Vec<_> = seses.iter().copied().collect();
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

fn seseix_to_seseaddr(sese:SingleEntrySingleExit<DefaultIx>, graph:&ControlFlowGraph) -> SingleEntrySingleExit<Address> {
    let a = graph[sese.0];
    let b = graph[sese.1];
    SingleEntrySingleExit(a, b)
}

fn build_program_tree_structure<N>(dom:&Dominators<N>, pdom:&Dominators<N>, seses:&Vec<SingleEntrySingleExit<N>>, start:N, end:N) -> (SingleEntrySingleExit<N>, HashMap<SingleEntrySingleExit<N>, Vec<SingleEntrySingleExit<N>>>) 
where N: Copy + Eq + std::hash::Hash + std::fmt::Debug
{
    let mut pts = HashMap::new();
    let mut stack:Vec<SingleEntrySingleExit<N>> = Vec::new();
    let largest = seses.first().copied().unwrap_or(SingleEntrySingleExit(start, end));
    for sese in seses {
        while let Some(top) = stack.last() {
            let start_top_dominates_sese = is_ancestor(top.0, sese.0, &dom);
            let end_top_postdominates_sese = is_ancestor(top.1, sese.1, &pdom);
            match (start_top_dominates_sese, end_top_postdominates_sese) {
                // top "encloses" sese
                (true, true) => break,
                // top and sese aren't related
                (false, false) => _= stack.pop(), 

                (true, false) => {
                    if !is_ancestor(sese.1, top.1, &pdom) && sese.0 != top.0 {
                        todo!("Cross-over (irreducible) segment detected: {top:?} and {sese:?}");
                    }
                    stack.pop();
                },
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
pub fn draw_pts<W, N, F>(buffer:&mut W, root:SingleEntrySingleExit<N>, pts:&HashMap<SingleEntrySingleExit<N>, Vec<SingleEntrySingleExit<N>>>, additional:&F, depth:u8) -> std::fmt::Result
where 
    N: std::fmt::Debug + std::hash::Hash + Eq + Copy,
    W: std::fmt::Write,
    F: Fn(&mut W, u8, SingleEntrySingleExit<N>) -> Result<bool, std::fmt::Error>
{
    let tab_prefix = " ".repeat((depth*2) as usize);

    write!(buffer, "{tab_prefix}{root:?} {{")?;
    let has_written = additional(buffer, (1+depth)*2, root)?;

    if let Some(children) = pts.get(&root) {
        for child in children {
            draw_pts(buffer, *child, pts, additional, depth+1)?;
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
