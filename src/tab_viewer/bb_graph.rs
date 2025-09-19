use std::collections::HashMap;

use egui::{Color32, Direction, InnerResponse, Layout, MenuBar, Painter, Pos2, Rect, Scene, Stroke, Ui, UiBuilder, Vec2};
use egui_graphs::{
    generate_simple_digraph, DefaultEdgeShape,  DefaultNodeShape, Graph, GraphView, LayoutHierarchical, LayoutStateHierarchical, SettingsInteraction, SettingsNavigation, SettingsStyle
};
use petgraph::{csr::DefaultIx, prelude::StableGraph, Directed};

use crate::{ir::{address::Address, basic_block::{BasicBlock, BlockIdentifier, BlockSlot}}, memory::Memory, tab_viewer::{draw_bb, TabSignals}};

pub type FunctGraphView<'a> = GraphView<
    'a,
    (),
    (),
    Directed,
    DefaultIx,
    DefaultNodeShape,
    DefaultEdgeShape,
    LayoutStateHierarchical,
    LayoutHierarchical,
>;

#[derive(Clone)]
pub struct BlockGraph{
    scene_rect: Rect,
    graph: Option<Graph>,
}

pub fn arrow(painter: &Painter, origin: Pos2, vec: Vec2, stroke: impl Into<Stroke>) {
    use emath::Rot2;
    let rot = Rot2::from_angle(std::f32::consts::TAU / 10.0);
    let tip_length = 10.0_f32;
    let tip = origin + vec;
    let dir = vec.normalized();
    let stroke = stroke.into();
    painter.line_segment([origin, tip], stroke);
    painter.line_segment([tip, tip - tip_length * (rot * dir)], stroke);
    painter.line_segment([tip, tip - tip_length * (rot.inverse() * dir)], stroke);
}

impl BlockGraph {
    pub fn new() -> Self {
        Self { scene_rect: Rect::ZERO  , graph:None}
    }
    pub fn draw(&mut self, ui: &mut Ui, mem:&Memory, current_function:Option<Address>, signals:&mut TabSignals) {
        let mut reset_view = false; 
        MenuBar::new().ui(ui, |ui| {
            reset_view = ui.button("Reset view").clicked();
        });

        if self.graph.is_none() {
            let g = if let Some(addr) = current_function {
                let hf = mem.functions.get(&addr).unwrap();
                (&hf.cfg).into()
            } else {
                let g = petgraph::stable_graph::StableGraph::new();
                egui_graphs::Graph::from(&g)
            };
            self.graph = Some(g);
        }

        egui::Frame::group(ui.style())
            .inner_margin(0.0)
            .show(ui, |ui| {
                if let Some(g) = self.graph.as_mut() {

                    if let Some(pos) = signals.is_requested_pos() {
                        let hf = mem.functions.get(&current_function.unwrap()).unwrap();
                        // todo!("Convert arbitrary addresses to CFG node addresses.");
                        let idx = hf.cfg.get_node_idx(hf.composed_blocks.slot_by_address(pos).unwrap());
                        let node = g.node(idx.into()).unwrap();
                        let pos = node.display().pos;
                        let min = Pos2::new(pos.x - 1.0, pos.y - 1.0);
                        let max = Pos2::new(pos.x + 1.0, pos.y + 1.0);
                        g.set_bounds(Rect { min, max});

                    }

                    let vis = FunctGraphView::new(g);
                    let mut vis  = vis.with_navigations(&SettingsNavigation::new()
                        .with_fit_to_screen_enabled(false)
                        .with_zoom_and_pan_enabled(true)
                    ).with_styles(&SettingsStyle::new()
                        .with_labels_always(true)
                    ).with_interactions(&SettingsInteraction::new()
                        .with_dragging_enabled(true)
                        .with_node_clicking_enabled(true)
                        .with_edge_clicking_enabled(false)
                    );


                    
                    if ui.add(&mut vis).clicked() {
                        if let Some(n) = g.hovered_node() {
                            if let Some(addr) = current_function {
                                let hf = mem.functions.get(&addr).unwrap();
                                match hf.cfg[n.index() as u32] {
                                    BlockIdentifier::Unset => (),
                                    BlockIdentifier::Physical(interval) => signals.request_pos(interval.start()),
                                    BlockIdentifier::Virtual(address, _) => signals.request_pos(address),
                                }
                                ;
                            }
                        }
                    }
                }   
            });

    }
}