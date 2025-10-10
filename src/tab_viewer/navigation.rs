use egui::Ui;

use crate::{ir::expression::Expression, memory::Memory};

use super::TabSignals;

#[derive(Clone)]
pub struct NavigationView {}

impl NavigationView {
    pub fn new() -> Self {
        Self {}
    }
    pub fn draw(&mut self, ui: &mut Ui, signals: &mut TabSignals, mem: &Memory) {
        for (&addr, _func) in mem.functions.iter() {
            let name = mem
                .symbols
                .resolve_exp(&Expression::from(addr))
                .and_then(|s| Some(s.name.as_str()))
                .unwrap();
            if ui.label(name).clicked() {
                signals.request_pos(addr);
            };
        }
    }
}
