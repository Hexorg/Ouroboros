use egui::{Grid, Ui};
use nodit::DiscreteFinite;

use crate::memory::Memory;

use super::TabSignals;

#[derive(Clone)]
pub struct SectionListView {}

impl SectionListView {
    pub fn new(sections: Vec<String>) -> Self {
        Self {}
    }
    pub fn draw(&mut self, ui: &mut Ui, signals: &mut TabSignals, mem: &Memory) {
        Grid::new("SectionListView").striped(true).show(ui, |ui| {
            ui.label("Section Name");
            ui.label("Virtual Address");
            ui.label("Virtual Size");
            ui.label("Size of raw data");
            ui.label("pointer_to_raw_data");
            ui.label("pointer_to_relocations");
            ui.label("pointer_to_linenumbers");
            ui.label("number_of_relocations");
            ui.label("number_of_linenumbers");
            ui.label("characteristics");
            ui.end_row();
            for section in &mem.navigation.sections {
                let name_label = ui.label(section.name.clone());
                let address_label = ui.label(format!("{}", section.virtual_address));
                ui.label(format!("{}", section.virtual_size));
                ui.label(format!("{}", section.size_of_raw_data));
                ui.label(format!("0x{:x}", section.pointer_to_raw_data));
                ui.label(format!("0x{:x}", section.pointer_to_relocations));
                ui.label(format!("0x{:x}", section.pointer_to_linenumbers));
                ui.label(format!("{}", section.number_of_relocations));
                ui.label(format!("{}", section.number_of_linenumbers));
                ui.label(format!("{}", section.characteristics));

                if name_label.clicked() || address_label.clicked() {
                    signals.request_pos(section.virtual_address);
                }

                if name_label.hovered() {
                    name_label.highlight();
                    ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                }

                if address_label.hovered() {
                    address_label.highlight();
                    ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                }

                ui.end_row();
            }
        });
    }
}
