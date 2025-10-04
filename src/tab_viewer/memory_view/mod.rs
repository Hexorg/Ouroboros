use std::collections::HashMap;

use eframe::egui;
use egui::{
    Color32, Grid, InnerResponse, Response, RichText, ScrollArea, Sense, Spacing, Stroke, Style,
    Ui, UiBuilder, Vec2,
};

mod asm_view;
mod hexdump_view;

use super::{CodeTheme, TokenType};
use crate::{
    ir::{address::Address, basic_block::BasicBlock, expression::Expression},
    memory::Memory,
    tab_viewer::TabSignals,
};

#[derive(Clone)]
pub struct MemoryView {
    // pos:usize,
    theme: CodeTheme,
    addr_row_map: HashMap<Address, usize>,
    row_addr_map: Vec<Address>,
    title: String,
}

impl MemoryView {
    const STROKE: Stroke = Stroke {
        width: 1.0,
        color: Color32::RED,
    };

    /// Needs memory reference to figure out address to display row mapping.
    pub fn new(style: &Style, mem: &Memory) -> Self {
        let (addr_row_map, row_addr_map) = map_addr(mem);
        Self {
            theme: CodeTheme::from_style(style),
            addr_row_map,
            row_addr_map,
            title: String::from("Memory View"),
        }
    }

    pub fn title(&self) -> egui::WidgetText {
        self.title.clone().into()
    }

    pub fn draw(&mut self, ui: &mut Ui, signals: &mut TabSignals, mem: &Memory) {
        if signals.is_new_file() || signals.is_repopulate_instruction_rows() {
            println!("Repopulating instruction view data");
            let (addr_row_map, row_addr_map) = map_addr(mem);
            self.addr_row_map = addr_row_map;
            self.row_addr_map = row_addr_map;
        }

        let mut area = ScrollArea::both().auto_shrink(false).id_salt("MemoryView");
        if let Some(addr) = signals.is_requested_pos() {
            println!("Requesting new scroll offset to address {addr}");
            area = area.vertical_scroll_offset(
                (ui.spacing().interact_size.y + ui.spacing().item_spacing.y)
                    * self.addr_row_map[&addr] as f32,
            );
        }

        if self.row_addr_map.len() > 0 {
            area.show_rows(
                ui,
                ui.spacing().interact_size.y,
                self.addr_row_map.len(),
                |ui, row_range| {
                    let start_addr = self.row_addr_map[row_range.start];

                    let last_row = if row_range.end < self.row_addr_map.len() {
                        row_range.end
                    } else {
                        self.row_addr_map.len() - 1
                    };
                    let end_addr = self.row_addr_map[last_row];

                    if let Some(section) = mem.navigation.sections.iter().find(|s| {
                        s.virtual_address <= start_addr
                            && (s.virtual_address.0 + s.virtual_size as u64) > start_addr.0
                    }) {
                        if !self.title.contains(&section.name) {
                            self.title = format!("Memory View: {}", section.name);
                        }
                    }

                    let mut current_addr = start_addr;
                    ui.vertical(|ui| {
                        while current_addr < end_addr {
                            if let Some(state) = mem.literal.get_at_point(current_addr) {
                                match &state.kind {
                                    crate::memory::LiteralKind::Data(_) => hexdump_view::draw(
                                        &self.theme,
                                        ui,
                                        signals,
                                        &mut current_addr,
                                        end_addr,
                                        state,
                                    ),
                                    crate::memory::LiteralKind::Instruction(_, _) => {
                                        asm_view::draw(
                                            &self.theme,
                                            ui,
                                            signals,
                                            mem,
                                            &mut current_addr,
                                            end_addr,
                                            state,
                                        )
                                    }
                                }
                                ui.add_space(8.0);
                            } else {
                                Grid::new("NoAddrGrid").show(ui, |ui| {
                                    ui.label(self.theme.make_rich(
                                        TokenType::Punctuation,
                                        format!("{current_addr}"),
                                    ));
                                    ui.label(self.theme.make_rich(
                                        TokenType::Keyword,
                                        "No data loaded at this address",
                                    ));
                                });
                                break;
                            }
                            ui.end_row();
                        }
                    });
                },
            );
        }
    }
}

fn map_addr(mem: &Memory) -> (HashMap<Address, usize>, Vec<Address>) {
    let mut ar_map = HashMap::new();
    let mut ra_map = Vec::new();
    for (_, data) in mem.literal.iter() {
        match &data.kind {
            crate::memory::LiteralKind::Data(d) => {
                let size = data.kind.size();
                let end_addr = data.addr + size.into();
                let mut current_addr = data.addr;
                while current_addr < end_addr {
                    let row = ar_map.len();
                    ar_map.insert(current_addr, row);
                    ra_map.push(current_addr);
                    current_addr.0 += 16;
                }
            }
            crate::memory::LiteralKind::Instruction(_, instructions) => {
                for instr in instructions {
                    let row = ar_map.len();
                    let addr = Address(instr.inst_start);
                    ar_map.insert(addr, row);
                    ra_map.push(addr);
                }
            }
        }
    }
    (ar_map, ra_map)
}
