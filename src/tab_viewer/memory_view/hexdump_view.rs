use egui::{
    Color32, Grid, InnerResponse, Key, Label, Layout, Response, RichText, ScrollArea, Sense,
    Spacing, Stroke, Style, Ui, UiBuilder, Vec2,
};
use egui_extras::{Column, Table, TableBuilder};

use crate::{
    ir::address::Address,
    memory::{LiteralKind, LiteralState},
    tab_viewer::TabSignals,
};

use super::super::{CodeTheme, TokenType};

pub fn draw(
    theme: &CodeTheme,
    ui: &mut Ui,
    signals: &mut TabSignals,
    current_addr: &mut Address,
    mut end_addr: Address,
    state: &LiteralState,
) {
    if let LiteralKind::Data(bytes) = &state.kind {
        let mut alignment = current_addr.0 % 16;
        current_addr.0 = current_addr.0 - alignment;

        let my_max_addr = (state.addr.0 + bytes.len() as u64);
        if end_addr.0 > my_max_addr {
            end_addr.0 = my_max_addr;
        }

        let row_height = ui.spacing().interact_size.y;
        let spacer = ui.spacing().item_spacing.x;
        TableBuilder::new(ui)
            .id_salt(state.addr)
            .columns(Column::auto(), 9) // Address + Bytes 0-7
            .column(Column::exact(spacer)) // Separator
            .columns(Column::auto(), 9) // Bytes 8-15 + ASCII text
            .striped(true)
            .cell_layout(Layout::bottom_up(egui::Align::Center))
            .vscroll(false)
            .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysHidden)
            .header(row_height, |mut header| {
                header.col(|ui| _ = ui.label("Address"));
                header.col(|ui| _ = ui.label("0"));
                header.col(|ui| _ = ui.label("1"));
                header.col(|ui| _ = ui.label("2"));
                header.col(|ui| _ = ui.label("3"));
                header.col(|ui| _ = ui.label("4"));
                header.col(|ui| _ = ui.label("5"));
                header.col(|ui| _ = ui.label("6"));
                header.col(|ui| _ = ui.label("7"));
                header.col(|_ui| {});
                header.col(|ui| _ = ui.label("8"));
                header.col(|ui| _ = ui.label("9"));
                header.col(|ui| _ = ui.label("A"));
                header.col(|ui| _ = ui.label("B"));
                header.col(|ui| _ = ui.label("C"));
                header.col(|ui| _ = ui.label("D"));
                header.col(|ui| _ = ui.label("E"));
                header.col(|ui| _ = ui.label("F"));
                header.col(|_ui| {});
            })
            .body(|mut table| {
                while current_addr.0 < end_addr.0 {
                    let offset = current_addr.0 as i64 - (state.addr.0) as i64;

                    let mut hover_offset = None;
                    table.row(row_height, |mut row| {
                        row.col(|ui| {
                            _ = ui.label(
                                theme.make_rich(TokenType::Punctuation, format!("{current_addr}")),
                            )
                        });
                        for i in 0..16 {
                            let byte_offset = offset + i;

                            // Add separator column after byte 7
                            if i == 8 {
                                row.col(|_ui| {});
                            }

                            row.col(|ui| {
                                if byte_offset >= 0 && (byte_offset as usize) < bytes.len() {
                                    let label = ui.label(theme.make_rich(
                                        TokenType::NumericalLiteral,
                                        format!("{:02X}", bytes[byte_offset as usize]),
                                    ));
                                    if label.contains_pointer() {
                                        hover_offset = Some(byte_offset);
                                        ui.painter_at(label.rect).rect(
                                            label.rect,
                                            0.,
                                            theme.highlight_color,
                                            Stroke::NONE,
                                            egui::StrokeKind::Inside,
                                        );
                                        if ui.ctx().input(|r| r.key_pressed(Key::D)) {
                                            signals
                                                .mark_instruction(state.addr + byte_offset.into());
                                        }
                                    }
                                }
                            });
                        }

                        row.col(|ui| {
                            ui.horizontal_top(|ui| {
                                ui.spacing_mut().item_spacing.x = 0.0;
                                for offset in offset..bytes.len().min((offset + 16) as usize) as i64
                                {
                                    let c = if offset >= 0 {
                                        if bytes[offset as usize].is_ascii()
                                            && !bytes[offset as usize].is_ascii_control()
                                        {
                                            bytes[offset as usize] as char
                                        } else {
                                            '.'
                                        }
                                    } else {
                                        ' '
                                    };
                                    let lbl =
                                        Label::new(theme.make_rich(TokenType::StringLiteral, c))
                                            .wrap_mode(egui::TextWrapMode::Extend);
                                    let c = ui.add(lbl);
                                    if let Some(hoffset) = hover_offset {
                                        if hoffset == offset {
                                            ui.painter_at(c.rect).rect(
                                                c.rect,
                                                0.,
                                                theme.highlight_color,
                                                Stroke::NONE,
                                                egui::StrokeKind::Inside,
                                            );
                                        }
                                    }
                                }
                            });
                        });
                    });
                    current_addr.0 += 16;
                    if current_addr.0 > end_addr.0 {
                        *current_addr = end_addr;
                    }
                }
            });
    } else {
        panic!("Unexpected state kind passed to hexdump view");
    }
}
