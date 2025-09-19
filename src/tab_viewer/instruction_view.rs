use std::{
    collections::HashMap,
    fmt::format,
    hash::{Hash, Hasher},
};

use eframe::egui;
use egui::{
    scroll_area, Color32, Grid, Id, InnerResponse, LayerId, NumExt, Rect, Response, RichText,
    ScrollArea, Sense, Spacing, Stroke, Style, Ui, UiBuilder, Vec2,
};
use egui_extras::{Column, TableBuilder};
use nodit::InclusiveInterval;
use sleigh_compile::ldef::SleighLanguage;
use sleigh_runtime::{Instruction, SubtableCtx};

use super::{CodeTheme, TokenType};
use crate::{
    ir::{
        address::Address, basic_block::BasicBlock, expression::Expression,
        high_function::HighFunction,
    },
    memory::Memory,
    tab_viewer::TabSignals,
};
pub struct InstructionsView<'t, T> {
    salt: T,
    theme: &'t CodeTheme,
    block: Option<&'t BasicBlock>,
}

fn horizontal<R>(ui: &mut Ui, add_contents: impl FnOnce(&mut Ui) -> R) -> InnerResponse<R> {
    let ui_builder = egui::UiBuilder::new().layout(egui::Layout::left_to_right(egui::Align::Min));
    let ui_builder = ui_builder.style(Style {
        spacing: Spacing {
            item_spacing: Vec2::new(0.0, 0.0),
            ..Default::default()
        },
        ..Default::default()
    });
    ui.scope_builder(ui_builder, add_contents)
}

fn parse_numeric_field(
    tokens: &mut Vec<RichText>,
    field: sleigh_runtime::Field,
    value: i64,
    lang: &SleighLanguage,
    theme: &CodeTheme,
) {
    let is_signed = field.signed || (lang.sleigh.default_space_size * 8 == field.num_bits);

    let fmt = pcode::NumericFormatter {
        value: value as u64,
        is_signed,
        is_hex: field.hex,
        num_bits: field.num_bits,
    };
    tokens.push(theme.make_rich(TokenType::NumericalLiteral, format!("{fmt}")));
}

fn parse_field(
    tokens: &mut Vec<RichText>,
    lang: &SleighLanguage,
    value: i64,
    field: sleigh_runtime::Field,
    theme: &CodeTheme,
) {
    let attachment = match field.attached {
        Some(id) => lang.sleigh.get_attachment(id),
        None => {
            parse_numeric_field(tokens, field, value, lang, theme);
            return;
        }
    };

    let idx = value as usize;
    match attachment {
        sleigh_runtime::AttachmentRef::Name(names) => {
            tokens.push(theme.make_rich(
                TokenType::Punctuation,
                lang.sleigh.get_str(*names.get(idx).unwrap()),
            ));
        }
        sleigh_runtime::AttachmentRef::Value(values) => {
            parse_numeric_field(tokens, field, *values.get(idx).unwrap(), lang, theme);
        }
        sleigh_runtime::AttachmentRef::Register(regs, _) => {
            let name = (*regs.get(idx).unwrap()).unwrap().name;
            tokens.push(theme.make_rich(TokenType::Symbol, lang.sleigh.get_str(name)));
        }
    }
}

fn parse_ctx(
    tokens: &mut Vec<RichText>,
    buffer: &mut String,
    ctx: SubtableCtx,
    lang: &SleighLanguage,
    theme: &CodeTheme,
    depth: u8,
) {
    use sleigh_runtime::DisplaySegment::*;
    for segment in ctx.display_segments() {
        match segment {
            Literal(idx) => {
                let s = lang.sleigh.get_str(*idx);
                if s == " "
                    || (s.len() > 0
                        && buffer.len() > 0
                        && s.chars().all(|c| c.is_alphanumeric())
                        && buffer.chars().all(|c| !c.is_alphabetic()))
                {
                    if buffer.len() > 0 {
                        if buffer.chars().all(|c| c.is_alphanumeric()) {
                            tokens.push(
                                theme.make_rich(TokenType::Keyword, buffer.to_ascii_lowercase()),
                            );
                            buffer.clear();
                        } else {
                            tokens.push(
                                theme.make_rich(TokenType::Punctuation, std::mem::take(buffer)),
                            );
                        }
                        if s != " " {
                            buffer.push_str(s);
                        }
                    }
                } else {
                    if s.len() > 0 {
                        let p = s
                            .chars()
                            .map(|c| format!("0x{:x}", c as u8))
                            .collect::<Vec<_>>()
                            .join(",");
                        buffer.push_str(s);
                    }
                }
            }
            Field(idx) => {
                let field = ctx.fields()[*idx as usize];
                let value = ctx.locals()[*idx as usize];
                if buffer.len() > 0 {
                    tokens.push(theme.make_rich(TokenType::Punctuation, std::mem::take(buffer)));
                }
                parse_field(tokens, lang, value, field, theme);
            }
            Subtable(idx) => {
                let ctx = ctx.visit_constructor(ctx.subtables()[*idx as usize]);
                parse_ctx(tokens, buffer, ctx, lang, theme, depth + 1);
            }
        }
    }
    if depth == 0 && buffer.len() > 0 {
        if tokens.len() > 0 {
            if buffer.chars().any(|c| c.is_alphanumeric()) {
                tokens.push(theme.make_rich(TokenType::Punctuation, std::mem::take(buffer)));
            } else {
                tokens.push(theme.make_rich(TokenType::Symbol, std::mem::take(buffer)));
            }
        } else {
            tokens.push(theme.make_rich(TokenType::Keyword, buffer.to_ascii_lowercase()));
            buffer.clear();
        }
    }
}

fn draw_line(
    theme: &CodeTheme,
    grid: &mut Ui,
    i: &Instruction,
    lang: &SleighLanguage,
    ir: Option<&BasicBlock>,
) {
    grid.label(theme.make_rich(TokenType::Punctuation, format!("{}", Address(i.inst_start))));

    let mut tokens = Vec::new();
    let mut buffer = String::new();
    let ctx = i.root(&lang.sleigh);

    parse_ctx(&mut tokens, &mut buffer, ctx, lang, theme, 0);

    grid.label(tokens[0].clone());

    horizontal(grid, |cell| {
        for token in tokens.drain(1..) {
            cell.label(token);
        }
    });

    if let Some(block) = ir {
        if let Some(text) = block
            .key_instructions
            .get(&i.inst_start.into())
            .and_then(|e| Some(e.with_sleigh_language(lang)))
        {
            grid.label(theme.make_rich(TokenType::Comment, format!(" ; {text}")));
        }
    }
    let min = grid.min_rect().width();
    let clip = grid.clip_rect().width();

    if clip > min {
        grid.allocate_at_least(
            Vec2 {
                x: clip - min,
                y: 16.0,
            },
            Sense::empty(),
        );
    }
    grid.end_row();
}

impl<'t, T> InstructionsView<'t, T>
where
    T: std::hash::Hash,
{
    const STROKE: Stroke = Stroke {
        width: 1.0,
        color: Color32::DARK_RED,
    };
    pub fn new(salt: T, theme: &'t CodeTheme, block: Option<&'t BasicBlock>) -> Self {
        Self { salt, theme, block }
    }

    pub fn draw(self, ui: &mut Ui, mem: &Memory, lang: &SleighLanguage, addr: Address) -> Response {
        ui.scope_builder(
            UiBuilder::new().sense(Sense::hover() | Sense::click()),
            |ui| {
                // ui.push_id(self.salt, |ui| {
                let grid = Grid::new(self.salt)
                    .striped(true)
                    .show(ui, |grid| {
                        if let Some(block) = self.block {
                            let literals = mem
                                .literal
                                .get_at_point(addr)
                                .expect("Unable to get memory literal");
                            let mut iter = literals.get_instructions().iter();

                            let i = iter
                                .find(|i| i.inst_start == addr.0)
                                .expect("Unable to find instruction at address");
                            // let basic_ir = block.instruction_map.get(&i.inst_start.into());
                            // let composed_ir = self.function.and_then(|f| f.composed_blocks.get_at_point(block.address).unwrap().instruction_map.get(&i.inst_start.into()));
                            draw_line(self.theme, grid, i, lang, Some(block));
                            while let Some(i) = iter.next() {
                                if block.identifier.contains(i.inst_start) {
                                    // let basic_ir = block.instruction_map.get(&i.inst_start.into());
                                    // let composed_ir = self.function.and_then(|f| f.composed_blocks.get_at_point(block.address).unwrap().instruction_map.get(&i.inst_start.into()));
                                    draw_line(self.theme, grid, i, lang, Some(block));
                                } else {
                                    break;
                                }
                            }
                        }
                    })
                    .response;
                if grid.contains_pointer() {
                    ui.painter().rect(
                        grid.interact_rect,
                        0.0,
                        Color32::from_black_alpha(0),
                        Self::STROKE,
                        egui::StrokeKind::Outside,
                    );
                }
                // })
            },
        )
        .response
    }
}

#[derive(Clone)]
pub struct BlockView {
    // pos:usize,
    theme: CodeTheme,
    addr_row_map: HashMap<Address, usize>,
    row_addr_map: Vec<Address>,
}

pub fn draw_bb(
    ui: &mut Ui,
    block: &BasicBlock,
    lang: &SleighLanguage,
    id_salt: impl std::hash::Hash,
) -> InnerResponse<()> {
    let header = ui.label("Known memory state at the end of this block:");
    let width = header.rect.width();
    Grid::new(id_salt).striped(true).show(ui, |ui| {
        for (addr, value) in &block.memory {
            ui.label(format!("{}", addr.with_sleigh_language(lang)));
            ui.label(":=");
            ui.label(format!("{}", value.with_sleigh_language(lang)));
            let min = ui.min_rect().width();

            if width > min {
                ui.allocate_at_least(
                    Vec2 {
                        x: width - min,
                        y: 16.0,
                    },
                    Sense::empty(),
                );
            }
            ui.end_row();
        }
    })
}

impl BlockView {
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
        }
    }

    pub fn draw(
        &mut self,
        ui: &mut Ui,
        signals: &mut TabSignals,
        mem: &Memory,
        lang: &SleighLanguage,
    ) {
        // TODO: Add whole file view:
        let mut area = ScrollArea::both().auto_shrink(false);
        if let Some(addr) = signals.is_requested_pos() {
            area = area.vertical_scroll_offset(
                (ui.spacing().interact_size.y + ui.spacing().item_spacing.y)
                    * self.addr_row_map[&addr] as f32,
            );
        }

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
                let mut current_addr = start_addr;
                while current_addr < end_addr {
                    let literal = mem.literal.get_at_point(current_addr).unwrap();
                    match &literal.kind {
                        crate::memory::LiteralKind::Data(data_kind) => todo!(),
                        crate::memory::LiteralKind::Instruction(instructions) => {
                            let mut current_block_span: Option<&BasicBlock> = None;
                            let mut current_function: Option<&HighFunction> = None;
                            for instr in instructions {
                                let mut should_draw = false;
                                if instr.inst_start >= current_addr.0
                                    && instr.inst_start < end_addr.0
                                {
                                    if let Some(block) = current_block_span {
                                        if !block.identifier.contains(instr.inst_start) {
                                            current_block_span =
                                                mem.ir.get_by_address(instr.inst_start);
                                            should_draw = true;
                                        }
                                    } else {
                                        current_block_span =
                                            mem.ir.get_by_address(instr.inst_start);
                                        should_draw = true;
                                    }

                                    if should_draw {
                                        // draw single line of the view
                                        let r = InstructionsView::new(
                                            instr.inst_start,
                                            &self.theme,
                                            current_block_span,
                                        )
                                        .draw(
                                            ui,
                                            mem,
                                            lang,
                                            instr.inst_start.into(),
                                        );
                                        r.on_hover_ui(|hover| {
                                            if let (Some(bb), Some(hf)) =
                                                (current_block_span, current_function)
                                            {
                                                let bb = hf
                                                    .composed_blocks
                                                    .get_by_identifier(bb.identifier)
                                                    .unwrap();
                                                draw_bb(hover, bb, lang, bb.identifier);
                                            }
                                        });
                                        current_addr = instr.inst_next.into();
                                    }
                                }
                            }
                        }
                    }
                }
            },
        );
    }
}

fn map_addr(mem: &Memory) -> (HashMap<Address, usize>, Vec<Address>) {
    let mut ar_map = HashMap::new();
    let mut ra_map = Vec::new();
    for (_, data) in mem.literal.iter() {
        match &data.kind {
            crate::memory::LiteralKind::Data(data_kind) => todo!(),
            crate::memory::LiteralKind::Instruction(instructions) => {
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
