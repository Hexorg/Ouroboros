use egui::{
    Color32, Grid, InnerResponse, Response, RichText, ScrollArea, Sense, Spacing, Stroke, Style,
    Ui, UiBuilder, Vec2,
};

use sleigh_compile::ldef::SleighLanguage;
use sleigh_runtime::{Instruction, SubtableCtx};

use super::{CodeTheme, TokenType};
use crate::{
    ir::{address::Address, basic_block::BasicBlock, expression::Expression},
    memory::{LiteralKind, LiteralState, Memory},
    tab_viewer::TabSignals,
};

pub struct InstructionBlockView<'t, T> {
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
) -> Response {
    let addr_lbl =
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
    addr_lbl
}

pub fn draw_bb(
    ui: &mut Ui,
    block: &BasicBlock,
    lang: &SleighLanguage,
    id_salt: impl std::hash::Hash,
) -> InnerResponse<()> {
    let header = ui.label("Known memory state at the end of this block:");
    let width = header.rect.width();
    let r = Grid::new(id_salt).striped(true).show(ui, |ui| {
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
    });
    let n = Expression::from(0);
    let esp = block
        .registers
        .get(lang.sp)
        .unwrap_or(&n)
        .with_sleigh_language(lang);
    ui.label(format!("ESP State: {esp}"));
    r
}

impl<'t, T> InstructionBlockView<'t, T>
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
                            let lbl = draw_line(self.theme, grid, i, lang, Some(block));

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

pub fn draw(
    theme: &CodeTheme,
    ui: &mut Ui,
    signals: &mut TabSignals,
    mem: &Memory,
    current_addr: &mut Address,
    mut end_addr: Address,
    state: &LiteralState,
) {
    if let LiteralKind::Instruction(size, instr) = &state.kind {
        let my_max_addr = state.addr.0 + *size as u64;
        if end_addr.0 > my_max_addr {
            end_addr.0 = my_max_addr;
        }
        println!("Drawing instructions from {current_addr} to {end_addr}");
        Grid::new(state.addr).striped(true).show(ui, |ui| {
            let mut iter = instr.iter();
            let i = iter
                .find(|i| i.inst_start == current_addr.0)
                .expect(&format!(
                    "Unable to find instruction at address {current_addr}"
                ));
            let block = mem
                .navigation
                .function_span
                .get_at_point(i.inst_start.into())
                .and_then(|a| mem.functions.get(a))
                .and_then(|hf| hf.composed_blocks.get_by_address(i.inst_start))
                .or_else(|| mem.ir.get_by_address(i.inst_start));
            let lbl = draw_line(theme, ui, i, &mem.lang, block);
            if lbl.contains_pointer() {
                ui.painter_at(lbl.rect).rect(
                    lbl.rect,
                    0.,
                    theme.highlight_color,
                    Stroke::NONE,
                    egui::StrokeKind::Inside,
                );
                if lbl.clicked() {
                    signals.request_pos(i.inst_start.into());
                }
                if ui.ctx().input(|r| r.key_pressed(egui::Key::F)) {
                    signals.define_function(i.inst_start.into());
                }
            }

            while let Some(i) = iter.next() {
                let block = mem
                    .navigation
                    .function_span
                    .get_at_point(i.inst_start.into())
                    .and_then(|a| mem.functions.get(a))
                    .and_then(|hf| hf.composed_blocks.get_by_address(i.inst_start))
                    .or_else(|| mem.ir.get_by_address(i.inst_start));
                let lbl = draw_line(theme, ui, i, &mem.lang, block);
                if lbl.contains_pointer() {
                    ui.painter_at(lbl.rect).rect(
                        lbl.rect,
                        0.,
                        theme.highlight_color,
                        Stroke::NONE,
                        egui::StrokeKind::Inside,
                    );
                    if ui.ctx().input(|r| r.key_pressed(egui::Key::F)) {
                        signals.define_function(i.inst_start.into());
                    }
                }
                if i.inst_start >= end_addr.0 {
                    break;
                }
            }
        });
        *current_addr = end_addr;
    } else {
        panic!("Unexpected state kind passed to ASM view");
    }
}
