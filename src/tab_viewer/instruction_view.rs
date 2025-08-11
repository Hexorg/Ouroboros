use std::{collections::HashMap, hash::{Hash, Hasher}};

use eframe::egui;
use egui::{scroll_area, Color32, Grid, Id, InnerResponse, LayerId, NumExt, Rect, Response, ScrollArea, Sense, Spacing, Stroke, Style, Ui, UiBuilder, Vec2};
use egui_extras::{Column, TableBuilder};
use iced_x86::{Code, Formatter, FormatterOutput, Instruction, NasmFormatter};
use nodit::InclusiveInterval;

use crate::{ir::{Address, BasicBlock, Expression, HighFunction}, memory::Memory, tab_viewer::TabSignals};
use super::{CodeTheme, TokenType};
pub struct InstructionsView<'t, T> {
    salt: T,
    theme:&'t CodeTheme,
    block:Option<&'t BasicBlock>,
    function:Option<&'t HighFunction>,
}

struct EguiFormatterOutput<'t>{
    theme:&'t CodeTheme,
    grid:&'t mut  Ui,
}

impl<'t> FormatterOutput for EguiFormatterOutput<'t> {
    fn write(&mut self, text: &str, kind: iced_x86::FormatterTextKind) {
        match kind {
            iced_x86::FormatterTextKind::Text => self.grid.label(self.theme.make_rich(TokenType::StringLiteral, text)),
            iced_x86::FormatterTextKind::Directive => self.grid.label(self.theme.make_rich(TokenType::Keyword, text)),
            iced_x86::FormatterTextKind::Prefix => self.grid.label(self.theme.make_rich(TokenType::Keyword, text)),
            iced_x86::FormatterTextKind::Mnemonic => self.grid.label(self.theme.make_rich(TokenType::Keyword, text)),
            iced_x86::FormatterTextKind::Keyword => self.grid.label(self.theme.make_rich(TokenType::Keyword, text)),
            iced_x86::FormatterTextKind::Operator => self.grid.label(self.theme.make_rich(TokenType::StringLiteral, text)),
            iced_x86::FormatterTextKind::Punctuation => self.grid.label(self.theme.make_rich(TokenType::Punctuation, text)),
            iced_x86::FormatterTextKind::Number => self.grid.label(self.theme.make_rich(TokenType::Symbol, text)),
            iced_x86::FormatterTextKind::Register => self.grid.label(self.theme.make_rich(TokenType::Punctuation, text)),
            iced_x86::FormatterTextKind::Decorator => self.grid.label(self.theme.make_rich(TokenType::Punctuation, text)),
            iced_x86::FormatterTextKind::SelectorValue => self.grid.label(self.theme.make_rich(TokenType::StringLiteral, text)),
            iced_x86::FormatterTextKind::LabelAddress => self.grid.label(self.theme.make_rich(TokenType::StringLiteral, text)),
            iced_x86::FormatterTextKind::FunctionAddress => self.grid.label(self.theme.make_rich(TokenType::Symbol, text)),
            iced_x86::FormatterTextKind::Data => self.grid.label(self.theme.make_rich(TokenType::Punctuation, text)),
            iced_x86::FormatterTextKind::Label => self.grid.label(self.theme.make_rich(TokenType::StringLiteral, text)),
            iced_x86::FormatterTextKind::Function => self.grid.label(self.theme.make_rich(TokenType::Symbol, text)),
            _ => self.grid.label(self.theme.make_rich(TokenType::Comment, text)),
        };
    }
}

fn horizontal<R>(ui:&mut Ui, add_contents: impl FnOnce(&mut Ui) -> R) -> InnerResponse<R> {
    let ui_builder = egui::UiBuilder::new().layout(egui::Layout::left_to_right(egui::Align::Min));
    let ui_builder = ui_builder.style(Style{spacing:Spacing{item_spacing:Vec2::new(0.0, 0.0), ..Default::default()}, ..Default::default()});
    ui.scope_builder(
            ui_builder,
            add_contents,
        )
}

fn draw_line(theme:&CodeTheme, grid:&mut Ui, i:&Instruction, formatter:&mut NasmFormatter, basic_expression:Option<&crate::ir::Expression>, composed_expression:Option<&crate::ir::Expression>) {
    
    grid.label(theme.make_rich(TokenType::Punctuation, format!("{}", Address(i.ip()))));
    
    
    horizontal(grid, |cell| {
        let mut egui = EguiFormatterOutput{theme, grid:cell};
        formatter.format_mnemonic_options(i, &mut egui, 0);
    });
    
   horizontal(grid, |operands| {
        let mut egui = EguiFormatterOutput{
            theme,
            grid:operands,
        };
        formatter.format_all_operands(i, &mut egui);
    });

    
    if let Some(e) = composed_expression {
        grid.label(theme.make_rich(TokenType::Comment, format!(" ; {e}")));
    } else {
        if let Some(e) = basic_expression {
            grid.label(theme.make_rich(TokenType::Comment, format!(" ; {e}")));
        } else {
            grid.label(theme.make_rich(TokenType::Comment, format!(" ; {:?}", i.code())));
        }
    }
    let min = grid.min_rect().width();
    let clip = grid.clip_rect().width();

    if clip > min {
        grid.allocate_at_least(Vec2 { x: clip-min, y: 16.0 }, Sense::empty());
    }
    grid.end_row();
}

impl<'t, T> InstructionsView<'t, T> where T:std::hash::Hash {
    const STROKE:Stroke = Stroke{width:1.0, color:Color32::DARK_RED};
    pub fn new(salt:T, theme:&'t CodeTheme, block:Option<&'t BasicBlock>, function:Option<&'t HighFunction>) -> Self {
        Self { salt, theme, block, function }
    }

    

    pub fn draw(self, ui: &mut Ui, mem:&Memory, addr:Address) -> Response {
        
        ui.scope_builder(UiBuilder::new().sense(Sense::hover() | Sense::click()), |ui| {
                // ui.push_id(self.salt, |ui| {
                let grid = Grid::new(self.salt)
                    .striped(true)
                    .show(ui, |grid| {
                    if let Some(block) = self.block {
                        let mut formatter = NasmFormatter::new();//with_options(mem.get_symbol_resolver(), None);
                        let literals = mem.literal.get_at_point(addr).expect("Unable to get memory literal");
                        let mut iter = literals.get_instructions().iter();
                        let i = iter.find(|i| i.ip() == addr.0).expect("Unable to find instruction at address");
                        let basic_ir = block.instruction_map.get(&i.ip().into());
                        let composed_ir = self.function.and_then(|f| f.composed_blocks.get_at_point(block.address).unwrap().instruction_map.get(&i.ip().into()));
                        draw_line(self.theme, grid, i, &mut formatter, basic_ir, composed_ir);
                        while let Some(i) = iter.next() {
                            if block.get_interval().contains_point(i.ip().into()) {
                                let basic_ir = block.instruction_map.get(&i.ip().into());
                                let composed_ir = self.function.and_then(|f| f.composed_blocks.get_at_point(block.address).unwrap().instruction_map.get(&i.ip().into()));
                                draw_line(self.theme, grid, i, &mut formatter, basic_ir, composed_ir);
                            } else {
                                break;
                            }
                        }
                    }
                }).response;
                if grid.contains_pointer() {
                    ui.painter().rect(grid.interact_rect, 0.0, Color32::from_black_alpha(0), Self::STROKE, egui::StrokeKind::Outside);
                }
            // })
        }).response
    }
}

#[derive(Clone)]
pub struct BlockView{
    // pos:usize,
    theme:CodeTheme,
    addr_row_map:HashMap<Address, usize>,
    row_addr_map:Vec<Address>,
}

pub fn draw_bb(ui:&mut Ui,block:&BasicBlock, id_salt:impl std::hash::Hash) -> InnerResponse<()> {
    let header = ui.label("Known memory state at the end of this block:");
    let width = header.rect.width();
    Grid::new(id_salt).striped(true).show(ui, |ui| {
        for (addr, value) in &block.memory {
            ui.label(format!("{addr}"));
            ui.label(":=");
            ui.label(format!("{value}"));
            let min = ui.min_rect().width();
        
            if width > min {
                ui.allocate_at_least(Vec2 { x: width-min, y: 16.0 }, Sense::empty());
            }
            ui.end_row();
        }
    })
}

impl BlockView {
    const STROKE:Stroke = Stroke{width:1.0, color:Color32::RED};

    /// Needs memory reference to figure out address to display row mapping.
    pub fn new(style: &Style, mem:&Memory) -> Self {
        let (addr_row_map, row_addr_map) = map_addr(mem);
        Self { theme: CodeTheme::from_style(style), addr_row_map, row_addr_map}
    }

    pub fn draw(&mut self, ui: &mut Ui, mem:&Memory, signals:&mut TabSignals) {
        // TODO: Add whole file view:
        let mut area = ScrollArea::both()
        .auto_shrink(false);
        if let Some(addr) = signals.is_requested_pos() {
            area = area.vertical_scroll_offset((ui.spacing().interact_size.y + ui.spacing().item_spacing.y) * self.addr_row_map[&addr] as f32);
        }

        area.show_rows(ui, ui.spacing().interact_size.y, self.addr_row_map.len(), |ui, row_range| {
            let start_addr = self.row_addr_map[row_range.start];
            let last_row = if row_range.end < self.row_addr_map.len() { row_range.end } else { self.row_addr_map.len() - 1};
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
                            if instr.ip() >= current_addr.0 && instr.ip() < end_addr.0 {
                                if let Some(block) = current_block_span  {
                                    if !block.get_interval().contains_point(instr.ip().into()) {
                                        current_block_span = mem.ir.get_at_point(instr.ip().into());
                                        current_function = current_block_span.and_then(|b| mem.functions.get(&b.parent_function));
                                        should_draw =  true;
                                    }
                                } else {
                                    current_block_span = mem.ir.get_at_point(instr.ip().into());
                                    current_function = current_block_span.and_then(|b| mem.functions.get(&b.parent_function));
                                    should_draw = true;
                                }

                                if should_draw { // draw single line of the view
                                    let r = InstructionsView::new(instr.ip(), &self.theme, current_block_span, current_function).draw(ui, mem, instr.ip().into());
                                    r.on_hover_ui(|hover| {
                                        if let (Some(bb), Some(hf))  = (current_block_span, current_function) {
                                            let bb = hf.composed_blocks.get_at_point(bb.address).unwrap();
                                            draw_bb(hover, bb, bb.address);
                                        }
                                    });
                                    current_addr = instr.next_ip().into();
                                }
                            }
    
                        }
                    }
                }
            }
        });
    }
}

fn map_addr(mem:&Memory) -> (HashMap<Address, usize>, Vec<Address>) {
    let mut ar_map = HashMap::new();
    let mut ra_map = Vec::new();
    for (_, data) in mem.literal.iter() {
        match &data.kind {
            crate::memory::LiteralKind::Data(data_kind) => todo!(),
            crate::memory::LiteralKind::Instruction(instructions) => {
                for instr in instructions {
                    let row = ar_map.len();
                    let addr = Address(instr.ip());
                    ar_map.insert(addr, row);
                    ra_map.push(addr);
                }
            }
        }
    }
    (ar_map, ra_map)
}