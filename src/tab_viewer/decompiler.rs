use std::{borrow::Cow, collections::HashMap};

use egui::{text::LayoutJob, text_selection::LabelSelectionState, Color32, Frame, Grid, InnerResponse, Pos2, Rect, Response, RichText, Sense, Spacing, Stroke, Style, Ui, Vec2, Widget};

use crate::{ir::*, memory::Memory, tab_viewer::TabSignals};

use super::{CodeTheme, TokenType};
    

fn vertical<R>(ui:&mut Ui, add_contents: impl FnOnce(&mut Ui) -> R) -> InnerResponse<R> {
    let ui_builder = egui::UiBuilder::new().layout(egui::Layout::top_down(egui::Align::Min));
    let ui_builder = ui_builder.style(Style{spacing:Spacing{item_spacing:Vec2::new(0.0, 0.0), ..Default::default()}, ..Default::default()});
    ui.scope_builder(
            ui_builder,
            add_contents,
        )
}

#[derive(Clone)]
pub struct Decompiler{
    theme:CodeTheme,
    interned_tokens:HashMap<&'static str, RichText>,
    pub hovered_symbol:Option<String>,
    pub hovered_set_now:bool,
}


impl Decompiler {
    pub fn new(style: &Style) -> Self {
        let theme = CodeTheme::from_style(style);
        let interned_tokens = HashMap::from([
            ("if", theme.make_rich(TokenType::Keyword, "if ")),
            ("else", theme.make_rich(TokenType::Keyword, "else ")),
            ("while", theme.make_rich(TokenType::Keyword, "while ")),
            ("return", theme.make_rich(TokenType::Keyword, "return ")),
            ("deref", theme.make_rich(TokenType::Punctuation, "*")),
            ("(", theme.make_rich(TokenType::Punctuation, "(")),
            (")", theme.make_rich(TokenType::Punctuation, ")")),
            ("{", theme.make_rich(TokenType::Punctuation, "{")),
            ("}", theme.make_rich(TokenType::Punctuation, "}")),
            ("=", theme.make_rich(TokenType::Punctuation, " = ")),
            ("==", theme.make_rich(TokenType::Punctuation, " == ")),
            ("!=", theme.make_rich(TokenType::Punctuation, " != ")),
            ("+", theme.make_rich(TokenType::Punctuation, " + ")),
            ("-", theme.make_rich(TokenType::Punctuation, " - ")),
            (">", theme.make_rich(TokenType::Punctuation, " > ")),
            (">=", theme.make_rich(TokenType::Punctuation, " >= ")),
            ("<", theme.make_rich(TokenType::Punctuation, " < ")),
            ("<=", theme.make_rich(TokenType::Punctuation, " <= ")),
            ("<<", theme.make_rich(TokenType::Punctuation, " << ")),
            (">>", theme.make_rich(TokenType::Punctuation, " >> ")),
            ("&", theme.make_rich(TokenType::Punctuation, " & ")),
            ("|", theme.make_rich(TokenType::Punctuation, " | ")),
            (",", theme.make_rich(TokenType::Punctuation, ", ")),
            (";", theme.make_rich(TokenType::Punctuation, ";")),
            (" ", theme.make_rich(TokenType::Whitespace, " ")),
        ]);
        Self { theme, interned_tokens, hovered_symbol:None, hovered_set_now:false }
        
    }
    fn mk_color(&self, lbl: &'static str) -> RichText {
        self.interned_tokens[lbl].clone()
    }

    pub fn draw(&mut self, ui: &mut Ui, mem:&Memory, current_function:Option<Address>, signals:&mut TabSignals) {
        if let Some(addr) = current_function {
            let hf = mem.functions.get(&addr).unwrap();
            let entry = mem.ast.get(&addr).unwrap().entry();
            ui.spacing_mut().item_spacing = Vec2::ZERO;
            self.draw_at_pos(ui, signals, mem, hf, entry, 0);
        }
    }

    fn draw_at_pos(&mut self, ui:&mut Ui, signals: &mut TabSignals, mem:&Memory, hf:&HighFunction, stmt:&AstStatement, depth:u8) {
        let mut tab_prefix = String::with_capacity((depth*2) as usize);
        for _ in 0..(depth*2) { // 2 spaces per depth
            tab_prefix.push(' ');
        }
        let tab_prefix = self.theme.make_rich(TokenType::Whitespace, tab_prefix);

        match stmt {
            AstStatement::Block(v) => {
                for stmt in v {
                    self.draw_at_pos(ui, signals, mem, hf, stmt, depth);
                }
            }
            AstStatement::Nop => (),
            AstStatement::If{sese: pts, condition, true_statement, true_branch, else_statement, else_branch} => {
                let mut if_rect = Rect::NOTHING;
                ui.horizontal(|ui| {
                    ui.label(tab_prefix.clone());
                    let if_keyword = ui.label(self.mk_color("if"));
                    if_rect = if_keyword.rect;
                    if if_keyword.clicked() {
                        signals.request_pos(pts.0);
                    }
                    if if_keyword.hovered() {
                        if_keyword.highlight();
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                    ui.label(self.mk_color("("));
                    self.draw_expression(ui, signals, mem, hf, condition, *pts, condition.get_entry_point(), false);
                    ui.label(self.mk_color(")"));
                    ui.label(self.mk_color(" "));
                    let block_start = ui.label(self.mk_color("{"));
                    if block_start.clicked() {
                        signals.request_pos(*true_branch);
                    }
                    if block_start.hovered() {
                        block_start.highlight();
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                });
                self.draw_at_pos(ui, signals, mem, hf, true_statement, depth+1);
                ui.horizontal(|ui| {
                    ui.label(tab_prefix.clone());
                    let end_rect = ui.label(self.mk_color("}")).rect;
                    if depth != 0 {
                        ui.painter().line(vec![Pos2{x:if_rect.min.x+2., y:if_rect.max.y+2.}, Pos2{x:end_rect.min.x+2., y:end_rect.min.y-2.}], Stroke::new(1.0, Color32::DARK_GRAY));
                    }
                    if_rect = end_rect;
                    ui.label(self.mk_color(" "));
                    if !else_statement.is_nop() {
                        let else_keyword = ui.label(self.mk_color("else"));
                        if else_keyword.clicked() {
                            signals.request_pos(pts.0);
                        }
                        if else_keyword.hovered() {
                            else_keyword.highlight();
                            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                        }
                        let block_start = ui.label(self.mk_color("{"));
                        if block_start.clicked() {
                            signals.request_pos(*else_branch);
                        }
                        if block_start.hovered() {
                            block_start.highlight();
                            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                        }
                    }
                });
                self.draw_at_pos(ui, signals, mem, hf, else_statement, depth+1);
                if !else_statement.is_nop() {
                    ui.horizontal(|ui| {
                        ui.label(tab_prefix);
                        let else_end_rect = ui.label(self.mk_color("}")).rect;
                        if depth != 0 {
                            ui.painter().line(vec![Pos2{x:if_rect.min.x+2., y:if_rect.max.y+2.}, Pos2{x:else_end_rect.min.x+2., y:else_end_rect.min.y-2.}], Stroke::new(1.0, Color32::DARK_GRAY));
                        }
                    });
                }
            },
            AstStatement::Comment(c) => {
                ui.horizontal(|ui| {
                    ui.label(tab_prefix);
                    ui.label(self.theme.make_rich(TokenType::Comment, format!("// {c}")));
                });
            }
            AstStatement::MultilineComment(c) => {
                ui.horizontal(|ui| {
                    ui.label(tab_prefix);
                    ui.label(self.theme.make_rich(TokenType::Comment, format!("/*\n{c}\n*/")));
                });
            }
            AstStatement::Call { destination, params , call_from, sese: pts} => {
                let result = VariableSymbol::CallResult { call_from:*call_from, call_to:destination.clone() };
                ui.horizontal(|ui| {
                    ui.label(tab_prefix);
                    if let Cow::Borrowed(s) = resolve_symbol(mem, &result, hf, *pts) {
                        self.draw_symbol(ui, &result, true, mem, hf, *pts);
                        ui.label(self.mk_color("="));
                    }
                    self.draw_expression(ui, signals, mem, hf, destination, *pts, destination.get_entry_point(), true);
                    ui.label(self.mk_color("("));
                    for (idx, param) in params.iter().enumerate() {
                        self.draw_expression(ui, signals, mem, hf, param, *pts, param.get_entry_point(), false);
                        if idx < params.len() - 1 {
                            ui.label(self.mk_color(","));
                        }
                    }
                    ui.label(self.mk_color(")"));
                    ui.label(self.mk_color(";"));
                });
            }
            AstStatement::Assignment{sese: pts, destination,value} => {
                ui.horizontal(|ui| {
                    ui.label(tab_prefix);
                    self.draw_expression(ui, signals, mem, hf, destination, *pts, destination.get_entry_point(), false);
                    ui.label(self.mk_color("="));
                    self.draw_expression(ui, signals, mem, hf, value, *pts, value.get_entry_point(), false);
                    ui.label(self.mk_color(";"));
                });
            }
            AstStatement::Loop { sese: pts, condition, body, body_address } => {
                let mut loop_rect = Rect::NOTHING;
                ui.horizontal(|ui| {
                    ui.label(tab_prefix.clone());
                    let while_keyword = ui.label(self.mk_color("while"));
                    loop_rect = while_keyword.rect;
                    if while_keyword.clicked() {
                        signals.request_pos(pts.0);
                    }
                    if while_keyword.hovered() {
                        while_keyword.highlight();
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                    ui.label(self.mk_color("("));
                    self.draw_expression(ui, signals, mem, hf, condition, *pts, condition.get_entry_point(), false);
                    ui.label(self.mk_color(")"));
                    ui.label(self.mk_color(" "));
                    ui.label(self.mk_color("{"));
                });
                self.draw_at_pos(ui, signals, mem, hf, body, depth+1);
                ui.horizontal(|ui| {
                    ui.label(tab_prefix);
                    let end_rect = ui.label(self.mk_color("}")).rect;
                    if depth != 0 {
                        ui.painter().line(vec![Pos2{x:loop_rect.min.x+2., y:loop_rect.max.y+2.}, Pos2{x:end_rect.min.x+2., y:end_rect.min.y-2.}], Stroke::new(1.0, Color32::DARK_GRAY));
                    }
                });
            },
            AstStatement::Return { sese: pts, result } => {
                ui.horizontal(|ui| {
                    ui.label(tab_prefix);
                    ui.label(self.mk_color("return"));
                    // println!("Trying to resolve {result} ({result:?})");
                    self.draw_expression(ui, signals, mem, hf, result, *pts, result.get_entry_point(), false);
                    ui.label(self.mk_color(";"));
                });
            }
            AstStatement::Function { name, args, body} => {
                ui.horizontal(|ui| {
                    ui.label(tab_prefix);
                    ui.label(self.theme.make_rich(TokenType::Symbol, name));
                    ui.label(self.mk_color("("));
                    for (idx, arg) in args.iter().enumerate() {
                        self.draw_symbol(ui, &arg, true, mem, hf, hf.pts.root);
                        if idx < args.len() - 1 {
                            ui.label(self.mk_color(","));
                        }
                    }
                    ui.label(self.mk_color(")"));
                    ui.label(self.mk_color(" "));
                    ui.label(self.mk_color("{"));
                });
                self.draw_at_pos(ui, signals, mem, hf, body, depth+1);
                ui.label(self.mk_color("}"));
            },
            a => {
                ui.horizontal(|ui| {
                    ui.label(tab_prefix);
                    ui.label(self.theme.make_rich(TokenType::Comment, format!("// unsupported statement: {a:?}")));
                });
            }
        }
    }

    fn draw_dereference(&mut self, ui:&mut Ui, signals: &mut TabSignals, mem:&Memory, hf:&HighFunction, e:&Expression, ip_block:SingleEntrySingleExit<Address>, pos:usize) {
        match &e[pos] {
            ExpressionOp::Value(v) => {
                let mut label = self.draw_symbol(ui, &VariableSymbol::Ram(Expression::from(*v)), false, mem, hf, ip_block);
                if label.clicked() {
                    signals.request_pos(Address(*v as u64));
                }
                label = label.on_hover_text("Deference::Value");
                if label.hovered() {
                    label.highlight();
                    ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                }
            },
            ExpressionOp::Variable(v) => {
                ui.label(self.mk_color("deref"));
                let label = self.draw_symbol(ui, v, false, mem, hf, ip_block);
                label.on_hover_text("Deference::Variable");
            },
            a => {
                let var = VariableSymbol::Ram(e.get_sub_expression(pos));
                if let Cow::Borrowed(sym) = resolve_symbol(mem, &var, hf, ip_block) {
                    let label = self.draw_symbol(ui, &var, false, mem, hf, ip_block);
                    label.on_hover_text("Deference::ComplexExpression");
                } else {
                    ui.label(self.mk_color("deref"));
                    ui.label(self.mk_color("("));
                    ui.label(self.theme.make_rich(TokenType::Symbol, format!("Todo:Deref{a:?}")));
                    ui.label(self.mk_color(")"));
                }

            }
        }
    }

    fn draw_symbol(&mut self, ui:&mut Ui, symbol:&VariableSymbol, is_declaration:bool, mem:&Memory, hf:&HighFunction, ip_block:SingleEntrySingleExit<Address>) -> Response {
        let sym = resolve_symbol(mem, &symbol, hf, ip_block);
        if is_declaration {
            ui.label(self.theme.make_rich(TokenType::Type, sym.kind.name.clone()));
        }
        let mut lbl = None;
        if let Some(h) = &self.hovered_symbol {
            if h == &sym.name {
                lbl = Some(Frame::new().fill(Color32::LIGHT_YELLOW).show(ui, |ui| {
                    ui.label(self.theme.make_rich(TokenType::Type, sym.name.clone()))
                }).inner);
            }
        }

        let lbl = lbl.unwrap_or_else(|| ui.label(self.theme.make_rich(TokenType::Symbol, sym.name.clone())));
        let selection = LabelSelectionState::load(ui.ctx());
        if ui.rect_contains_pointer(lbl.rect) && !selection.has_selection() {
            if !self.hovered_symbol.as_ref().and_then(|s| Some(s == &sym.name)).unwrap_or(false) {
                self.hovered_symbol = Some(sym.name.clone());
            }
            self.hovered_set_now = true;
        }
        lbl
    }


    fn draw_expression(&mut self, ui:&mut Ui, signals: &mut TabSignals, mem:&Memory, hf:&HighFunction, e:&Expression, ip_block:SingleEntrySingleExit<Address>, pos:usize, is_call:bool) {
        match &e[pos] {
            ExpressionOp::Dereference(d) => {
                self.draw_dereference(ui, signals, mem, hf, e, ip_block, *d);
            },
            ExpressionOp::Equals(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color("=="));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call)

            },
            ExpressionOp::Value(v) => {
                
                if is_call {
                    let mut label = self.draw_symbol(ui, &VariableSymbol::Ram(Expression::from(*v)), false, mem, hf, ip_block);
                    if label.clicked() {
                        signals.request_pos(Address(*v as u64));
                    }
                    label = label.on_hover_text("Expression::Value");
                    if label.hovered() {
                        label.highlight();
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                } else {
                    ui.label(self.theme.make_rich(TokenType::NumericalLiteral, if *v < 1024 { format!("{v}") } else { format!("0x{v:x}")} ));
                }
            },
            ExpressionOp::Variable(v) => {
                let label = self.draw_symbol(ui, v, false, mem, hf, ip_block);
                label.on_hover_text("Expression::Variable");
            },
            ExpressionOp::NotEquals(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color("!="));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::Add(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color("+"));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::Sub(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color("-"));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::And(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color("&"));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::BitShiftLeft(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color("<<"));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::BitShiftRight(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color(">>"));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::Greater(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color(">"));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::GreaterOrEquals(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color(">="));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::Less(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color("<"));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            },
            ExpressionOp::LessOrEquals(l, r) => {
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *l, is_call);
                ui.label(self.mk_color("<="));
                self.draw_expression(ui, signals, mem, hf, e, ip_block, *r, is_call);
            }
            op => _ = ui.label(self.theme.make_rich(TokenType::Symbol, format!("TODO:Draw{op:?}"))),
        }
    }
}

fn resolve_symbol<'a>(mem:&'a Memory, dst:&VariableSymbol, hf:&'a HighFunction, ip_block:SingleEntrySingleExit<Address>) -> Cow<'a, VariableDefinition> {
    if let Some(def) = mem.symbols.resolve(dst)
        .or_else(|| mem.ast.get(&hf.cfg.start).unwrap().scope.get_symbol(ip_block, dst)) 
    {
        Cow::Borrowed(def)
    } else {
        Cow::Owned(VariableDefinition { kind: VariableType { name: "void *".to_owned() }, name: format!("unresolved_({dst})",), variable: dst.clone() })
    }
}