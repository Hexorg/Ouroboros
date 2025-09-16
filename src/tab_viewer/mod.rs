use std::ops::Add;

use egui::Popup;
use egui_dock::tab_viewer::OnCloseResponse;

use crate::{ir::Address,  memory::Memory, DecompilerApp};

mod theme;
mod instruction_view;
mod decompiler;
mod bb_graph;

pub use theme::{CodeTheme, TokenType};

pub use instruction_view::{InstructionsView, BlockView, draw_bb};
pub use decompiler::{Decompiler};
pub use bb_graph::BlockGraph;

enum SignalKind {
    RequestPos(Address),
    Other
}

pub struct TabSignals{
    new_signals:Vec<SignalKind>,
    signals:Vec<SignalKind>,
}

impl TabSignals {
    pub fn new() -> Self {
        Self { 
            new_signals:Vec::new(),
            signals:Vec::new()
        }
    }

    pub fn is_requested_pos(&self) -> Option<Address> {
        self.signals.iter().find_map(|p| if let SignalKind::RequestPos(p) = p { Some(*p) } else { None })
    }

    pub fn request_pos(&mut self, addr:Address) {
        use SignalKind::RequestPos;
        self.new_signals.push(RequestPos(addr));
    }

    pub fn new_frame(&mut self) {
        let new = std::mem::take(&mut self.new_signals);
        let mut old = std::mem::take(&mut self.signals);
        self.signals = new;
        old.clear();
        self.new_signals = old;
    }
}

pub struct TabViewer<'m> {
    pub memory:&'m Memory<'m>,
    pub current_function: Option<Address>,
    pub signals: &'m mut TabSignals
}

#[derive(Clone)]
pub enum TabKind {
    ASM(BlockView),
    Decompiler(Decompiler),
    BlockGraph(BlockGraph),
    // Terminal(Terminal),
}

impl PartialEq for TabKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ASM(_), Self::ASM(_)) |
            (Self::BlockGraph(_), Self::BlockGraph(_)) => true,
            (Self::Decompiler(_), Self::Decompiler(_)) => true,
            _ => false,
        }
    }
}

impl Eq for TabKind {}

impl<'m> TabViewer<'m> {
    pub fn new(memory: &'m Memory, current_function:Option<Address>, signals:&'m mut TabSignals) -> Self {
        Self { memory, current_function, signals}
    }
}

impl<'m> egui_dock::TabViewer for TabViewer<'m> {
    type Tab = TabKind;

    fn title(&mut self, tab: &mut Self::Tab) -> egui::WidgetText {
        match tab {
            TabKind::ASM(_) => "Listing".into(),
            TabKind::Decompiler(d) => self.current_function.map(|f| format!("Decompile: FUN_{:x}", f.0)).unwrap_or("Decompile: No function".into()).into(),
            TabKind::BlockGraph(d) => self.current_function.map(|f| format!("Block graph: FUN_{:x}", f.0)).unwrap_or("Block graph: No function".into()).into()

        }
    }

    fn ui(&mut self, ui: &mut egui::Ui, tab: &mut Self::Tab) {
        match tab {
            TabKind::ASM(l) => {
                l.draw(ui, self.memory, &mut self.signals);
            },
            TabKind::Decompiler(b) => {
                b.hovered_set_now = false;
                b.draw(ui, self.memory, self.current_function, &mut self.signals);
                if !b.hovered_set_now {
                    b.hovered_symbol = None;
                }
            },
            TabKind::BlockGraph(b) => b.draw(ui, self.memory, self.current_function, &mut self.signals)
        }
    }
}