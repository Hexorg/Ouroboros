use sleigh_compile::ldef::SleighLanguage;

use crate::{
    ir::{address::Address, expression::VariableSymbol},
    memory::Memory,
};

mod bb_graph;
mod decompiler;
mod instruction_view;
// mod terminal;
mod theme;

pub use theme::{CodeTheme, TokenType};

pub use bb_graph::BlockGraph;
pub use decompiler::Decompiler;
pub use instruction_view::{draw_bb, MemoryView};
// pub use terminal::TerminalView;

pub enum SignalKind {
    RequestPos(Address),
    RenameSymbol(VariableSymbol, String),
}

impl std::fmt::Debug for SignalKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignalKind::RequestPos(address) => {
                f.write_fmt(format_args!("SignalKind::RequestPos({address})"))
            }
            SignalKind::RenameSymbol(variable_symbol, name) => f.write_fmt(format_args!(
                "SignalKind::RenameSymbol({variable_symbol}, {name})"
            )),
        }
    }
}

impl<'a> IntoIterator for &'a TabSignals {
    type Item = &'a SignalKind;

    type IntoIter = std::slice::Iter<'a, SignalKind>;
    fn into_iter(self) -> Self::IntoIter {
        self.signals.iter()
    }
}

pub struct TabSignals {
    new_signals: Vec<SignalKind>,
    signals: Vec<SignalKind>,
}

impl TabSignals {
    pub fn new() -> Self {
        Self {
            new_signals: Vec::new(),
            signals: Vec::new(),
        }
    }

    pub fn is_requested_pos(&self) -> Option<Address> {
        self.signals.iter().find_map(|p| {
            if let SignalKind::RequestPos(p) = p {
                Some(*p)
            } else {
                None
            }
        })
    }

    pub fn request_pos(&mut self, addr: Address) {
        use SignalKind::RequestPos;
        self.new_signals.push(RequestPos(addr));
    }

    pub fn rename_symbol(&mut self, symbol: VariableSymbol, name: String) {
        use SignalKind::RenameSymbol;
        self.new_signals.push(RenameSymbol(symbol, name))
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
    pub memory: &'m Memory<'m>,
    pub lang: &'m SleighLanguage,
    pub current_function: Option<Address>,
    pub signals: &'m mut TabSignals,
}

#[derive(Clone)]
pub enum TabKind {
    ASM(MemoryView),
    Decompiler(Decompiler),
    BlockGraph(BlockGraph),
    // Terminal(TerminalView),
}

impl PartialEq for TabKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ASM(_), Self::ASM(_)) | (Self::BlockGraph(_), Self::BlockGraph(_)) => true,
            (Self::Decompiler(_), Self::Decompiler(_)) => true,
            _ => false,
        }
    }
}

impl Eq for TabKind {}

impl<'m> TabViewer<'m> {
    pub fn new(
        memory: &'m Memory,
        current_function: Option<Address>,
        signals: &'m mut TabSignals,
        lang: &'m SleighLanguage,
    ) -> Self {
        Self {
            memory,
            current_function,
            signals,
            lang,
        }
    }
}

impl<'m> egui_dock::TabViewer for TabViewer<'m> {
    type Tab = TabKind;

    fn title(&mut self, tab: &mut Self::Tab) -> egui::WidgetText {
        match tab {
            TabKind::ASM(_) => "Listing".into(),
            TabKind::Decompiler(_) => self
                .current_function
                .map(|f| format!("Decompile: FUN_{:x}", f.0))
                .unwrap_or("Decompile: No function".into())
                .into(),
            TabKind::BlockGraph(_) => self
                .current_function
                .map(|f| format!("Block graph: FUN_{:x}", f.0))
                .unwrap_or("Block graph: No function".into())
                .into(),
            // TabKind::Terminal(t) => t.title().into(),
        }
    }

    fn ui(&mut self, ui: &mut egui::Ui, tab: &mut Self::Tab) {
        match tab {
            TabKind::ASM(l) => {
                l.draw(ui, &mut self.signals, self.memory, self.lang);
            }
            TabKind::Decompiler(b) => {
                b.hovered_set_now = false;
                b.draw(ui, self.memory, self.current_function, &mut self.signals);
                if !b.hovered_set_now {
                    b.hovered_symbol = None;
                }
            }
            TabKind::BlockGraph(b) => {
                b.draw(ui, self.memory, self.current_function, &mut self.signals)
            }
        }
    }
}
