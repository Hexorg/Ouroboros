use sleigh_compile::ldef::SleighLanguage;

use crate::{
    ir::{address::Address, expression::VariableSymbol},
    memory::Memory,
};

mod bb_graph;
mod decompiler;
mod memory_view;
mod navigation;
mod section_list;
// mod terminal;
mod theme;

pub use theme::{CodeTheme, TokenType};

pub use bb_graph::BlockGraph;
pub use decompiler::Decompiler;
pub use memory_view::MemoryView;
pub use navigation::NavigationView;
pub use section_list::SectionListView;
// pub use terminal::TerminalView;

pub enum SignalKind {
    NewOpenFile,
    RequestPos(Address),
    RenameSymbol(VariableSymbol, String),
    MarkInstruction(Address),
    DefineFunctionStart(Address),
    RepopulateInstructionRows,
}

impl std::fmt::Debug for SignalKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NewOpenFile => f.write_str("SignalKind::NewOpenFile"),
            SignalKind::RequestPos(address) => {
                f.write_fmt(format_args!("SignalKind::RequestPos({address})"))
            }
            SignalKind::RenameSymbol(variable_symbol, name) => f.write_fmt(format_args!(
                "SignalKind::RenameSymbol({variable_symbol}, {name})"
            )),
            Self::MarkInstruction(a) => {
                f.write_fmt(format_args!("SignalKind::MarkInstruction({a})"))
            }
            Self::DefineFunctionStart(a) => {
                f.write_fmt(format_args!("SignalKind::DefineFunctionStart({a})"))
            }
            Self::RepopulateInstructionRows => f.write_str("SignalKind::RepopulateInstructionRows"),
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

    pub fn is_new_file(&self) -> bool {
        self.signals
            .iter()
            .any(|s| matches!(s, SignalKind::NewOpenFile))
    }

    pub fn is_repopulate_instruction_rows(&self) -> bool {
        self.signals
            .iter()
            .any(|s| matches!(s, SignalKind::RepopulateInstructionRows))
    }

    pub fn is_mark_instruction(&self) -> Option<Address> {
        self.signals.iter().find_map(|p| {
            if let SignalKind::MarkInstruction(p) = p {
                Some(*p)
            } else {
                None
            }
        })
    }

    pub fn is_define_funtion(&self) -> Option<Address> {
        self.signals.iter().find_map(|p| {
            if let SignalKind::DefineFunctionStart(p) = p {
                Some(*p)
            } else {
                None
            }
        })
    }

    pub fn announce_new_file(&mut self) {
        use SignalKind::NewOpenFile;
        self.new_signals.push(NewOpenFile);
    }

    pub fn repopulate_instruction_rows(&mut self) {
        use SignalKind::RepopulateInstructionRows;
        self.new_signals.push(RepopulateInstructionRows);
    }

    pub fn mark_instruction(&mut self, addr: Address) {
        use SignalKind::MarkInstruction;
        self.new_signals.push(MarkInstruction(addr));
    }

    pub fn define_function<A: Into<Address>>(&mut self, addr: A) {
        use SignalKind::DefineFunctionStart;
        self.new_signals.push(DefineFunctionStart(addr.into()));
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
    pub memory: &'m Memory,
    pub current_function: Option<Address>,
    pub signals: &'m mut TabSignals,
}

#[derive(Clone)]
pub enum TabKind {
    MemoryView(MemoryView),
    Decompiler(Decompiler),
    BlockGraph(BlockGraph),
    Sections(SectionListView),
    Navigation(NavigationView),
    // Terminal(TerminalView),
}

impl PartialEq for TabKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::MemoryView(_), Self::MemoryView(_))
            | (Self::BlockGraph(_), Self::BlockGraph(_)) => true,
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
    ) -> Self {
        Self {
            memory,
            current_function,
            signals,
        }
    }
}

impl<'m> egui_dock::TabViewer for TabViewer<'m> {
    type Tab = TabKind;

    fn title(&mut self, tab: &mut Self::Tab) -> egui::WidgetText {
        match tab {
            TabKind::Sections(_) => "Section List".into(),
            TabKind::MemoryView(m) => m.title(),
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
            TabKind::Navigation(_) => "Navigation".into(),
            // TabKind::Terminal(t) => t.title().into(),
        }
    }

    fn ui(&mut self, ui: &mut egui::Ui, tab: &mut Self::Tab) {
        match tab {
            TabKind::MemoryView(l) => {
                l.draw(ui, &mut self.signals, self.memory);
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
            TabKind::Sections(s) => s.draw(ui, &mut self.signals, self.memory),
            TabKind::Navigation(n) => n.draw(ui, &mut self.signals, self.memory),
        }
    }

    fn id(&mut self, tab: &mut Self::Tab) -> egui::Id {
        match tab {
            TabKind::MemoryView(_) => "MemoryView".into(),
            TabKind::Decompiler(_) => "Decompiler".into(),
            TabKind::BlockGraph(_) => "BlockGraph".into(),
            TabKind::Sections(_) => "Sections".into(),
            TabKind::Navigation(_) => "Navigation".into(),
        }
    }
}
