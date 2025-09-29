use egui::{Id, Ui};

use alacritty_terminal::{term::Config, Grid, Term};

#[derive(Clone)]
struct TermSignals {}

impl TermSignals {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Clone)]
pub struct TerminalView {
    id: Id,
}

impl TerminalView {
    pub fn new(ui: &mut Ui) -> Self {
        let id = Id::new("terminal");
        ui.ctx().memory_mut(|mem| {
            mem.data.insert_temp(
                id,
                Term::new(
                    Config::default(),
                    Grid::new(20, 80, 255),
                    TermSignals::new(),
                ),
            );
        });
        Self { id }
    }
    pub fn draw(&self, ui: &mut Ui) {
        ui.ctx().memory_mut(|mem| {
            mem.data.insert_temp(id, value);
        });
    }

    pub fn title(&self) -> &'static str {
        "Terminal"
    }
}
