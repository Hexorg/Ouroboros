// mod listing;

mod test;
mod symbol_resolver;
mod memory;
mod ir;

use std::{collections::BTreeMap, ops::Add, pin::Pin, sync::Arc};

use egui::{text::LayoutJob, Color32, FontFamily, FontId, Key, Layout, RichText, Sense, Stroke, TextStyle, Visuals};

mod tab_viewer;
use iced_x86::Register;
// use listing::Listing;

use eframe::egui;

use egui_dock::{DockArea, DockState, NodeIndex, Style, SurfaceIndex};

use crate::{ir::{lift, Address, Expression, ExpressionOp, HighFunction, VariableDefinition, VariableSymbol}, memory::{LiteralState, Memory}, symbol_resolver::SymbolTable, tab_viewer::{BlockGraph, BlockView, Decompiler, SignalKind, TabKind, TabSignals, TabViewer}};



struct DecompilerApp<'s> {
    memory: Memory<'s>,
    current_function:Option<Address>,
    signals: TabSignals,
    tree: DockState<TabKind>,
    buttons: [(&'static str, TabKind); 3]
}

fn main() -> eframe::Result {
    // env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1024., 768.0]),
        ..Default::default()
    };
    let mut memory = Memory::new();
    let state = LiteralState::from_machine_code(test::EXAMPLE_CODE, test::EXAMPLE_CODE_BITNESS, test::EXAMPLE_CODE_RIP);
    let ir = lift(state.get_instructions());
    let f_start = Address(test::EXAMPLE_CODE_RIP);
    memory.literal.insert_strict(state.get_interval(), state).unwrap();
    memory.ir = ir;


    let hf = HighFunction::from_mem(f_start, &memory);
    hf.fill_global_symbols(&mut memory);
    let pts = hf.pts.pretty_print(&|sese, tabs| {
        let mut result = String::new();
        let start = hf.composed_blocks.get_at_point(hf.cfg.start).unwrap();
        for nbr in start.iter_function(&hf.composed_blocks) {
            if hf.pts.get_section(nbr.address) == Some(sese) {
                result.push_str(&format!("{tabs}{nbr:?}\n"));
            }
        }
        result.pop();
        result
    });

    println!("PTS:\n{pts}");

    // memory.symbols.add(test::EXAMPLE_CODE_RIP, "openPAK".to_owned());
    // memory.symbols.add(0x611084, "pakFilename".to_owned());
    // memory.symbols.add(0x669218, "pakFilename2".to_owned());
    // memory.symbols.add(0x6128a0, "propName".to_owned());
    // memory.symbols.add(0x4b07f0, "get_pak_cache".to_owned());
    // memory.symbols.add(0x4b0730, "logPAK".to_owned());
    // memory.symbols.add(0x4a1310, "file_OpenEx".to_owned());
    // memory.symbols.add(0x04a1490, "read_last_open_file_into".to_owned());
    // memory.symbols.add(0x49C5C0, "mem_newBlock_inSpecialSegment".to_owned());

    // memory.symbols.add(0x4BCDD0, "_strncpy".to_owned());
    // memory.symbols.add(0x668F94, "pakFlags".to_owned());
    // memory.symbols.add(0x668fcc, "isDebugPak".to_owned());


    // memory.symbols.add(0x4A1220, "close_last_open_fd_if_no_error".to_owned());
    // memory.symbols.add(0x4B0760, "setPalData".to_owned());
    // memory.symbols.add(0x62C124, "IS_READ_PAK_WHOLE_MAYBE".to_owned());
    // memory.symbols.add(0x668F90, "palData".to_owned());
    // memory.symbols.add(0x4A2E40, "setStatics".to_owned());
    // memory.symbols.add(0x52e268, "USER32.DLL::MessageBoxA".to_owned());

    // memory.symbols.add(0x4affb0, "fixObjectPointers".to_owned());
    // memory.symbols.add(0x4b1450, "toRenderable".to_owned());
    // memory.symbols.add(0x49c600, "mem_FreeMem_inSpecialSegment".to_owned());
    // memory.symbols.add(0x4b2370, "curious_s15f16_math".to_owned());
    // memory.symbols.add(0x4afed0, "ui_scaling_related".to_owned());
    // memory.symbols.add(0x4b0840, "cache_pak".to_owned());
    // memory.symbols.add(0x4b0680, "loadTextures".to_owned());
    // memory.symbols.add(0x4ada90, "SetStaticsAndReturnPalCase_1to2_2to3".to_owned());
    // memory.symbols.add(0x4a2e80, "SET_PAK_RELATED_TO_NULL".to_owned());
    // memory.symbols.add(0x4a14f0, "seek_file".to_owned());


    let mut ast = hf.build_ast(&memory);
    

    let var_esp = Expression::from(VariableSymbol::Register(Register::ESP));

    let mut param_1 = var_esp.clone();
    param_1.add_value(4);
    param_1.dereference();

    let mut param_2 = var_esp.clone();
    param_2.add_value(8);
    param_2.dereference();

    let mut param_3 = var_esp;
    param_3.add_value(12);
    param_3.dereference();

    let stack_4 = VariableSymbol::Ram(Expression::from(vec![ExpressionOp::Variable(VariableSymbol::Register(Register::ESP)), ExpressionOp::Value(4), ExpressionOp::Add(0, 1)]));
    let stack_8 = VariableSymbol::Ram(Expression::from(vec![ExpressionOp::Variable(VariableSymbol::Register(Register::ESP)), ExpressionOp::Value(8), ExpressionOp::Add(0, 1)]));
    let stack_12 = VariableSymbol::Ram(Expression::from(vec![ExpressionOp::Variable(VariableSymbol::Register(Register::ESP)), ExpressionOp::Value(12), ExpressionOp::Add(0, 1)]));
    ast.scope.add(hf.pts.root, stack_4.clone(), VariableDefinition::new("char *".to_owned(), "filename_a".to_owned(), stack_4));
    ast.scope.add(hf.pts.root, stack_8.clone(), VariableDefinition::new("char *".to_owned(), "filename_b".to_owned(), stack_8));
    ast.scope.add(hf.pts.root, stack_12.clone(), VariableDefinition::new("char *".to_owned(), "filename_c".to_owned(), stack_12));

    memory.ast.insert(f_start, ast);
    memory.functions.insert(f_start, hf);

    

    eframe::run_native(
        "Ouroboros",
        options,
        Box::new(|cc| {
            let style = &cc.egui_ctx.style();
            let buttons = [
            ("Listing", TabKind::ASM(BlockView::new(style, &memory))),
            ("Decompiler", TabKind::Decompiler(Decompiler::new(style))),
            ("Block Graph", TabKind::BlockGraph(BlockGraph::new())),
            ];


            let mut tree = DockState::new(vec![TabKind::ASM(BlockView::new(style, &memory))]);
            tree.split((SurfaceIndex(0), NodeIndex(0)), egui_dock::Split::Right, 0.5, egui_dock::Node::leaf(TabKind::Decompiler(Decompiler::new(style))));


            

            Ok(Box::new(DecompilerApp{
                current_function: Some(f_start),
                memory,
                tree,
                signals:TabSignals::new(),
                buttons,
            }))
        }),
    )
}






impl<'s> eframe::App for DecompilerApp<'s> {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("menu_panel").show(ctx, |panel| {
            let menu_bar = egui::MenuBar::new().ui(panel, |ui| {
                let file = ui.menu_button("File", |file_ui| {
                    
                    let open = file_ui.button("Open...");
                    if open.clicked() || file_ui.ctx().input(|i| i.key_pressed(Key::O)) {
                        println!("Open file");
                    }
                    
                });
                // if ui.ctx().input(|i| i.key_pressed(Key::F)) {
                //     file.response.request_focus();
                //     todo!("Make sure a TextEdit isn't accepting input somewhere")
                // }
                ui.menu_button("Windows", |windows_ui| {
                    let style = windows_ui.style();
                    
                    for (name, tab) in &self.buttons {
                        if windows_ui.button(*name).clicked() {
                            if let Some(index) = self.tree.find_tab_from(|p| p == tab) {
                                self.tree.set_active_tab(index);
                            } else {
                                self.tree.main_surface_mut().push_to_focused_leaf(tab.clone());
                            }
                        }
                    }
                });
            });
            menu_bar.response.ctx.enable_accesskit();
        });
        egui::TopBottomPanel::bottom("status_bar").show(ctx,|panel| {
            panel.label("Status bar...");
        });

        self.signals.new_frame();
        
        let mut tab_viewer = TabViewer::new(&self.memory, self.current_function, &mut self.signals);
        DockArea::new(&mut self.tree)
            .style(Style::from_egui(ctx.style().as_ref()))
            .show(ctx, &mut tab_viewer);

        for signal in &self.signals {
            use SignalKind::*;
            match signal {
                RequestPos(_) => (),
                RenameSymbol(var, name) => {
                    self.memory.symbols.resolve_mut(var).and_then(|v| Some(v.name = name.clone())).or_else(|| {
                        self.current_function
                            .and_then(|f| self.memory.ast.get_mut(&f))
                            .and_then(|ast| {
                                let section = ast.scope.find_owning_section(var).unwrap();
                                ast.scope.get_symbol_mut(section, var).unwrap().name = name.clone();
                                Some(())
                    })
                    });
                },
                a => println!("Process signal: {a:?}"),
            }
        }

    }
    
}
