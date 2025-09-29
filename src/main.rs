mod ir;
mod memory;
mod symbol_resolver;
mod test;

use egui::Key;
use smallvec::smallvec;
mod tab_viewer;

use eframe::egui;

use egui_dock::{DockArea, DockState, NodeIndex, Style, SurfaceIndex};

use ir::{
    address::Address,
    expression::{Expression, ExpressionOp, InstructionSize, VariableSymbol},
    high_function::HighFunction,
    scope::VariableDefinition,
    type_system::VariableType,
};
use memory::{LiteralState, Memory};
use sleigh_compile::ldef::SleighLanguage;
use tab_viewer::{BlockGraph, Decompiler, MemoryView, SignalKind, TabKind, TabSignals, TabViewer};

struct DecompilerApp<'s> {
    memory: Memory<'s>,
    current_function: Option<Address>,
    signals: TabSignals,
    lang: SleighLanguage,
    tree: DockState<TabKind>,
    buttons: [(&'static str, TabKind); 3],
}

fn main() -> eframe::Result {
    // ir::lift_with_sleigh(test::EXAMPLE_CODE_RIP, test::EXAMPLE_CODE);
    // env_lo::init(); // Log to stderr (if you run with `RUST_LOG=debug`).
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1280., 768.0]),
        ..Default::default()
    };
    let mut memory = Memory::new();
    let lang = sleigh_compile::SleighLanguageBuilder::new(
        "./Ghidra/Processors/x86/data/languages/x86.ldefs",
        "x86:LE:32:default",
    )
    .build()
    .unwrap();
    let state = LiteralState::from_machine_code(test::EXAMPLE_CODE, test::EXAMPLE_CODE_RIP, &lang);

    let f_start = Address(test::EXAMPLE_CODE_RIP);
    memory.ir = ir::lift(state.get_instructions(), &lang);
    memory
        .literal
        .insert_strict(state.get_interval(), state)
        .unwrap();

    let hf = HighFunction::from_mem(f_start, &memory, &lang);
    hf.take_interval_ownership(&mut memory.function_span);
    hf.fill_global_symbols(&mut memory);
    let mut ast = hf.build_ast(&memory, &lang);

    memory
        .symbols
        .add(test::EXAMPLE_CODE_RIP, 4, "openPAK".to_owned());
    memory.symbols.add(0x611084, 4, "pakFilename".to_owned());
    memory.symbols.add(0x669218, 4, "pakFilename2".to_owned());
    memory.symbols.add(0x6128a0, 4, "propName".to_owned());
    memory.symbols.add(0x4b07f0, 4, "get_pak_cache".to_owned());
    memory.symbols.add(0x4b0730, 4, "logPAK".to_owned());
    memory.symbols.add(0x4a1310, 4, "file_OpenEx".to_owned());
    memory
        .symbols
        .add(0x04a1490, 4, "read_last_open_file_into".to_owned());
    memory
        .symbols
        .add(0x49C5C0, 4, "mem_newBlock_inSpecialSegment".to_owned());

    memory.symbols.add(0x4BCDD0, 4, "_strncpy".to_owned());
    memory.symbols.add(0x668F94, 4, "pakFlags".to_owned());
    memory.symbols.add(0x668fcc, 4, "isDebugPak".to_owned());

    memory
        .symbols
        .add(0x4A1220, 4, "close_last_open_fd_if_no_error".to_owned());
    memory.symbols.add(0x4B0760, 4, "setPalData".to_owned());
    memory
        .symbols
        .add(0x62C124, 4, "IS_READ_PAK_WHOLE_MAYBE".to_owned());
    memory.symbols.add(0x668F90, 4, "palData".to_owned());
    memory.symbols.add(0x4A2E40, 4, "setStatics".to_owned());
    memory
        .symbols
        .add(0x52e268, 4, "USER32.DLL::MessageBoxA".to_owned());

    memory
        .symbols
        .add(0x4affb0, 4, "fixObjectPointers".to_owned());
    memory.symbols.add(0x4b1450, 4, "toRenderable".to_owned());
    memory
        .symbols
        .add(0x49c600, 4, "mem_FreeMem_inSpecialSegment".to_owned());
    memory
        .symbols
        .add(0x4b2370, 4, "curious_s15f16_math".to_owned());
    memory
        .symbols
        .add(0x4afed0, 4, "ui_scaling_related".to_owned());
    memory.symbols.add(0x4b0840, 4, "cache_pak".to_owned());
    memory.symbols.add(0x4b0680, 4, "loadTextures".to_owned());
    memory.symbols.add(
        0x4ada90,
        4,
        "SetStaticsAndReturnPalCase_1to2_2to3".to_owned(),
    );
    memory
        .symbols
        .add(0x4a2e80, 4, "SET_PAK_RELATED_TO_NULL".to_owned());
    memory.symbols.add(0x4a14f0, 4, "seek_file".to_owned());

    // let mut ast = hf.build_ast(&memory, &lang);

    let var_esp = Expression::from(VariableSymbol::Varnode(lang.sp));

    let mut param_1 = var_esp.clone();
    param_1.add_value(4, InstructionSize::U32);
    param_1.dereference();

    let mut param_2 = var_esp.clone();
    param_2.add_value(8, InstructionSize::U32);
    param_2.dereference();

    let mut param_3 = var_esp;
    param_3.add_value(12, InstructionSize::U32);
    param_3.dereference();

    let stack_4 = VariableSymbol::Ram(
        Box::new(Expression::from(smallvec![
            ExpressionOp::var_reg(lang.sp),
            ExpressionOp::Value(4),
            ExpressionOp::Add(0, 1, InstructionSize::U32),
        ])),
        4,
    );
    let stack_8 = VariableSymbol::Ram(
        Box::new(Expression::from(smallvec![
            ExpressionOp::var_reg(lang.sp),
            ExpressionOp::Value(8),
            ExpressionOp::Add(0, 1, InstructionSize::U32),
        ])),
        4,
    );
    let stack_12 = VariableSymbol::Ram(
        Box::new(Expression::from(smallvec![
            ExpressionOp::var_reg(lang.sp),
            ExpressionOp::Value(12),
            ExpressionOp::Add(0, 1, InstructionSize::U32),
        ])),
        4,
    );
    ast.scope.add(
        hf.pts.root,
        stack_4.clone(),
        VariableDefinition::new(
            VariableType::Pointer(Box::new(VariableType::Char)),
            "filename_a".to_owned(),
            stack_4,
        ),
    );
    ast.scope.add(
        hf.pts.root,
        stack_8.clone(),
        VariableDefinition::new(
            VariableType::Pointer(Box::new(VariableType::Char)),
            "filename_b".to_owned(),
            stack_8,
        ),
    );
    ast.scope.add(
        hf.pts.root,
        stack_12.clone(),
        VariableDefinition::new(
            VariableType::Pointer(Box::new(VariableType::Char)),
            "filename_c".to_owned(),
            stack_12,
        ),
    );

    memory.ast.insert(f_start, ast);
    memory.functions.insert(f_start, hf);

    eframe::run_native(
        "Ouroboros",
        options,
        Box::new(|cc| {
            let style = &cc.egui_ctx.style();
            let buttons = [
                ("Listing", TabKind::ASM(MemoryView::new(style, &memory))),
                ("Decompiler", TabKind::Decompiler(Decompiler::new(style))),
                ("Block Graph", TabKind::BlockGraph(BlockGraph::new())),
            ];

            let mut tree = DockState::new(vec![TabKind::ASM(MemoryView::new(style, &memory))]);
            tree.split(
                (SurfaceIndex(0), NodeIndex(0)),
                egui_dock::Split::Right,
                0.5,
                egui_dock::Node::leaf(TabKind::Decompiler(Decompiler::new(style))),
            );

            Ok(Box::new(DecompilerApp {
                current_function: Some(f_start),
                memory,
                tree,
                lang,
                signals: TabSignals::new(),
                buttons,
            }))
        }),
    )
}

impl<'s> eframe::App for DecompilerApp<'s> {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("menu_panel").show(ctx, |panel| {
            let menu_bar = egui::MenuBar::new().ui(panel, |ui| {
                ui.menu_button("File", |file_ui| {
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
                    // let style = windows_ui.style();

                    for (name, tab) in &self.buttons {
                        if windows_ui.button(*name).clicked() {
                            if let Some(index) = self.tree.find_tab_from(|p| p == tab) {
                                self.tree.set_active_tab(index);
                            } else {
                                self.tree
                                    .main_surface_mut()
                                    .push_to_focused_leaf(tab.clone());
                            }
                        }
                    }
                });
            });
            menu_bar.response.ctx.enable_accesskit();
        });
        egui::TopBottomPanel::bottom("status_bar").show(ctx, |panel| {
            panel.label("Status bar...");
        });

        self.signals.new_frame();

        let mut tab_viewer = TabViewer::new(
            &self.memory,
            self.current_function,
            &mut self.signals,
            &self.lang,
        );
        DockArea::new(&mut self.tree)
            .style(Style::from_egui(ctx.style().as_ref()))
            .show(ctx, &mut tab_viewer);

        for signal in &self.signals {
            use SignalKind::*;
            match signal {
                RequestPos(_) => (),
                RenameSymbol(var, name) => {
                    self.memory
                        .symbols
                        .resolve_mut(var)
                        .and_then(|v| Some(v.name = name.clone()))
                        .or_else(|| {
                            self.current_function
                                .and_then(|f| self.memory.ast.get_mut(&f))
                                .and_then(|ast| {
                                    let section = ast.scope.find_owning_section(var).unwrap();
                                    ast.scope.get_symbol_mut(section, var).unwrap().name =
                                        name.clone();
                                    Some(())
                                })
                        });
                }
            }
        }
    }
}
