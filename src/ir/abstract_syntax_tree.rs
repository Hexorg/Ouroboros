use sleigh_compile::ldef::SleighLanguage;

use crate::{
    ir::{
        basic_block::{BlockSlot, DestinationKind, NextBlock},
        expression::InstructionSize,
        high_function::CallingConvention,
        type_system::VariableType,
    },
    memory::Memory,
};

use super::{
    control_flow_graph::SingleEntrySingleExit,
    high_function::HighFunction,
    scope::{Scope, VariableDefinition},
    Address, BasicBlock, Expression, ExpressionOp, VariableSymbol,
};

type OpIdx = usize;

pub struct AbstractSyntaxTree {
    pub scope: Scope,
    entry: AstStatement,
}

pub enum AstStatement {
    Block(Vec<AstStatement>),
    Nop,
    Function {
        name: VariableSymbol,
        args: Vec<VariableSymbol>,
        body: Box<AstStatement>,
    },
    Assignment {
        sese: SingleEntrySingleExit<BlockSlot>,
        destination: Expression,
        value: Expression,
    },
    Call {
        sese: SingleEntrySingleExit<BlockSlot>,
        destination: DestinationKind,
        params: Vec<Expression>,
        call_from: Address,
    },
    If {
        sese: SingleEntrySingleExit<BlockSlot>,
        condition: Expression,
        true_statement: Box<AstStatement>,
        true_branch: BlockSlot,
        else_statement: Box<AstStatement>,
        else_branch: BlockSlot,
    },
    Loop {
        sese: SingleEntrySingleExit<BlockSlot>,
        condition: Expression,
        body: Box<AstStatement>,
        body_address: BlockSlot,
    },
    Return {
        sese: SingleEntrySingleExit<BlockSlot>,
        result: Expression,
    },
    Comment(String),
    MultilineComment(String),
}

impl AstStatement {
    pub fn is_nop(&self) -> bool {
        matches!(self, AstStatement::Nop)
    }
}

impl std::fmt::Debug for AstStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstStatement::Function { .. } => f.write_str("AstNode::FunctionHeader"),
            AstStatement::Call { .. } => f.write_str("AstNode::Call"),
            AstStatement::Assignment { .. } => f.write_str("AstNode::Assignment"),
            AstStatement::If { .. } => f.write_str("AstNode::If"),
            AstStatement::Comment(..) => f.write_str("AstNode::Comment"),
            AstStatement::MultilineComment(..) => f.write_str("AstNode::MultilineComment"),
            AstStatement::Loop { .. } => f.write_str("AstNode::Loop"),
            AstStatement::Return { .. } => f.write_str("AstNode::Return"),
            AstStatement::Block(_) => f.write_str("AstNode::Block"),
            AstStatement::Nop => f.write_str("AstNode::Nop"),
        }
    }
}

impl std::fmt::Debug for AbstractSyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AbstractSyntaxTree")
            .field("entry", &self.entry)
            .finish()
    }
}

impl AbstractSyntaxTree {
    pub fn new(hf: &HighFunction, mem: &Memory, lang: &SleighLanguage) -> Self {
        let mut scope = Scope::new();
        scope.fill_parents(&hf.pts, hf.pts.root);

        for call_result in &hf.used_call_results {
            if let VariableSymbol::CallResult { call_from, call_to } = call_result {
                if let Some(section) = hf
                    .pts
                    .get_section(hf.composed_blocks.slot_by_address(*call_from).unwrap())
                {
                    let key = VariableSymbol::CallResult {
                        call_from: *call_from,
                        call_to: call_to.clone(),
                    };

                    let symbol_name =
                        if let Some(function) = mem.symbols.resolve_destination(call_to) {
                            format!("{}_result", function.name)
                        } else {
                            format!("{key}")
                        };
                    let value = VariableDefinition {
                        kind: VariableType::default(),
                        name: symbol_name,
                        variable: key.clone(),
                    };
                    scope.add(section, key, value);
                } else {
                    println!("Can't map call from {call_from} to an SESE");
                }
            }
        }

        let body =
            AstStatement::Block(build_block(&mut scope, hf.cfg.start, hf, lang, hf.pts.root));
        let mut statements = Vec::new();
        // statements.push(AstStatement::Comment(format!("Scope:")));
        // statements.push(AstStatement::MultilineComment(scope.pretty_print(&hf.pts)));
        // statements.push(AstStatement::Comment(format!("*** Memory reads ***")));
        // for read in &hf.memory_read {
        //     statements.push(AstStatement::Comment(format!("{read}")));
        // }
        // statements.push(AstStatement::Comment(String::new()));
        // statements.push(AstStatement::Comment(format!("*** Memory writes ***")));
        // for write in &hf.memory_written {
        //     statements.push(AstStatement::Comment(format!("{write}")));
        // }
        let mut args = Vec::new();
        match hf.calling_convention {
            CallingConvention::Cdecl => {
                for addr in &hf.memory_read {
                    if let ExpressionOp::Variable(VariableSymbol::Varnode(r)) = addr.get(0) {
                        if r == &lang.sp {
                            if let ExpressionOp::Value(_) = addr.get(1) {
                                if let ExpressionOp::Add(_, _, _) = addr.get(2) {
                                    args.push(VariableSymbol::Ram(Box::new(addr.clone()), 4));
                                }
                            }
                        }
                    }
                }
            }
        }
        statements.push(AstStatement::Function {
            name: VariableSymbol::Ram(Box::new(Expression::from(hf.start)), 4),
            args,
            body: Box::new(body),
        });
        Self {
            scope,
            entry: AstStatement::Block(statements),
        }
    }

    pub fn entry(&self) -> &AstStatement {
        &self.entry
    }
}

fn build_block(
    scope: &mut Scope,
    start: BlockSlot,
    hf: &HighFunction,
    lang: &SleighLanguage,
    sese: SingleEntrySingleExit<BlockSlot>,
) -> Vec<AstStatement> {
    let mut ast = Vec::new();
    let mut branch_block_slot = add_assignments(&mut ast, start, hf, lang, sese);

    if branch_block_slot == sese.1 {
        return ast;
    }

    if let Some(pts_children) = hf.pts.get_children(sese) {
        // print
        while let Some(c_pts) = pts_children.iter().find(|p| p.0 == branch_block_slot) {
            // child block fails out to the same address as parent block - no need to draw else branch.
            add_program_segment(
                scope,
                &mut ast,
                hf,
                lang,
                *c_pts,
                c_pts.1 == sese.1 && c_pts.1 != hf.cfg.single_end(),
            );
            if c_pts.1 != hf.cfg.single_end() {
                branch_block_slot = add_assignments(&mut ast, c_pts.1, hf, lang, sese);
            }
            if c_pts.1 == sese.1 {
                break;
            }
        }
    }
    ast
}

fn define_all_variables(
    scope: &mut Scope,
    sese: SingleEntrySingleExit<BlockSlot>,
    expression: &Expression,
    pos: usize,
) {
    match &expression[pos] {
        ExpressionOp::Dereference(d) => {
            let variable = VariableSymbol::Ram(Box::new(expression.get_sub_expression(*d)), 4);
            if scope.get_symbol_recursive(sese, &variable).is_none() {
                scope.add(
                    sese,
                    variable.clone(),
                    VariableDefinition {
                        kind: VariableType::default(),
                        name: format!("DAT_{variable}"),
                        variable,
                    },
                );
            }
        }
        ExpressionOp::Variable(variable_symbol) => {
            if scope.get_symbol_recursive(sese, variable_symbol).is_none() {
                scope.add(
                    sese,
                    variable_symbol.clone(),
                    VariableDefinition {
                        kind: VariableType::default(),
                        name: format!("DAT_{variable_symbol}"),
                        variable: variable_symbol.clone(),
                    },
                );
            }
        }
        ExpressionOp::Value(_)
        | ExpressionOp::DestinationRegister(_)
        | ExpressionOp::Assign(_, _) => (),
        ExpressionOp::Overflow(l, _) | ExpressionOp::CountOnes(l) | ExpressionOp::Not(l) => {
            define_all_variables(scope, sese, expression, *l);
        }
        ExpressionOp::Multiequals(l, r)
        | ExpressionOp::Add(l, r, _)
        | ExpressionOp::Sub(l, r, _)
        | ExpressionOp::Multiply(l, r, _)
        | ExpressionOp::LessOrEquals(l, r, _)
        | ExpressionOp::Less(l, r, _)
        | ExpressionOp::GreaterOrEquals(l, r, _)
        | ExpressionOp::Greater(l, r, _)
        | ExpressionOp::Equals(l, r, _)
        | ExpressionOp::NotEquals(l, r, _)
        | ExpressionOp::BitShiftRight(l, r, _)
        | ExpressionOp::BitShiftLeft(l, r, _)
        | ExpressionOp::And(l, r)
        | ExpressionOp::Xor(l, r)
        | ExpressionOp::Or(l, r) => {
            define_all_variables(scope, sese, expression, *l);
            define_all_variables(scope, sese, expression, *r);
        }
    }
}

fn add_program_segment(
    scope: &mut Scope,
    ast_block: &mut Vec<AstStatement>,
    hf: &HighFunction,
    lang: &SleighLanguage,
    sese: SingleEntrySingleExit<BlockSlot>,
    is_force_drop_else_branch: bool,
) {
    let branch_block = &hf.composed_blocks[sese.0];
    if let NextBlock::Jump {
        condition,
        true_branch,
        false_branch,
    } = &branch_block.next
    {
        let true_branch_slot = hf.composed_blocks.slot_by_destination(true_branch).unwrap();
        let false_branch_slot = hf
            .composed_blocks
            .slot_by_destination(false_branch)
            .unwrap();

        let true_branch_distance_to_return =
            *hf.cfg.distance_to_return.get(&true_branch_slot).unwrap();
        let false_branch_distance_to_return =
            *hf.cfg.distance_to_return.get(&false_branch_slot).unwrap();

        // (true_branch_distance_to_return == 0 && *true_branch != pts.1) ||
        // (false_branch_distance_to_return == 0  && *false_branch != pts.1) ||
        let (first_branch, else_branch, is_loop, condition) = if false_branch_slot == sese.1 {
            if true_branch_slot != sese.0 {
                // if (expr) { do work or return };
                (true_branch_slot, None, false, condition.clone())
            } else {
                // while (expr) { do work };
                (true_branch_slot, None, true, condition.clone())
            }
        } else if true_branch_slot == sese.1 {
            let mut condition = condition.clone();
            condition.not();
            if false_branch_slot != sese.0 {
                // if (!expr) {do work or return };
                (false_branch_slot, None, false, condition)
            } else {
                // while (expr) { do work };
                (false_branch_slot, None, true, condition)
            }
        } else {
            // full if statement
            if false_branch_distance_to_return == 0 {
                // prefer printing return blocks in the if segment.
                let mut condition = condition.clone();
                condition.not();
                (
                    false_branch_slot,
                    if is_force_drop_else_branch {
                        None
                    } else {
                        Some(true_branch_slot)
                    },
                    false,
                    condition.clone(),
                )
            } else {
                (
                    false_branch_slot,
                    if is_force_drop_else_branch {
                        None
                    } else {
                        Some(false_branch_slot)
                    },
                    false,
                    condition.clone(),
                )
            }
        };

        //define_all_variables(scope, sese, &condition, condition.get_entry_point());

        let block = build_block(scope, first_branch, hf, lang, sese);
        if let Some(else_branch) = else_branch {
            let false_block = build_block(scope, else_branch, hf, lang, sese);
            if matches!(false_block.last(), Some(AstStatement::Return { .. })) {
                // if it's a return block, we don't need to draw else
                ast_block.push(AstStatement::If {
                    sese,
                    condition: condition,
                    true_statement: Box::new(AstStatement::Block(block)),
                    true_branch: first_branch,
                    else_statement: Box::new(AstStatement::Nop),
                    else_branch,
                });
                ast_block.extend(false_block);
            } else {
                ast_block.push(AstStatement::If {
                    sese,
                    condition: condition,
                    true_statement: Box::new(AstStatement::Block(block)),
                    true_branch: first_branch,
                    else_statement: Box::new(AstStatement::Block(false_block)),
                    else_branch,
                });
            }
        } else if is_loop {
            ast_block.push(AstStatement::Loop {
                sese,
                condition: condition,
                body: Box::new(AstStatement::Block(block)),
                body_address: first_branch,
            });
        } else {
            ast_block.push(AstStatement::If {
                sese,
                condition: condition,
                true_statement: Box::new(AstStatement::Block(block)),
                true_branch: first_branch,
                else_statement: Box::new(AstStatement::Nop),
                else_branch: BlockSlot::default(),
            });
        }
    } else {
        panic!("Unexpected start of a program segment.")
    }
    // ast_block.push(super::AstStatement::Block(ast.get_entry_point(), count, hf.pts.root));
}

fn add_return(
    stmts: &mut Vec<AstStatement>,
    block: &BasicBlock,
    hf: &HighFunction,
    lang: &SleighLanguage,
    sese: SingleEntrySingleExit<BlockSlot>,
) {
    match hf.calling_convention {
        CallingConvention::Cdecl => {
            stmts.push(AstStatement::Return {
                sese,
                result: block
                    .registers
                    .get(lang.sleigh.get_reg("EAX").unwrap().get_raw_var())
                    .unwrap()
                    .clone(),
            });
        }
    }
}

fn add_assignments<'a>(
    stmts: &mut Vec<AstStatement>,
    block_slot: BlockSlot,
    hf: &'a HighFunction,
    lang: &SleighLanguage,
    sese: SingleEntrySingleExit<BlockSlot>,
) -> BlockSlot {
    if block_slot != sese.1 {
        let block = &hf.composed_blocks[block_slot];
        for addr in &block.memory_writes {
            if addr
                .iter()
                .filter(|p| *p == &ExpressionOp::Variable(VariableSymbol::Varnode(lang.sp)))
                .count()
                == 0
            {
                let mut destination = addr.clone();
                destination.dereference();
                let state = block.memory.get(addr).unwrap();
                stmts.push(AstStatement::Assignment {
                    sese,
                    destination,
                    value: state.clone(),
                });
            }
        }
        match &block.next {
            NextBlock::Call {
                origin,
                destination,
                default_return,
            } => {
                add_call(stmts, block, hf, lang, destination, *origin, sese);
                if let Some(next_block) = hf.composed_blocks.slot_by_address(*default_return) {
                    add_assignments(stmts, next_block, hf, lang, sese)
                } else {
                    block_slot
                }
            }
            NextBlock::Return { .. } => {
                add_return(stmts, block, hf, lang, sese);
                block_slot
            }
            NextBlock::Follow(dst) => add_assignments(
                stmts,
                hf.composed_blocks.slot_by_destination(dst).unwrap(),
                hf,
                lang,
                sese,
            ),
            NextBlock::Jump { .. } => block_slot,
        }
    } else {
        block_slot
    }
}

fn add_call(
    stmts: &mut Vec<AstStatement>,
    block: &BasicBlock,
    hf: &HighFunction,
    lang: &SleighLanguage,
    destination: &DestinationKind,
    call_from: Address,
    sese: SingleEntrySingleExit<BlockSlot>,
) {
    let mut params = Vec::new();
    if let Some(stack) = block.registers.get(lang.sp) {
        let mut param_1 = stack.clone();
        loop {
            param_1.add_value(4, InstructionSize::U32);

            if let Some(state) = block.get_memory_state_or_none(&param_1) {
                if let Some(ExpressionOp::Variable(VariableSymbol::Varnode(_))) = state.root_op() {
                    break;
                } else {
                    params.push(state.clone())
                }
            } else {
                break;
            }
        }
    }
    stmts.push(AstStatement::Call {
        destination: destination.clone(),
        params,
        call_from,
        sese,
    });
}
