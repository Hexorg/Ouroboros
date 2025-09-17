use std::{collections::HashMap, ops::Index};

use iced_x86::Register;

use crate::{ir::{address, basic_block::NextBlock, high_function::CallingConvention, SymbolMap, VariableType}, memory::Memory};

use super::{BasicBlock, Expression, VariableDefinition, Scope, VariableSymbol, HighFunction, ExpressionOp, Address, SingleEntrySingleExit};

type OpIdx = usize;



pub struct AbstractSyntaxTree{
    pub scope: Scope,
    entry:AstStatement,
}

pub enum AstStatement{
    Block(Vec<AstStatement>),
    Nop,
    Function{
        name:VariableSymbol,
        args: Vec<VariableSymbol>,
        body:Box<AstStatement>
    },
    Assignment{
        sese:SingleEntrySingleExit<Address>,
        destination:Expression, 
        value: Expression
    },
    Call{
        sese:SingleEntrySingleExit<Address>,
        destination:Expression, 
        params:Vec<Expression>,
        call_from:Address,
    },
    If{
        sese:SingleEntrySingleExit<Address>, 
        condition:Expression, 
        true_statement:Box<AstStatement>, 
        true_branch:Address,
        else_statement:Box<AstStatement>,
        else_branch:Address,
    },
    Loop{
        sese:SingleEntrySingleExit<Address>,
        condition:Expression,
        body:Box<AstStatement>,
        body_address:Address,
    },
    Return{
        sese:SingleEntrySingleExit<Address>,
        result:Expression,
    },
    Comment(String),
    MultilineComment(String)
}

impl AstStatement {
    pub fn is_nop(&self) -> bool {
        matches!(self, AstStatement::Nop)
    }
}

impl std::fmt::Debug for AstStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstStatement::Function{..} => f.write_str("AstNode::FunctionHeader"),
            AstStatement::Call{..} => f.write_str("AstNode::Call"),
            AstStatement::Assignment{..} => f.write_str("AstNode::Assignment"),
            AstStatement::If{..} => f.write_str("AstNode::If"),
            AstStatement::Comment(..) => f.write_str("AstNode::Comment"),
            AstStatement::MultilineComment(..) => f.write_str("AstNode::MultilineComment"),
            AstStatement::Loop{..} => f.write_str("AstNode::Loop"),
            AstStatement::Return{..} => f.write_str("AstNode::Return"),
            AstStatement::Block(_) => f.write_str("AstNode::Block"),
            AstStatement::Nop => f.write_str("AstNode::Nop"),
        }
    }
}


impl std::fmt::Debug for AbstractSyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AbstractSyntaxTree").field("entry", &self.entry).finish()
    }
}

impl AbstractSyntaxTree {
    pub fn new(hf:&HighFunction, mem:&Memory) -> Self {
        let mut scope = Scope::new();
        scope.fill_parents(&hf.pts, hf.pts.root);

        for call_result in &hf.used_call_results {
            if let VariableSymbol::CallResult { call_from, call_to } = call_result {
                if let Some(section) = hf.pts.get_section(*call_from) {
                    let key = VariableSymbol::CallResult { call_from:*call_from, call_to:call_to.clone() };
                    
                    let symbol_name = if let Some(function) = mem.symbols.resolve_exp(call_to) {
                        format!("{}_result", function.name)
                    } else {
                        format!("{key}")
                    };
                    let value = VariableDefinition{
                        kind:VariableType { name: "void *".to_string() },
                        name: symbol_name,
                        variable: key.clone()
                    };
                    scope.add(section, key, value);
                } else {
                    println!("Can't map call from {call_from} to an SESE");
                }
            }
        }

        let body = AstStatement::Block(build_block(&mut scope, hf.cfg.start, hf, hf.pts.root));
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
                    if let ExpressionOp::Variable(VariableSymbol::Register(Register::ESP)) = addr.get(0) {
                        if let ExpressionOp::Value(_) = addr.get(1) {
                            if let ExpressionOp::Add(_,_) = addr.get(2) {
                                args.push(VariableSymbol::Ram(addr.clone()));
                            }
                        }
                    }
                }
            }
        }
        statements.push(AstStatement::Function{ name: VariableSymbol::Ram(Expression::from(hf.cfg.start)), args, body:Box::new(body)});
        Self{
            scope,
            entry: AstStatement::Block(statements),   
        }
    }

    pub fn entry(&self) -> &AstStatement {
        &self.entry
    }

}

fn build_block(scope:&mut Scope, start:Address, hf:&HighFunction, sese:SingleEntrySingleExit<Address>) -> Vec<AstStatement> {
    let mut ast = Vec::new();
    let mut branch_block = add_assignments(&mut ast, hf.composed_blocks.get_at_point(start).unwrap(), hf, sese);

    if branch_block.address == sese.1 {
        return ast
    }
    
    if let Some(pts_children) = hf.pts.get_children(sese) {
        // print
        while let Some(c_pts) = pts_children.iter().find(|p| p.0 == branch_block.address) {
            // child block fails out to the same address as parent block - no need to draw else branch.
            add_program_segment(scope, &mut ast, hf, *c_pts, c_pts.1 == sese.1 && c_pts.1 != Address::NULL);
            if c_pts.1 != Address::NULL {
                branch_block = add_assignments(&mut ast, hf.composed_blocks.get_at_point(c_pts.1).unwrap(), hf, sese);
            }
            if c_pts.1 == sese.1 { 
                break;
            }
        }
    }
    ast
}

fn define_all_variables(scope:&mut Scope, sese:SingleEntrySingleExit<Address>, expression:&Expression, pos:usize) {
    match &expression[pos] {
        ExpressionOp::Dereference(d) => {
                    let variable = VariableSymbol::Ram(expression.get_sub_expression(*d));
                    if scope.get_symbol_recursive(sese, &variable).is_none() {
                        scope.add(sese, variable.clone(), VariableDefinition { 
                            kind: VariableType{name:String::from("void *")}, 
                            name: format!("DAT_{variable}"), 
                            variable});
                    }
                },
        ExpressionOp::Variable(variable_symbol) => {
            if scope.get_symbol_recursive(sese, variable_symbol).is_none() {
                scope.add(sese, variable_symbol.clone(), VariableDefinition { 
                            kind: VariableType{name:String::from("void *")}, 
                            name: format!("DAT_{variable_symbol}"), 
                            variable:variable_symbol.clone()});
            }
        }
        ExpressionOp::Value(_) |
        ExpressionOp::DestinationRegister(_) => (),
        
        ExpressionOp::Multiequals(l, r) |
        ExpressionOp::Add(l, r) |
        ExpressionOp::Sub(l, r) |
        ExpressionOp::Multiply(l, r) |
        ExpressionOp::LessOrEquals(l, r) |
        ExpressionOp::Less(l, r) |
        ExpressionOp::GreaterOrEquals(l, r) |
        ExpressionOp::Greater(l, r) |
        ExpressionOp::Equals(l, r) |
        ExpressionOp::NotEquals(l, r) |
        ExpressionOp::BitShiftRight(l, r) |
        ExpressionOp::BitShiftLeft(l, r) |
        ExpressionOp::And(l, r) |
        ExpressionOp::Assign(l, r) => {
            define_all_variables(scope, sese, expression, *l);
            define_all_variables(scope, sese, expression, *r);
        }
    }

}

fn add_program_segment(scope:&mut Scope, ast_block:&mut Vec<AstStatement>, hf:&HighFunction, sese:SingleEntrySingleExit<Address>, is_force_drop_else_branch:bool) {
    let branch_block = hf.composed_blocks.get_at_point(sese.0).unwrap();
    if let NextBlock::ConditionalJump { condition, true_branch, false_branch } = &branch_block.next {
        let true_branch_distance_to_return = *hf.cfg.distance_to_return.get(true_branch).unwrap();
        let false_branch_distance_to_return = *hf.cfg.distance_to_return.get(false_branch).unwrap();

        // (true_branch_distance_to_return == 0 && *true_branch != pts.1) ||
        // (false_branch_distance_to_return == 0  && *false_branch != pts.1) ||
        let (first_branch, else_branch, is_loop, condition) = if *false_branch == sese.1  {
            if *true_branch != sese.0 {
                // if (expr) { do work or return };
                (*true_branch, None, false, condition.clone())
            } else {
                // while (expr) { do work };
                (*true_branch, None, true, condition.clone())
            }
        } else if *true_branch == sese.1 {
            let mut condition = condition.clone();
            condition.not();
            if *false_branch != sese.0 {
                // if (!expr) {do work or return };
                (*false_branch, None, false, condition)
            } else {
                // while (expr) { do work };
                (*false_branch, None, true, condition)
            }
        } else {
            // full if statement
            if false_branch_distance_to_return == 0 { // prefer printing return blocks in the if segment.
                let mut condition = condition.clone();
                condition.not();
                (*false_branch, if is_force_drop_else_branch { None } else { Some(*true_branch) }, false, condition.clone())
            } else {
                (*true_branch, if is_force_drop_else_branch { None } else { Some(*false_branch) }, false, condition.clone())
            }
        };
        
        //define_all_variables(scope, sese, &condition, condition.get_entry_point());
        
        let block = build_block(scope, first_branch, hf, sese);
        if let Some(else_branch) = else_branch {
            let false_block = build_block(scope, else_branch, hf, sese);
            if matches!(false_block.last(), Some(AstStatement::Return{..})) {
                // if it's a return block, we don't need to draw else 
                ast_block.push(AstStatement::If{
                    sese,
                    condition:condition, 
                    true_statement:Box::new(AstStatement::Block(block)), 
                    true_branch:first_branch,
                    else_statement:Box::new(AstStatement::Nop),
                    else_branch,
                });
                ast_block.extend(false_block);
            } else {
                ast_block.push(AstStatement::If{
                    sese,
                    condition:condition, 
                    true_statement:Box::new(AstStatement::Block(block)), 
                    true_branch: first_branch,
                    else_statement:Box::new(AstStatement::Block(false_block)),
                    else_branch
                });
            }
        } else if is_loop {
            ast_block.push(AstStatement::Loop{
                sese,
                condition:condition, 
                body:Box::new(AstStatement::Block(block)),
                body_address:first_branch,
            });
        } else {
            ast_block.push(AstStatement::If{
                sese,
                condition:condition, 
                true_statement:Box::new(AstStatement::Block(block)), 
                true_branch: first_branch,
                else_statement:Box::new(AstStatement::Nop),
                else_branch: Address::NULL
            });
        }
        



    } else {
        panic!("Unexpected start of a program segment.")
    }
    // ast_block.push(super::AstStatement::Block(ast.get_entry_point(), count, hf.pts.root));
}









fn add_return(stmts:&mut Vec<AstStatement>, block:&BasicBlock, hf:&HighFunction, sese:SingleEntrySingleExit<Address>) {
    match hf.calling_convention {
        CallingConvention::Cdecl => {
            stmts.push(AstStatement::Return { sese, result: block.get_register_state_or_none(&iced_x86::Register::EAX).unwrap().clone() });
        }
    }
}


fn add_assignments<'a>(stmts:&mut Vec<AstStatement>, block:&'a BasicBlock, hf:&'a HighFunction, sese:SingleEntrySingleExit<Address>) -> &'a BasicBlock {
    if block.address != sese.1 {
        for (ip, value) in &block.instruction_map {
            if let Some(ExpressionOp::Assign(l, r)) = value.last_op() {
                match value[*l] {
                    ExpressionOp::DestinationRegister(_) => (),
                    ExpressionOp::Dereference(d) => {
                        let sub = value.get_sub_expression(d);
                        if sub.iter_vars().find(|p| *p == &VariableSymbol::Register(iced_x86::Register::ESP)).is_none() {
                            let destination = value.get_sub_expression(*l);
                            let value = value.get_sub_expression(*r);
                            stmts.push(AstStatement::Assignment{sese, destination, value});
                            // stmts.push(AstStatement::Assignment{sese, variable:VariableSymbol::Ram(sub), value:state});
                        }
                    }
                    _ => {
                        let destination = value.get_sub_expression(*l);
                        let value = value.get_sub_expression(*r);
                        stmts.push(AstStatement::Assignment{sese, destination, value});
                    }
                }
            }
        }
        match &block.next {
            NextBlock::Call { origin, destination, return_instruction } => {
                add_call(stmts, block, hf, destination, *origin, sese);
                add_assignments(stmts, hf.composed_blocks.get_at_point(*return_instruction).unwrap(), hf, sese)
            },
            NextBlock::Return{..} => {
                add_return(stmts, block, hf, sese);
                block
            }
            NextBlock::Unconditional(addr) => add_assignments(stmts, hf.composed_blocks.get_at_point(*addr).unwrap(), hf, sese),
            _ => block,
        }
    } else {
        block
    }
}

fn add_call(stmts:&mut Vec<AstStatement>, block:&BasicBlock, hf:&HighFunction, destination:&Expression, call_from:Address, sese:SingleEntrySingleExit<Address>) {
    let mut params = Vec::new();
    if let Some(stack) = block.get_register_state_or_none(&iced_x86::Register::ESP) {
        let mut param_1 = stack.clone();
        loop {
            param_1.add_value(4);
            
            if let Some(state) = block.get_memory_state_or_none(&param_1) {
                if let Some(ExpressionOp::Variable(VariableSymbol::Register(_))) = state.last_op() { break; } else {
                    params.push(state.clone())
                }
            } else {
                break;
            }
        }
    }
    stmts.push(AstStatement::Call { destination: destination.clone(), params, call_from, sese});
}