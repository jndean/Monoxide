
use std::collections::HashMap;
use std::str::FromStr;

use crate::tokeniser::Token;
use crate::ast::{
    StatementNode, ExpressionNode, LookupNode, LetUnletNode,
    FractionNode, BinopNode, IfNode, ModopNode, FunctionNode,Module
};
use crate::interpreter::{Fraction, Instruction};


pub struct Parser {
    tokens: Vec<Token>,
    token_pos: usize,
    memo: HashMap<(usize, String), (usize, Parsed)>
}

type VecStatementNode = Vec<StatementNode>;
#[derive(Clone)]
pub enum Parsed {
    String(Option<String>),
    Token(Option<Token>),
    VecStatementNode(Option<VecStatementNode>),
    StatementNode(Option<StatementNode>),
    ExpressionNode(Option<ExpressionNode>),
    Instruction(Option<Instruction>),
    LookupNode(Option<LookupNode>),
    IfNode(Option<IfNode>),
    FunctionNode(Option<FunctionNode>)
}


#[allow(unused_macros)]
macro_rules! memoise {
    ($raw_func:ident as $out_func:ident -> $ret_type:ident) => {

        fn $out_func(&mut self) -> Option<$ret_type> {
            let pos = self.mark();
            let key = (pos, String::from(stringify!($raw_func)));
            match self.memo.get(&key) {
                Some((end, result)) => {
                    let end = *end;
                    let result = (*result).clone();
                    if let Parsed::$ret_type(ret) = result {
                        self.reset(end);
                        return ret;
                    } else {unreachable!()}
                },
                None => {
                    let result = self.$raw_func();
                    self.memo.insert(
                        key, 
                        (self.mark(), Parsed::$ret_type(result.clone()))
                    );
                    result
                }
            }
        }

    }
}


pub fn parse(tokens: Vec<Token>) -> Option<Module>{
    let mut parser = Parser{tokens, token_pos: 0, memo: HashMap::new()};
    if let Some(func) = parser.function() {
        Some(Module{functions: vec![func]})
    } else {
        None
    }
}


impl Parser {

    fn mark(&self) -> usize {
        self.token_pos
    }

    fn reset(&mut self, pos: usize) {
        self.token_pos = pos;
    }

    fn expect_literal(&mut self, value: &str) -> bool {
        if let Some(tokenref) =  self.tokens.get(self.token_pos).as_ref() {
            if tokenref.string_ == value {
                self.token_pos += 1;
                return true;
            };
        };
        false
    }

    fn expect_type(&mut self, type_: &str) -> Option<Token> {
        if let Some(tokenref) =  self.tokens.get(self.token_pos).as_ref() {
            if tokenref.type_ == type_ {
                self.token_pos += 1;
                return Some((*tokenref).clone());
            }
        }
        None
    }

    fn repeat<F, R>(&mut self, method: F, allow_empty: bool) -> Option<Vec<R>>
        where F: Copy + Fn(&mut Parser) -> Option<R>
    {
        let pos = self.mark();
        let mut results = Vec::new();
        loop {
            match method(self) {
                Some(result) => results.push(result),
                None => break
            };
        };
        if results.is_empty() && !allow_empty {
            self.reset(pos);
            None
        } else {
            Some(results)
        }
    }

    memoise!(function_ as function -> FunctionNode);
    pub fn function_(&mut self) -> Option<FunctionNode> {
        let pos = self.mark();

        if self.expect_literal("fn") {
        if let Some(name) = self.name() {
        if self.expect_literal("(") {
        let borrow_params = self.name_list();
        if self.expect_literal(")") {
        if self.expect_literal("(") {
        let steal_params = self.name_list();
        if self.expect_literal(")") {
        if self.expect_literal("{") {
        if let Some(stmts) = self.statements() {
        if self.expect_literal("}") {
        if self.expect_literal("~") {
        if self.name() == Some(name.clone()) {
        if self.expect_literal("(") {
        let return_params = self.name_list();
        if self.expect_literal(")") {
            return Some(FunctionNode{
                name, borrow_params, steal_params, return_params, stmts
            });
        }}}}}}}}}}}}};

        self.reset(pos);
        None
    }

    pub fn name_list(&mut self) -> Vec<String> {
        let pos = self.mark();
        let mut result = Vec::new();
        if let Some(item) = self.name() {
            result.push(item);
            result.extend(self.repeat(Parser::comma_name, true).unwrap());
            return result;
        }
        self.reset(pos);
        result
    }

    memoise!(comma_name_ as comma_name -> String);
    pub fn comma_name_(&mut self) -> Option<String> {
        let pos = self.mark();
        if self.expect_literal(",") {
            if let Some(name) = self.name() {
                return Some(name);
            }
        }
        self.reset(pos);
        None
    }

    memoise!(statements_ as statements -> VecStatementNode);
    pub fn statements_(&mut self) -> Option<Vec<StatementNode>> {
        self.repeat(Parser::statement, true)
    }


    memoise!(statement_ as statement -> StatementNode);
    pub fn statement_(&mut self) -> Option<StatementNode> {
        if let Some(stmt) = self.letunlet_stmt() {return Some(stmt);}
        if let Some(stmt) = self.modop_stmt() {return Some(stmt);}
        if let Some(stmt) = self.if_stmt() {return Some(stmt);}
        None
    }

    
    memoise!(if_stmt_ as if_stmt -> StatementNode);
    pub fn if_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        if self.expect_literal("if") {
        if self.expect_literal("(") {
        if let Some(fwd_expr) = self.expression() {
        if self.expect_literal(")") {
        if self.expect_literal("{") {
        if let Some(if_stmts) = self.statements() {
        if self.expect_literal("}") {
        let else_stmts = self.else_block();
        if self.expect_literal("~") {
        if self.expect_literal("if") {
        if self.expect_literal("(") {
        let bkwd_expr = self.expression();
        if self.expect_literal(")") {
        if self.expect_literal(";") {
            let else_stmts = match else_stmts {
                Some(stmts) => stmts,
                None => Vec::new()
            };
            let bkwd_expr = match bkwd_expr {
                Some(expr) => expr,
                None => fwd_expr.clone()
            };
            return Some(StatementNode::If(Box::new(
                IfNode{fwd_expr, if_stmts, else_stmts, bkwd_expr}
            )));
        }}}}}}}}}}}};

        self.reset(pos);
        None
    }

    memoise!(else_block_ as else_block -> VecStatementNode);
    pub fn else_block_(&mut self) -> Option<Vec<StatementNode>> {
        let pos = self.mark();

        if self.expect_literal("else") {
        if self.expect_literal("{") {
        if let Some(stmts) = self.statements() {
        if self.expect_literal("}") {
            return Some(stmts);
        }}}};

        self.reset(pos);
        None
    }


    memoise!(modop_stmt_ as modop_stmt -> StatementNode);
    pub fn modop_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        if let Some(lookup) = self.lookup() {
        if let Some(op) = self.modop() {
        if let Some(rhs) = self.expression() {
        if self.expect_literal(";") {
            return Some(StatementNode::Modop(Box::new(
                ModopNode{lookup, op, rhs}
            )));
        }}}};

        self.reset(pos);
        None
    }


    memoise!(letunlet_stmt_ as letunlet_stmt -> StatementNode);
    pub fn letunlet_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        if let Some(name) = self.name() {
        if self.expect_literal(":=") {
        if let Some(rhs) = self.expression() {
        if self.expect_literal(";") {
            return Some(StatementNode::LetUnlet(Box::new(
                LetUnletNode{name, rhs, is_unlet: false}
            )));
        }}}};
        self.reset(pos);

        if let Some(name) = self.name() {
        if self.expect_literal("=:") {
        if let Some(rhs) = self.expression() {
        if self.expect_literal(";") {
            return Some(StatementNode::LetUnlet(Box::new(
                LetUnletNode{name, rhs, is_unlet: true}
            )));
        }}}};

        self.reset(pos);
        None
    }


    memoise!(expression_ as expression -> ExpressionNode);
    pub fn expression_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if let Some(token) = self.expect_type("NUMBER") {
            let value = Fraction::from_str(&token.string_[..]).unwrap();
            let value = FractionNode{value};
            return Some(ExpressionNode::Fraction(Box::new(value)));
        };


        if let Some(lhs) = self.expression() {
        if let Some(op)  = self.binop() {
        if let Some(rhs) = self.expression() {
            return Some(
                ExpressionNode::Binop(Box::new(
                    BinopNode{lhs, rhs, op}
            )));
        }}};

        self.reset(pos);
        None
    }


    memoise!(binop_ as binop -> Instruction);
    pub fn binop_(&mut self) -> Option<Instruction> {
        if self.expect_literal("+") { return Some(Instruction::BinopAdd) };
        if self.expect_literal("-") { return Some(Instruction::BinopSub) };
        if self.expect_literal("*") { return Some(Instruction::BinopMul) };
        if self.expect_literal("/") { return Some(Instruction::BinopDiv) };
        None
    }


    memoise!(modop_ as modop -> Instruction);
    pub fn modop_(&mut self) -> Option<Instruction> {
        if self.expect_literal("+=") { return Some(Instruction::BinopAdd) };
        if self.expect_literal("-=") { return Some(Instruction::BinopSub) };
        if self.expect_literal("*=") { return Some(Instruction::BinopMul) };
        if self.expect_literal("/=") { return Some(Instruction::BinopDiv) };
        None
    }


    memoise!(lookup_ as lookup -> LookupNode);
    pub fn lookup_(&mut self) -> Option<LookupNode> {
        let pos = self.mark();

        if let Some(name)    = self.name() {
        if let Some(indices) = self.repeat(Parser::index, true) {
            return Some(LookupNode{name, indices});
        }};

        self.reset(pos);
        None
    }

    memoise!(index_ as index -> ExpressionNode);
    pub fn index_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if self.expect_literal("[") {
        if let Some(expr) = self.expression() {
        if self.expect_literal("]") {
            return Some(expr);
        }}};

        self.reset(pos);
        None
    }

    memoise!(name_ as name -> String);
    pub fn name_(&mut self) -> Option<String> {
        let pos = self.mark();

        let has_dot = self.expect_literal(".");
        if let Some(token) = self.expect_type("NAME") {
            return Some(
                if has_dot { String::from(".") + &token.string_ }
                else       { token.string_ }
            );
        };

        self.reset(pos);
        None
    }

}