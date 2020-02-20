
use std::collections::HashMap;
use std::str::FromStr;

use crate::tokeniser::Token;
use crate::ast::{
    StatementNode, ExpressionNode, LookupNode, LetUnletNode,
    FractionNode, BinopNode
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
    Token(Option<Token>),
    VecStatementNode(Option<VecStatementNode>),
    StatementNode(Option<StatementNode>),
    ExpressionNode(Option<ExpressionNode>),
    Instruction(Option<Instruction>),
    LookupNode(Option<LookupNode>),
    String(Option<String>)
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
                        println!("Retrieving memoised {} at pos {}", 
                            stringify!($ret_type), pos);
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

impl Parser {

    fn mark(&self) -> usize {
        self.token_pos
    }

    fn reset(&mut self, pos: usize) {
        self.token_pos = pos;
    }

    fn expect_literal(&mut self, value: &str) -> Option<Token> {
        if let Some(tokenref) =  self.tokens.get(self.token_pos).as_ref() {
            if tokenref.string_ == value {
                self.token_pos += 1;
                return Some((*tokenref).clone());
            }
        }
        None
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

    pub fn parse(tokens: Vec<Token>) {
        let mut parser = Parser{tokens, token_pos: 0, memo: HashMap::new()};
        match parser.statements() { 
            Some(x) => {
                println!("Parsed: {:#?}", x);
            }, 
            None => {
                println!("Failed to parse tokens");
            }
        };
    }

    memoise!(statements_ as statements -> VecStatementNode);
    pub fn statements_(&mut self) -> Option<Vec<StatementNode>> {
        self.repeat(Parser::statement, true)
    }


    memoise!(statement_ as statement -> StatementNode);
    pub fn statement_(&mut self) -> Option<StatementNode> {
        if let Some(stmt) = self.let_unlet() {
            if self.expect_literal(";").is_some() {
                return Some(stmt);}}
        None
    }

    
    memoise!(let_unlet_ as let_unlet -> StatementNode);
    pub fn let_unlet_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        if let Some(name) = self.name() {
        if self.expect_literal(":=").is_some() {
        if let Some(rhs) = self.expression() {
            return Some(StatementNode::LetUnlet(Box::new(
                LetUnletNode{name, rhs, is_unlet: false}
            )));
        }}};
        self.reset(pos);

        if let Some(name) = self.name() {
        if self.expect_literal("=:").is_some() {
        if let Some(rhs) = self.expression() {
            return Some(StatementNode::LetUnlet(Box::new(
                LetUnletNode{name, rhs, is_unlet: true}
            )));
        }}};

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
        if self.expect_literal("+").is_some() { return Some(Instruction::BinopAdd) };
        if self.expect_literal("-").is_some() { return Some(Instruction::BinopSub) };
        if self.expect_literal("*").is_some() { return Some(Instruction::BinopMul) };
        if self.expect_literal("/").is_some() { return Some(Instruction::BinopDiv) };
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

        if self.expect_literal("[").is_some() {
        if let Some(expr) = self.expression() {
        if self.expect_literal("]").is_some() {
            return Some(expr);
        }}};

        self.reset(pos);
        None
    }

    memoise!(name_ as name -> String);
    pub fn name_(&mut self) -> Option<String> {
        let pos = self.mark();

        let dot = self.expect_literal(".");
        if let Some(token) = self.expect_type("NAME") {
            return Some(
                if dot.is_some() { String::from(".") + &token.string_ }
                else             { token.string_ }
            );
        };

        self.reset(pos);
        None
    }

}