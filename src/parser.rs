
use std::str::FromStr;

use crate::tokeniser::Token;
use crate::ast;
use crate::interpreter::{Fraction, Instruction};


pub struct Parser {
    tokens: Vec<Token>,
    token_pos: usize,
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

    fn expect<F, R>(&mut self, method: F) -> Option<R>
        where F: Fn(&mut Parser) -> Option<R> 
    {
        method(self)
    }

    fn repeat<F, R>(&mut self, method: F, allow_empty: bool) -> Option<Vec<R>>
        where F: Copy + Fn(&mut Parser) -> Option<R>
    {
        let pos = self.mark();
        let mut results = Vec::new();
        loop {
            match self.expect(method) {
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
        let mut parser = Parser{tokens, token_pos: 0};
        match parser.lookup() { 
            Some(x) => {
                println!("Parsed: {:#?}", x);
            }, 
            None => {
                println!("Failed to parse tokens");
            }
        };
    }

    pub fn expression(&mut self) -> Option<ast::ExpressionNode> {
        let pos = self.mark();

        if let Some(token) = self.expect_type("NUMBER") {
            let value = Fraction::from_str(&token.string_[..]).unwrap();
            let value = ast::FractionNode{value};
            return Some(ast::ExpressionNode::Fraction(Box::new(value)));
        };


        if let Some(lhs) = self.expect(Parser::expression) {
        if let Some(op)  = self.expect(Parser::binop) {
        if let Some(rhs) = self.expect(Parser::expression) {
            return Some(
                ast::ExpressionNode::Binop(Box::new(
                    ast::BinopNode{lhs, rhs, op}
            )));
        }}};

        self.reset(pos);
        None
    }

    pub fn binop(&mut self) -> Option<Instruction> {
        if self.expect_literal("+").is_some() { return Some(Instruction::BinopAdd) };
        if self.expect_literal("-").is_some() { return Some(Instruction::BinopSub) };
        if self.expect_literal("*").is_some() { return Some(Instruction::BinopMul) };
        if self.expect_literal("/").is_some() { return Some(Instruction::BinopDiv) };
        None
    }

    pub fn lookup(&mut self) -> Option<ast::LookupNode> {
        let pos = self.mark();

        if let Some(name)    = self.expect(Parser::name) {
        if let Some(indices) = self.repeat(Parser::index, true) {
            return Some(ast::LookupNode{name, indices});
        }};

        self.reset(pos);
        None
    }

    pub fn index(&mut self) -> Option<ast::ExpressionNode> {
        let pos = self.mark();

        if self.expect_literal("[").is_some() {
        if let Some(expr) = self.expect(Parser::expression) {
        if self.expect_literal("]").is_some() {
            return Some(expr);
        }}};

        self.reset(pos);
        None
    }

    pub fn name(&mut self) -> Option<String> {
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