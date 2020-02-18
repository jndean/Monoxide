
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

    pub fn parse(tokens: Vec<Token>) {
        let mut parser = Parser{tokens, token_pos: 0};
        match parser.expression() { 
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

    fn binop(&mut self) -> Option<Instruction> {
        if self.expect_literal("+").is_some() { return Some(Instruction::BinopAdd) };
        if self.expect_literal("-").is_some() { return Some(Instruction::BinopSub) };
        if self.expect_literal("*").is_some() { return Some(Instruction::BinopMul) };
        if self.expect_literal("/").is_some() { return Some(Instruction::BinopDiv) };
        None
    }

    fn name(&mut self) -> Option<String> {
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

    fn expect<F, R>(&mut self, method: F) -> Option<R>
        where F: Fn(&mut Parser) -> Option<R> 
    {
        method(self)
    }
}