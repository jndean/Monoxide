
use std::str::FromStr;

use crate::tokeniser::Token;
use crate::ast;
use crate::interpreter::Fraction;


pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    token_pos: usize,
}

impl<'a> Parser<'a> {

    fn mark(&self) -> usize {
        self.token_pos
    }

    fn reset(&mut self, pos: usize) {
        self.token_pos = pos;
    }

    fn expect_literal(&mut self, value: &str) -> Option<&Token> {
        match self.tokens.get(self.token_pos).as_ref() {
            Some(tokenref) => {
                if tokenref.string_ == value {
                    self.token_pos += 1;
                    Some(tokenref)
                } else {
                    None
                }
            }
            None => None
        }
    }

    fn expect_type(&mut self, type_: &str) -> Option<&Token> {
        match self.tokens.get(self.token_pos).as_ref() {
            Some(tokenref) => {
                if tokenref.type_ == type_ {
                    self.token_pos += 1;
                    Some(tokenref)
                } else {
                    None
                }
            }
            None => None
        }
    }

    

    pub fn parse(tokens: &Vec<Token>) {
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

        let frac = self.expect_type("NUMBER");
        match frac { Some(token) => {
            let value = Fraction::from_str(&token.string_[..]).unwrap();
            let value = ast::FractionNode{value};
            return Some(ast::ExpressionNode::Fraction(Box::new(value)));
        }, None => ()};


        match self.expression()        { Some(lhs) => {
        match self.expect_literal("+") { Some(op) => {
        match self.expression()        { Some(rhs) => {
            let value = ast::BinopNode{lhs, rhs, op: ast::Binop::Add};
            return Some(ast::ExpressionNode::Binop(Box::new(value)));
        }, None => ()}}, None => ()}}, None => ()};

        self.reset(pos);

        None
    }
}