
use crate::tokeniser::Token;


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

    fn expect_literal(&mut self, value: &str) -> bool {
        match self.tokens.get(self.token_pos).as_ref() {
            Some(tokenref) => {
                if tokenref.string_ == value {
                    self.token_pos += 1;
                    true
                } else {
                    false
                }
            }
            None => false
        }
    }

    fn expect_type(&mut self, value: &str) -> bool {
        match self.tokens.get(self.token_pos).as_ref() {
            Some(tokenref) => {
                if tokenref.type_ == value {
                    self.token_pos += 1;
                    true
                } else {
                    false
                }
            }
            None => false
        }
    }

    pub fn parse(tokens: &Vec<Token>) {
        let parser = Parser{tokens, token_pos: 0};
        /* parse stuff here */
    }

    fn expression(&mut self) {

        //let num = self.expect_type()

    }
}