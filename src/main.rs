#![allow(dead_code)]

extern crate num_rational;

use std::fs;

mod tokeniser;
mod interpreter;
mod parsetree;
mod syntaxtree;
mod syntaxchecker;
mod compiler;
mod parser;

use syntaxchecker::{check_syntax, SyntaxError};


type Fraction = num_rational::BigRational;

fn main() {
    
    let src = fs::read_to_string("examples/tmp.mx").expect("File io error");
    let tokens = tokeniser::tokenise(&src);
    // println!("Tokens: {:#?}", tokens);
    let parsed = parser::parse(tokens).expect("Failed to parse");


    let module = match check_syntax(parsed) {
        Ok(module) => module,
        Err(SyntaxError{line, col, desc}) => {
            eprintln!("SyntaxError at line {}, column {}:\n ->  {}\n", line, col, desc);
            return;
        }
    };

    // println!("Module: {:#?}", module);
    let program = module.compile();
    // println!("Compiled: {:#?}", program);
    interpreter::Interpreter::run(&program);
    
}