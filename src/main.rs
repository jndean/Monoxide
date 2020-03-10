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

type Fraction = num_rational::BigRational;

fn main() {
    
    let src = fs::read_to_string("examples/tmp.mx").expect("File io error");
    let tokens = tokeniser::tokenise(&src);
    println!("{:#?}", tokens);
    let parsed = parser::parse(tokens).expect("Failed to parse");
    let checked = syntaxchecker::check_syntax(parsed);
    // println!("Parsed: {:#?}", module);
    // let program = module.compile();
    // println!("Compiled: {:#?}", program);
    // interpreter::Interpreter::run(&program);
    
}