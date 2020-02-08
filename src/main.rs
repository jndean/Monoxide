#![allow(dead_code)]

extern crate num_rational;

use std::fs;

mod tokeniser;
mod interpreter;
mod ast;
mod compiler;

type Fraction = num_rational::BigRational;
type BigInt = num_bigint::BigInt;

fn main() {
    let main_code = vec![
        interpreter::Instruction::LoadConst {idx: 0},
        interpreter::Instruction::StoreLocal {idx: 0},
        
        interpreter::Instruction::LoadLocal {idx: 0},
        interpreter::Instruction::LoadConst {idx: 1},
        interpreter::Instruction::BinopDiv,
        interpreter::Instruction::StoreLocal {idx: 1},
    ];

    let main_consts = vec![
        interpreter::Variable::Frac(Fraction::from(BigInt::from(9))),
        interpreter::Variable::Frac(Fraction::from(BigInt::from(5)))
    ];

    let functions = vec![
        interpreter::Function{code: main_code, consts: main_consts, num_locals: 2}
    ];

    let mut interpreter = interpreter::Interpreter::new(&functions, 0);
    interpreter.run();

    println!("{:#?}", interpreter);

    
    /*let src = fs::read_to_string("examples/tmp.mono")
        .expect("File io error");
    let tokens = tokeniser::tokenise(&src);
    println!("{:#?}", tokens);*/

    /*
    let mut compiler = compiler::CompilerCtx::new();

    let eleven = ast::FractionNode{value: Fraction::from(BigInt::from(11))};
    let twelve = ast::FractionNode{value: Fraction::from(BigInt::from(12))};
    let add = ast::BinopNode {
        lhs: ast::ExpressionNode::Fraction(Box::new(eleven)),
        rhs: ast::ExpressionNode::Fraction(Box::new(twelve)),
        op: ast::Binop::Add
    };
    let code = add.compile(&mut compiler);
    println!("ctx: {:#?}", compiler);
    println!("code: {:#?}", code);*/

}
