#![allow(dead_code)]

extern crate num_rational;

use std::fs;

mod tokeniser;
mod interpreter;
mod AST;
mod compiler;

type Fraction = num_rational::BigRational;
type BigInt = num_bigint::BigInt;

fn main() {
    let code = vec![
        interpreter::Instruction::LoadConst {idx: 0},
        interpreter::Instruction::StoreLocal {idx: 1},
        
        interpreter::Instruction::LoadConst {idx: 0},
        interpreter::Instruction::LoadConst {idx: 1},
        interpreter::Instruction::BinopDiv,
        interpreter::Instruction::StoreLocal {idx: 0},
    ];

    let locals = vec![
        interpreter::Variable::Frac(Box::new(Fraction::from(BigInt::from(0)))),
        interpreter::Variable::Frac(Box::new(Fraction::from(BigInt::from(1)))),
        interpreter::Variable::Frac(Box::new(Fraction::from(BigInt::from(2))))
    ];

    let consts = vec![
        interpreter::Variable::Frac(Box::new(Fraction::from(BigInt::from(9)))),
        interpreter::Variable::Frac(Box::new(Fraction::from(BigInt::from(5))))
    ];

    let mut scope = interpreter::Scope{
        code,
        ip: 0,
        stack: Vec::new(),
        locals,
        consts
    };

    /*println!("Before run:");
    println!("Stack = {:?}", scope.stack);
    println!("Locals = {:?}", scope.locals);

    scope.run();

    println!("After run:");
    println!("Stack = {:?}", scope.stack);
    println!("Locals = {:?}", scope.locals);*/
    
    /*let src = fs::read_to_string("src/main.rs")
        .expect("File io error");
    let tokens = tokeniser::tokenise(&src);*/


    let mut compiler = compiler::CompilerCtx::new();

    let eleven = AST::FractionNode{value: Fraction::from(BigInt::from(11))};
    let twelve = AST::FractionNode{value: Fraction::from(BigInt::from(12))};
    let add = AST::BinopNode {
        lhs: AST::ExpressionNode::Fraction(Box::new(eleven)),
        rhs: AST::ExpressionNode::Fraction(Box::new(twelve)),
        op: AST::Binop::Add
    };
    let code = add.compile(&mut compiler);
    println!("ctx: {:#?}", compiler);
    println!("code: {:#?}", code);

}
