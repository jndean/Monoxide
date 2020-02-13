#![allow(dead_code)]

extern crate num_rational;

use std::fs;
use std::str::FromStr;

mod tokeniser;
mod interpreter;
mod ast;
mod compiler;
mod parser;
use interpreter::{Instruction, Statement};

type Fraction = num_rational::BigRational;
type BigInt = num_bigint::BigInt;

fn main() {

    /*
    let main_code: Vec<Statement> = vec![
        vec![
            Instruction::SJumpIfBackwards{delta: 6}
        ],
        vec![
            Instruction::IJumpIfBackwards {delta: 2},
            Instruction::LoadConst {idx: 0},
            Instruction::StoreLocal {idx: 0}
        ],
        vec![Instruction::DebugPrint],
        vec![
            Instruction::LoadLocal {idx: 0},
            Instruction::LoadConst {idx: 1},
            Instruction::IJumpIfBackwards{delta: 2},
            Instruction::BinopAdd,
            Instruction::IJump{delta: 1},
            Instruction::BinopSub,
            interpreter::Instruction::StoreLocal {idx: 0}
        ],
        vec![Instruction::DebugPrint],
        vec![Instruction::Reverse]
    ];

    let loop_code = vec![
        vec![
            Instruction::IJumpIfBackwards {delta: 2},
            Instruction::LoadConst {idx: 0},
            Instruction::StoreLocal {idx: 0}
        ],
    ];

    let array_code = vec![
        vec![
            Instruction::IJumpIfBackwards {delta: 3},
            Instruction::LoadConst {idx: 2},
            Instruction::StoreLocal {idx: 0},
            Instruction::IJump {delta: 1},
            Instruction::FreeLocal {idx: 0}
        ],
        vec![
            Instruction::IJumpIfBackwards {delta: 3},
            Instruction::LoadLocal {idx: 0},
            Instruction::StoreLocal {idx: 1},
            Instruction::IJump {delta: 1},
            Instruction::FreeLocal {idx: 1}
        ],
        vec![
            Instruction::IJumpIfBackwards {delta: 6},
            Instruction::LoadLocal {idx: 0},
            Instruction::LoadConst {idx: 3},
            Instruction::LoadConst {idx: 3},
            Instruction::CreateArrayRef {size: 2},
            Instruction::ArrayReadNoPop,
            Instruction::LoadConst {idx: 1},
            Instruction::BinopAdd,
            Instruction::ArrayWrite
        ],
        vec![Instruction::DebugPrint],
        //vec![Instruction::Reverse]
    ];

    let main_consts = vec![
        interpreter::Variable::Frac(Fraction::from(BigInt::from(9))),
        interpreter::Variable::Frac(Fraction::from(BigInt::from(5))),
        interpreter::Variable::Array(vec![
            interpreter::Variable::Frac(Fraction::from(BigInt::from(0))),
            interpreter::Variable::Array(vec![
                interpreter::Variable::Frac(Fraction::from(BigInt::from(17))),
                interpreter::Variable::Frac(Fraction::from(BigInt::from(18)))
            ])
        ]),
        interpreter::Variable::Frac(Fraction::from(BigInt::from(1)))
    ];

    let functions = vec![
        interpreter::Function{code: array_code, consts: main_consts, num_locals: 3}
    ];

    let mut interpreter = interpreter::Interpreter::new(&functions, 0);
    interpreter.run();
    */
    
    let src = fs::read_to_string("examples/tmp.mono")
        .expect("File io error");
    let tokens = tokeniser::tokenise(&src);
    println!("{:#?}", tokens);
    parser::Parser::parse(&tokens);

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
