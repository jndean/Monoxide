#![allow(dead_code)]

use std::fs;
use std::fmt;


#[derive(Debug)]
enum Variable {
    Frac(Box<i32>),
    Array(Box<Vec<Variable>>)
}

impl fmt::Display for Variable{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Frac(valbox) => write!(f, "Frac({})", *valbox),
            Variable::Array(_) => write!(f, "Array(...)")
        }
    }
}

enum Instruction {
    LoadConst{idx: u16},
    LoadLocal{idx: u16},
    SetLocal{val: i32},
    StoreLocal{idx: u16}
}

#[derive(Debug)]
enum StackObject {
    Variable(Variable)
}


struct Scope {
    code: Vec<Instruction>,
    ip: usize,

    stack: Vec<StackObject>,
    locals: Vec<Variable>,
    consts: Vec<Variable>
}


impl Scope {
    fn run(&mut self) -> () {
        while self.ip < self.code.len() {
            match self.code[self.ip] {
                Instruction::LoadConst{idx} => self.load_const(idx),
                Instruction::LoadLocal{idx} => self.load_local(idx),
                Instruction::StoreLocal{idx} => self.store_local(idx),
                _ => println!("BLANK()"),

            }
            self.ip += 1;
        }
    }

        
    fn load_const(&mut self, idx: u16) {
        match &self.consts[idx as usize] {
            Variable::Frac(valbox) => {
                self.stack.push(
                    StackObject::Variable(
                        Variable::Frac(
                            Box::new(**valbox)
                        )
                    )
                );
            }
            Variable::Array(_) => println!("ConstLoading array")
        }
    }  
      
    fn load_local(&mut self, idx: u16) {
        let var = &self.locals[idx as usize];
        match var {
            Variable::Frac(valbox) => println!("LocalLoading {}", *valbox),
            Variable::Array(_) => println!("LocalLoading array")
        }
    }

    fn store_local(&mut self, idx: u16) {
        let obj = self.stack.pop().expect("Popped off empty stack");
        match obj {
            StackObject::Variable(var) => {
                self.locals[idx as usize] = var;
            },
            _ => panic!("Excpected to pop a variable off the stack, found something else!")
        }
    }
}


struct Token {
    type_: String,
    string_: String,
    line: usize,
    col: usize
}

fn tokenise(data: &String) -> Vec<Token> {

    let bytes = data.as_bytes();
    let mut pos = 0;
    while pos < bytes.len() {
        print!("{} ", bytes[pos]);
        pos += 1
    }

    Vec::new()
}


fn main() {
    let code = vec![
        Instruction::LoadConst {idx: 0},
        Instruction::StoreLocal {idx: 1}
    ];

    let locals = vec![
        Variable::Frac(Box::new(0)),
        Variable::Frac(Box::new(1)),
        Variable::Frac(Box::new(2))
    ];

    let consts = vec![
        Variable::Frac(Box::new(99))
    ];

    let mut scope = Scope{
        code,
        ip: 0,
        stack: Vec::new(),
        locals,
        consts
    };
    scope.run();

    println!("Stack[0] = {:?}", scope.locals);
    
    /*let src = fs::read_to_string("src/main.rs")
        .expect("File io error");

    let tokens = tokenise(&src);*/


}
