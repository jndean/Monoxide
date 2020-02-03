
use std::fmt;

extern crate num_rational;
extern crate num_bigint;

pub type Fraction = num_rational::BigRational;
type BigInt = num_bigint::BigInt;


#[derive(PartialEq)]
pub enum Variable {
    Frac(Box<Fraction>),
    Array(Box<Vec<Variable>>)
}

impl Variable {
    pub fn new_frac(val: Fraction) -> Variable {
        Variable::Frac(Box::new(val))
    }

}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Frac(valbox) => write!(f, "{}", *valbox),
            Variable::Array(_) => write!(f, "Array(...)")
        }
    }
}

/*
impl PartialEq for Variable {
    fn eq(&self, other: Variable&)
}*/

#[derive(Debug)]
pub enum Instruction {
    LoadConst{idx: u16},
    LoadLocal{idx: u16},
    StoreLocal{idx: u16},
    BinopAdd,
    BinopSub,
    BinopMul,
    BinopDiv
}

#[derive(Debug)]
pub enum StackObject {
    Variable(Variable)
}


pub struct Scope {
    pub code: Vec<Instruction>,
    pub ip: usize,

    pub stack: Vec<StackObject>,
    pub locals: Vec<Variable>,
    pub consts: Vec<Variable>
}


impl Scope {
    pub fn run(&mut self) -> () {
        while self.ip < self.code.len() {
            match self.code[self.ip] {
                Instruction::LoadConst{idx} => self.load_const(idx),
                Instruction::LoadLocal{idx} => self.load_local(idx),
                Instruction::StoreLocal{idx} => self.store_local(idx),
                Instruction::BinopAdd => self.binop_add(),
                Instruction::BinopDiv => self.binop_div(),
                _ => println!("BLANK()"),
            }
            self.ip += 1;
        }
    }

        
    fn load_const(&mut self, idx: u16) {
        self.stack.push(StackObject::Variable(
            match &self.consts[idx as usize] {
                Variable::Frac(valbox) => Variable::new_frac((**valbox).clone()),
                _ => panic!("Unimplemented")
                //Variable::Array(_) => Variable::Frac(Box::new(Fraction::new(0, 1))),
            }
        ));
    }  

    fn load_local(&mut self, idx: u16) {
        let var = &self.locals[idx as usize];
        match var {
            Variable::Frac(valbox) => println!("LocalLoading {}", *valbox),
            Variable::Array(_) => println!("LocalLoading array")
        }
    }

    fn store_local(&mut self, idx: u16) {
        self.locals[idx as usize] = self.pop_variable();
    }

    fn binop_add(&mut self) {
        let args = (self.pop_variable(), self.pop_variable());
        self.stack.push(StackObject::Variable(
            match args {
                (Variable::Frac(right), Variable::Frac(left)) => Variable::new_frac(*left + *right),
                (Variable::Array(_), Variable::Array(_)) => panic!("Unimplemented"),
                _ => panic!("Adding incompatible types")
            }
        ));
    }

    fn binop_div(&mut self) {
        let args = (self.pop_variable(), self.pop_variable());
        self.stack.push(StackObject::Variable(
            match args {
                (Variable::Frac(right), Variable::Frac(left)) => Variable::new_frac(*left / *right),
                (Variable::Array(_), Variable::Array(_)) => panic!("Unimplemented"),
                _ => panic!("Adding incompatible types")
            }
        ));
    }

    fn pop(&mut self) -> StackObject {
        self.stack.pop().expect("Popped off empty stack")
    }

    fn pop_variable(&mut self) -> Variable {
        match self.pop() {
            StackObject::Variable(var) => var,
            _ => panic!("Tried to pop a variable off the stack, found something else")
        }
    }
}

