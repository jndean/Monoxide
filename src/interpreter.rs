
use std::fmt;
use std::mem::replace;

extern crate num_rational;
extern crate num_bigint;

pub type Fraction = num_rational::BigRational;
type BigInt = num_bigint::BigInt;


#[derive(PartialEq, Clone)]
pub enum Variable {
    Frac(Fraction),
    Array(Vec<Variable>)
}

impl Variable {
    pub fn new_frac(val: Fraction) -> Variable {
        Variable::Frac(val)
    }
}


impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Frac(val) => write!(f, "{}", val),
            Variable::Array(_) => write!(f, "Array(...)")
        }
    }
}



#[derive(Debug)]
pub enum Instruction {
    LoadConst{idx: usize},
    LoadLocal{idx: usize},
    StoreLocal{idx: usize},
    BinopAdd,
    BinopSub,
    BinopMul,
    BinopDiv
}

#[derive(Debug)]
pub enum StackObject {
    Variable(Variable)
}

#[derive(Debug)]
pub struct Interpreter<'a> {
    pub functions: &'a Vec<Function>,
    pub stack: Vec<StackObject>,

    pub code: &'a Vec<Instruction>,
    pub ip: usize,
    pub locals: Vec<Option<Variable>>,
    pub consts: &'a Vec<Variable>,

    scope_stack: Vec<Scope<'a>>
}


#[derive(Debug)]
pub struct Scope<'a> {
    code: &'a Vec<Instruction>,
    ip: usize,
    locals: Vec<Option<Variable>>,
    consts: &'a Vec<Variable>
}


#[derive(Debug)]
pub struct Function {
    pub code: Vec<Instruction>,
    pub consts: Vec<Variable>,
    pub num_locals: usize
}


impl<'a> Interpreter<'a> {

    pub fn new(functions: &'a Vec<Function>, main_idx: usize) -> Interpreter<'a> {
        let main = &functions[main_idx];
        Interpreter {
            functions,
            stack: Vec::new(),
            code: &main.code,
            ip: 0,
            locals: vec![None; main.num_locals],
            consts: &main.consts,
            scope_stack: Vec::new()
        }
    }

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

    pub fn initialise_function(&mut self, idx: usize) {
        let func = &self.functions[idx];
        self.scope_stack.push(
            Scope{
                code  : replace(&mut self.code  , &func.code  ),
                consts: replace(&mut self.consts, &func.consts),
                ip    : replace(&mut self.ip    , 0           ),
                locals: replace(&mut self.locals, vec![None; func.num_locals])
            }
        );
    }
        
    fn load_const(&mut self, idx: usize) {
        self.stack.push(StackObject::Variable(
            match &self.consts[idx] {
                Variable::Frac(val) => Variable::Frac((*val).clone()),
                _ => panic!("Unimplemented")
                //Variable::Array(_) => Variable::Frac(Box::new(Fraction::new(0, 1))),
            }
        ));
    }  

    fn load_local(&mut self, idx: usize) {
        match &self.locals[idx] {
            Some(Variable::Frac(val)) => println!("LocalLoading {}", val),
            Some(Variable::Array(_)) => println!("LocalLoading array"),
            None => panic!()
        }
    }

    fn store_local(&mut self, idx: usize) {
        self.locals[idx] = Some(self.pop_variable());
    }

    fn binop_add(&mut self) {
        let args = (self.pop_variable(), self.pop_variable());
        self.stack.push(StackObject::Variable(
            match args {
                (Variable::Frac(right), Variable::Frac(left)) => Variable::new_frac(left + right),
                (Variable::Array(_), Variable::Array(_)) => panic!("Unimplemented"),
                _ => panic!("Adding incompatible types")
            }
        ));
    }

    fn binop_div(&mut self) {
        let args = (self.pop_variable(), self.pop_variable());
        self.stack.push(StackObject::Variable(
            match args {
                (Variable::Frac(right), Variable::Frac(left)) => Variable::new_frac(left / right),
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

