
use std::fmt;
use std::mem::replace;
use std::convert::TryInto;

extern crate num_rational;
extern crate num_bigint;

pub type Fraction = num_rational::BigRational;
type BigInt = num_bigint::BigInt;


#[derive(PartialEq, Clone)]
pub enum Variable {
    Frac(Fraction),
    Array(Vec<Variable>)
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
    ModopAdd{idx: usize},
    BinopAdd,
    BinopSub,
    BinopMul,
    BinopDiv,
    Reverse
}

#[derive(Debug)]
pub enum StackObject<'s> {
    FreeVar(Variable),
    LocalVar(usize),
    ConstVar(&'s Variable),
    //Index(Vec<usize>)
}


#[derive(Debug)]
pub struct Interpreter<'a> {
    pub functions: &'a Vec<Function>,
    pub stack: Vec<StackObject<'a>>,

    pub code: &'a Vec<Instruction>,
    pub ip: isize,
    pub backwards: bool,
    pub locals: Vec<Option<Variable>>,
    pub consts: &'a Vec<Variable>,

    scope_stack: Vec<Scope<'a>>
}


#[derive(Debug)]
pub struct Scope<'a> {
    code: &'a Vec<Instruction>,
    ip: isize,
    backwards: bool,
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
            backwards: false,
            locals: vec![None; main.num_locals],
            consts: &main.consts,
            scope_stack: Vec::new()
        }
    }

    pub fn run(&mut self) -> () {
        let code_len = self.code.len().try_into().unwrap();
        while (0 <= self.ip) & (self.ip < code_len) {
            match self.code[self.ip as usize] {
                Instruction::LoadConst{idx} => self.load_const(idx),
                Instruction::LoadLocal{idx} => self.load_local(idx),
                Instruction::StoreLocal{idx} => self.store_local(idx),
                Instruction::ModopAdd{idx} => self.modop(idx, Instruction::BinopAdd),
                Instruction::BinopAdd => self.binop_add(),
                Instruction::BinopDiv => self.binop_div(),
                Instruction::Reverse => self.reverse(),
                _ => println!("BLANK()"),
            }

            self.ip += if self.backwards {-1} else {1};
        }
    }

    pub fn initialise_function(&mut self, idx: usize, backwards: bool) {
        let func = &self.functions[idx];
        self.scope_stack.push(
            Scope{
                code  : replace(&mut self.code  , &func.code  ),
                consts: replace(&mut self.consts, &func.consts),
                locals: replace(&mut self.locals, vec![None; func.num_locals]),
                ip: self.ip,
                backwards: self.backwards,
            }
        );
        self.backwards = backwards;
        self.ip = if backwards {
            let code_len: isize = func.code.len().try_into().unwrap();
            code_len - 1
        } else {0}
    }
    
    fn modop(&mut self, idx: usize, op: Instruction) {
        if self.backwards {
            
        } else {
            self.load_local(idx);
            self.binop_add();
            self.store_local(idx);
        }
    }

    fn reverse(&mut self) {
        self.backwards = !self.backwards;
    }

    fn load_const(&mut self, idx: usize) {
        self.stack.push(StackObject::ConstVar(&self.consts[idx]));
    }  

    fn load_local(&mut self, idx: usize) {
        self.stack.push(StackObject::LocalVar(idx));
    }

    fn store_local(&mut self, idx: usize) {
        self.locals[idx] = Some(match self.pop() {
            StackObject::ConstVar(var) => var.clone(),
            StackObject::LocalVar(idx) => self.locals[idx].clone().unwrap(),
            StackObject::FreeVar(var) => var
        });
    }

    fn binop_add(&mut self) {
        let rhs = self.pop();
        let lhs = self.pop();
        let lhs: &Variable = self.stackvar_asref(&lhs);
        let rhs: &Variable = self.stackvar_asref(&rhs);

        let result = match (lhs, rhs) {
            (Variable::Frac(left), Variable::Frac(right)) => Variable::Frac(left + right),
            (Variable::Array(_), Variable::Array(_)) => unimplemented!(),
            _ => panic!("Adding incompatible types")
        };

        self.stack.push(StackObject::FreeVar(result));
    }

    fn binop_div(&mut self) {
        let rhs = self.pop();
        let lhs = self.pop();
        let lhs: &Variable = self.stackvar_asref(&lhs);
        let rhs: &Variable = self.stackvar_asref(&rhs);

        let result = match (lhs, rhs) {
            (Variable::Frac(left), Variable::Frac(right)) => Variable::Frac(left / right),
            (Variable::Array(_), Variable::Array(_)) => unimplemented!(),
            _ => panic!("Adding incompatible types")
        };

        self.stack.push(StackObject::FreeVar(result));
    }

    fn pop(&mut self) -> StackObject<'a> {
        self.stack.pop().expect("Popped off empty stack")
    }

    fn stackvar_asref(&'a self, var: &'a StackObject) -> &'a Variable {
        match &var {
            StackObject::ConstVar(var_ref) => *var_ref,
            StackObject::LocalVar(idx) => self.locals[*idx].as_ref().unwrap(),
            StackObject::FreeVar(var) => &var,
        }
    }
}

