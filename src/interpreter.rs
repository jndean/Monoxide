
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
    BinopDiv,
    Reverse,
    IJump{delta: isize},  // Instruction Jump //
    SJump{delta: isize},  // Statement Jump //
    IJumpIfBackwards{delta: isize},
    SJumpIfBackwards{delta: isize},
    EndStmt,
    Quit,
    DebugPrint,
}

pub type Statement = Vec<Instruction>;


#[derive(Debug)]
pub enum StackObject<'s> {
    FreeVar(Variable),
    LocalVar(usize),
    ConstVar(&'s Variable),
    //Index(Vec<usize>)
}


#[derive(Debug)]
pub struct Interpreter<'a> {
    functions: &'a Vec<Function>,
    stack: Vec<StackObject<'a>>,

    code: &'a Vec<Statement>,
    stmt_pos: usize,
    inst_pos: usize,
    forwards: bool,
    locals: Vec<Option<Variable>>,
    consts: &'a Vec<Variable>,

    scope_stack: Vec<Scope<'a>>
}


#[derive(Debug)]
pub struct Scope<'a> {
    code: &'a Vec<Statement>,
    stmt_pos: usize,
    forwards: bool,
    locals: Vec<Option<Variable>>,
    consts: &'a Vec<Variable>
}


#[derive(Debug)]
pub struct Function {
    pub code: Vec<Statement>,
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
            stmt_pos: 0,
            inst_pos: 0,
            forwards: true,
            locals: vec![None; main.num_locals],
            consts: &main.consts,
            scope_stack: Vec::new()
        }
    }

    pub fn run(&mut self) -> () {
        'statement_loop: loop {
            let instructions = &self.code[self.stmt_pos];

            'instruction_loop: loop {
                match &instructions[self.inst_pos] {
                    Instruction::LoadConst{idx} => self.load_const(*idx),
                    Instruction::LoadLocal{idx} => self.load_local(*idx),
                    Instruction::StoreLocal{idx} => self.store_local(*idx),
                    Instruction::BinopAdd => self.binop_add(),
                    Instruction::BinopSub => self.binop_sub(),
                    Instruction::BinopMul => self.binop_mul(),
                    Instruction::BinopDiv => self.binop_div(),
                    Instruction::Reverse => self.reverse(),
                    Instruction::DebugPrint => self.debug_print(),
                    Instruction::SJump{delta} => self.s_jump(*delta),
                    Instruction::SJumpIfBackwards{delta} => if !self.forwards { self.s_jump(*delta) },
                    Instruction::IJump{delta} => self.i_jump(*delta),
                    Instruction::IJumpIfBackwards{delta} => if !self.forwards { self.i_jump(*delta) },
                    
                    Instruction::EndStmt => {
                        if self.forwards { self.s_jump(1) } else { self.s_jump(-1) };
                        break 'instruction_loop;
                    },
                    Instruction::Quit => {
                        break 'statement_loop;
                    }
                }

                self.inst_pos += 1;
            }
        }
    }

    pub fn initialise_function(&mut self, idx: usize, forwards: bool) {
        let func = &self.functions[idx];
        self.scope_stack.push(
            Scope{
                code  : replace(&mut self.code  , &func.code  ),
                consts: replace(&mut self.consts, &func.consts),
                locals: replace(&mut self.locals, vec![None; func.num_locals]),
                stmt_pos: self.stmt_pos,
                forwards: self.forwards,
            }
        );
        self.forwards = forwards;
        self.stmt_pos = if forwards {0} else {func.code.len() - 1};
    }

    fn i_jump(&mut self, delta: isize) {
        self.inst_pos = ((self.inst_pos as isize) + delta) as usize;
    }

    fn s_jump(&mut self, delta: isize) {
        self.inst_pos = 0;
        self.stmt_pos = ((self.stmt_pos as isize) + delta) as usize;        
    }

    fn reverse(&mut self) {
        self.forwards = !self.forwards;
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
    
    fn binop_sub(&mut self) {
        let rhs = self.pop();
        let lhs = self.pop();
        let lhs: &Variable = self.stackvar_asref(&lhs);
        let rhs: &Variable = self.stackvar_asref(&rhs);

        let result = match (lhs, rhs) {
            (Variable::Frac(left), Variable::Frac(right)) => Variable::Frac(left - right),
            (Variable::Array(_), Variable::Array(_)) => unimplemented!(),
            _ => panic!("Adding incompatible types")
        };

        self.stack.push(StackObject::FreeVar(result));
    }

    fn binop_mul(&mut self) {
        let rhs = self.pop();
        let lhs = self.pop();
        let lhs: &Variable = self.stackvar_asref(&lhs);
        let rhs: &Variable = self.stackvar_asref(&rhs);

        let result = match (lhs, rhs) {
            (Variable::Frac(left), Variable::Frac(right)) => Variable::Frac(left * right),
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
            _ => panic!("Dividing incompatible types")
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

    fn debug_print(&self) {
        println!("Locals: {:#?}", self.locals);
    }
}

