
extern crate num_rational;
extern crate num_bigint;

use std::fmt;
use std::mem::replace;
use num_traits::cast::ToPrimitive;
use num_bigint::BigInt;


pub type Fraction = num_rational::BigRational;


#[derive(PartialEq, Clone)]
pub enum Variable {
    Frac(Fraction),
    Array(Vec<Variable>),
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Frac(val) => write!(f, "{}", val),
            Variable::Array(vec) => write!(f, "Array({:?})", vec)
        }
    }
}


impl Variable {
    fn to_bool(&self) -> bool {
        match self {
            Variable::Frac(value) => *value.numer() != BigInt::from(0),
            Variable::Array(items) => items.len() > 0
        }
    }
}


#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst{idx: usize},
    LoadRegister{register: usize},
    StoreRegister{register: usize},
    FreeRegister{register: usize},
    Load,
    LoadNoPop,
    Store,
    Pull,
    BinopAdd,
    BinopSub,
    BinopMul,
    BinopDiv,
    Reverse{idx: usize},
    Jump{delta: isize},  // Instruction Jump //
    JumpIfTrue{delta: isize},    
    JumpIfFalse{delta: isize},
    CreateIndex{size: usize},
    Quit,
    DebugPrint,
}

#[derive(Debug)]
pub enum StackObject {
    FreeVar(Variable),
    RegisterVar(usize),
    IndexVar(Vec<usize>),
}

#[derive(Debug)]
pub struct Code {
    pub fwd: Vec<Instruction>,
    pub bkwd: Vec<Instruction>
}


#[derive(Debug)]
pub struct Interpreter<'a> {
    functions: &'a Vec<Function>,
    stack: Vec<StackObject>,

    code: &'a Code,
    ip: usize,
    forwards: bool,
    registers: Vec<Option<Variable>>,
    consts: &'a Vec<Variable>,

    scope_stack: Vec<Scope<'a>>
}


#[derive(Debug)]
pub struct Scope<'a> {
    code: &'a Code,
    ip: usize,
    forwards: bool,
    registers: Vec<Option<Variable>>,
    consts: &'a Vec<Variable>
}


#[derive(Debug)]
pub struct Function {
    pub code: Code,
    pub consts: Vec<Variable>,
    pub num_registers: usize
}

#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>
}


macro_rules! binop_method {
    ($name:ident, $op:tt) => {
        fn $name (&mut self) {
            let rhs = self.pop();
            let lhs = self.pop();
            let lhs: &Variable = self.stackobj_as_varref(&lhs);
            let rhs: &Variable = self.stackobj_as_varref(&rhs);
            let result = match (lhs, rhs) {
                (Variable::Frac(left), Variable::Frac(right)) => Variable::Frac(left $op right),
                (Variable::Array(_), Variable::Array(_)) => unimplemented!(),
                _ => panic!("Adding incompatible types")
            };
            self.stack.push(StackObject::FreeVar(result));
        }
    };
}

impl<'a> Interpreter<'a> {

    pub fn new(functions: &'a Vec<Function>, main_idx: usize) -> Interpreter<'a> {
        let main = &functions[main_idx];
        Interpreter {
            functions,
            stack: Vec::new(),
            code: &main.code,
            ip: 0,
            forwards: true,
            registers: vec![None; main.num_registers],
            consts: &main.consts,
            scope_stack: Vec::new()
        }
    }

    pub fn run(&mut self) -> () {

        let mut instructions = &self.code.fwd;

        'instruction_loop: loop {

            let instruction = match instructions.get(self.ip) {
                Some(inst) => inst,
                None => break
            };

            println!("IP: {}, {:?}", self.ip, instruction);

            match instruction {
                Instruction::LoadConst{idx} => self.load_const(*idx),
                Instruction::LoadRegister{register} => self.load_register(*register),
                Instruction::StoreRegister{register} => self.store_register(*register),
                Instruction::FreeRegister{register} => self.free_register(*register),
                Instruction::Load => self.load(),
                Instruction::LoadNoPop => self.load_nopop(),
                Instruction::Store => self.store(),
                Instruction::BinopAdd => self.binop_add(),
                Instruction::BinopSub => self.binop_sub(),
                Instruction::BinopMul => self.binop_mul(),
                Instruction::BinopDiv => self.binop_div(),
                Instruction::Jump{delta} => self.jump(*delta),
                Instruction::CreateIndex{size} => self.create_index(*size),
                Instruction::JumpIfTrue{delta} => self.jump_if_true(*delta),
                Instruction::JumpIfFalse{delta} => self.jump_if_false(*delta),
                Instruction::Pull => self.pull(),
                
                Instruction::DebugPrint => self.debug_print(),
                Instruction::Quit => break 'instruction_loop,
                Instruction::Reverse{idx} => {
                    self.forwards = !self.forwards;
                    self.ip = *idx;
                    instructions = if self.forwards {&self.code.fwd} else {&self.code.bkwd};
                    continue 'instruction_loop;
                }
            }
            
            self.ip += 1;
        }
    }

    pub fn initialise_function(&mut self, idx: usize, forwards: bool) {
        let func = &self.functions[idx];
        self.scope_stack.push(
            Scope{
                code  : replace(&mut self.code  , &func.code  ),
                consts: replace(&mut self.consts, &func.consts),
                registers: replace(&mut self.registers, vec![None; func.num_registers]),
                ip: self.ip,
                forwards: self.forwards,
            }
        );
        self.forwards = forwards;
        self.ip = if forwards {0} else {func.code.bkwd.len() - 1};
    }

    #[inline]
    fn jump(&mut self, delta: isize) {
        self.ip = ((self.ip as isize) + delta) as usize;
    }

    fn jump_if_true(&mut self, delta: isize) {
        let condition = self.pop();
        let var = self.stackobj_as_varref(&condition);
        if var.to_bool() {
            self.jump(delta);
        }
    }

    fn jump_if_false(&mut self, delta: isize) {
        let stackobj = self.pop();
        let condition_var = self.stackobj_as_varref(&stackobj);
        if !condition_var.to_bool() {
            println!("Jumping because false {:?}", stackobj);
            self.jump(delta);
        }
    }

    fn load_const(&mut self, idx: usize) {
        self.stack.push(StackObject::FreeVar(self.consts[idx].clone()));
    }  

    fn load_register(&mut self, idx: usize) {
        self.stack.push(StackObject::RegisterVar(idx));
    }

    fn store_register(&mut self, idx: usize) {
        let stackvar = self.pop();
        let var = self.stackobj_as_var(stackvar);
        self.registers[idx] = Some(var);
    }

    fn free_register(&mut self, idx: usize) {
        self.registers[idx] = None;
    }

    fn create_index(&mut self, size: usize) {
        let mut index: Vec<usize> = Vec::with_capacity(size);
        for _ in 0..size {
            let stackvar = self.pop();
            if let Variable::Frac(value) = self.stackobj_as_varref(&stackvar) {
                index.push(value.to_integer().to_usize().unwrap());
            } else {
                panic!("Trying to use non-number as array index");
            };
        };
        if let StackObject::RegisterVar(register) = self.pop() {
            index.push(register);
        } else { unreachable!() };

        index.reverse();
        self.stack.push(StackObject::IndexVar(index));
    }

    fn load(&mut self) {
        let stackvar = self.pop();
        let var = self.stackobj_as_var(stackvar);
        self.stack.push(StackObject::FreeVar(var));
    }

    fn load_nopop(&mut self) {
        let stack_index = &self.stack[self.stack.len()-1];
        let var = self.stackobj_as_varref(&stack_index).clone();
        self.stack.push(StackObject::FreeVar(var));
    }

    fn store(&mut self) {
        let stackvar = self.pop();
        let value = self.stackobj_as_var(stackvar);
        let mut destination = self.pop();
        *self.stackobj_as_mut_varef(&mut destination) = value;
    }

    binop_method!(binop_add, +);
    binop_method!(binop_sub, -);
    binop_method!(binop_mul, *);
    binop_method!(binop_div, /);
    
    fn pop(&mut self) -> StackObject {
        self.stack.pop().expect("Popped off empty stack")
    }

    fn stackobj_as_varref<'b>(&'b self, var: &'b StackObject) -> &'b Variable {
        match &var {
            StackObject::RegisterVar(idx) => self.registers[*idx].as_ref().unwrap(),
            StackObject::FreeVar(var) => &var,
            StackObject::IndexVar(indices) => {
                let mut var: &Variable = self.registers[indices[0]].as_ref().unwrap();
                for idx in indices.iter().skip(1) {
                    if let Variable::Array(items) = var {
                        var = &items[*idx];
                    } else {
                        panic!("Indexing into non-array");
                    }
                };
                var
            },
            _ => panic!("Non-variable found on the stack")
        }
    }

    fn stackobj_as_var(&self, var: StackObject) -> Variable {
        match var {
            StackObject::RegisterVar(idx) => self.registers[idx].clone().unwrap(),
            StackObject::FreeVar(var) => var,
            StackObject::IndexVar(indices) => {
                let mut var: &Variable = self.registers[indices[0]].as_ref().unwrap();
                for idx in indices.iter().skip(1) {
                    if let Variable::Array(items) = var {
                        var = &items[*idx];
                    } else {
                        panic!("Indexing into non-array");
                    }
                };
                var.clone()
            },
            _ => panic!("Non-variable found on the stack")
        }
    }

    fn stackobj_as_mut_varef<'b>(&'b mut self, var: &'b mut StackObject) -> &'b mut Variable {
        match var {
            StackObject::RegisterVar(idx) => self.registers[*idx].as_mut().unwrap(),
            StackObject::FreeVar(var) => var,
            StackObject::IndexVar(indices) => {
                let mut var: &mut Variable = self.registers[indices[0]].as_mut().unwrap();
                for idx in indices.iter().skip(1) {
                    if let Variable::Array(items) = var {
                        var = &mut items[*idx];
                    } else {
                        panic!("Indexing into non-array");
                    }
                };
                var
            },
            _ => panic!("Non-variable found on the stack")
        }
    }

    fn pull(&mut self) {
        let mut stackvar = self.pop();
        let var: &mut Variable = self.stackobj_as_mut_varef(&mut stackvar);
        let value = if let Variable::Array(items) = var {
                items.pop().expect("Pulling off empty array")
        } else {
            panic!("Trying to pull from a non-array")
        };
        self.stack.push(StackObject::FreeVar(value));
    }

    pub fn debug_print(&self) {
        println!("registers: {:#?}\nStack: {:#?}\n----------", self.registers, self.stack);
    }
}

