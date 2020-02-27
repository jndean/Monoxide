
extern crate num_rational;
extern crate num_bigint;

use std::fmt;
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
    Jump{delta: isize},
    JumpIfTrue{delta: isize},    
    JumpIfFalse{delta: isize},
    CreateIndex{size: usize},
    Call{idx: usize},
    Uncall{idx: usize},
    Quit,
    DebugPrint,
}

#[derive(Debug)]
pub enum StackObject {
    FreeVar(Variable),
    RegisterVar(usize),
    LookupVar(Vec<usize>),
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
    //pub borrow_params: Vec<String>
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

    pub fn run(module: &Module) {
        let main = module.functions.get(0).expect("No main function");
        let mut interpreter = Interpreter {
            functions: &module.functions,
            stack: Vec::new(),
            scope_stack: Vec::new()
        };
        interpreter.call(0, true);
        interpreter.execute();
        interpreter.debug_print();
    }

    pub fn execute(&mut self) -> () {

        let scope = self.scope_stack.last().unwrap();
        let mut instructions = if scope.forwards {&scope.code.fwd} else {&scope.code.bkwd};

        'instruction_loop: loop {

            let instruction = match instructions.get(self.scope_stack.last().unwrap().ip) {
                Some(inst) => inst,
                None => break
            };

            println!("IP: {}, {:?}", self.scope_stack.last().unwrap().ip, instruction);

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
                Instruction::CreateIndex{size} => self.create_lookup(*size),
                Instruction::Jump{delta} => self.jump(*delta),
                Instruction::JumpIfTrue{delta} => self.jump_if_true(*delta),
                Instruction::JumpIfFalse{delta} => self.jump_if_false(*delta),
                Instruction::Pull => self.pull(),
                Instruction::Call{idx} => self.call(*idx, true),
                Instruction::Uncall{idx} => unimplemented!(),
                Instruction::Reverse{idx} => {
                    let scope: &mut Scope = self.scope_stack.last_mut().unwrap();
                    scope.forwards = !scope.forwards;
                    scope.ip = *idx;
                    instructions = if scope.forwards {&scope.code.fwd} else {&scope.code.bkwd};
                    continue 'instruction_loop;
                }
                Instruction::Quit => break 'instruction_loop,
                Instruction::DebugPrint => self.debug_print(),
            }
            
            self.scope_stack.last_mut().unwrap().ip += 1;
        }
    }

    pub fn call(&mut self, func_idx: usize, forwards: bool) {
        let func: &'a Function = self.functions.get(func_idx)
                                               .expect("Call to undefined function");
        self.scope_stack.push(
            Scope{
                code: &func.code,
                consts: &func.consts,
                registers: vec![None; func.num_registers],
                ip: if forwards {0} else {func.code.bkwd.len() - 1},
                forwards
            }
        );
    }

    #[inline]
    fn jump(&mut self, delta: isize) {
        let scope = self.scope_stack.last_mut().unwrap();
        scope.ip = ((scope.ip as isize) + delta) as usize;
    }

    #[inline]
    fn jump_if_true(&mut self, delta: isize) {
        let stackvar = self.pop();
        if self.stackobj_as_varref(&stackvar).to_bool() {
            self.jump(delta);
        }
    }

    #[inline]
    fn jump_if_false(&mut self, delta: isize) {
        let stackvar = self.pop();
        if !self.stackobj_as_varref(&stackvar).to_bool() {
            self.jump(delta);
        }
    }

    #[inline]
    fn load_const(&mut self, idx: usize) {
        let scope = self.scope_stack.last().unwrap();
        self.stack.push(StackObject::FreeVar(scope.consts[idx].clone()));
    }  

    #[inline]
    fn load_register(&mut self, idx: usize) {
        self.stack.push(StackObject::RegisterVar(idx));
    }

    #[inline]
    fn store_register(&mut self, idx: usize) {
        let stackvar = self.pop();
        let var = self.stackobj_as_var(stackvar);
        let scope = self.scope_stack.last_mut().unwrap();
        scope.registers[idx] = Some(var);
    }

    #[inline]
    fn free_register(&mut self, idx: usize) {
        let scope = self.scope_stack.last_mut().unwrap();
        scope.registers[idx] = None;
    }

    fn create_lookup(&mut self, size: usize) {
        let mut index: Vec<usize> = Vec::with_capacity(size);
        for _ in 0..size {
            let stackvar = self.pop();
            if let Variable::Frac(value) = self.stackobj_as_varref(&stackvar) {
                index.push(value.to_integer().to_usize().unwrap());
            } else {
                panic!("Trying to use non-number as array index");
            };
        };
        match self.pop() {
           StackObject::RegisterVar(register) => {
               index.push(register);
               index.push(self.scope_stack.len()-1);
           },
           StackObject::LookupVar(indices) => {
               index.extend(indices.into_iter().rev());
           },
           _ => unreachable!()
        };

        index.reverse();
        self.stack.push(StackObject::LookupVar(index));
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
            StackObject::FreeVar(var) => &var,
            StackObject::RegisterVar(idx) => {
                self.scope_stack.last().unwrap().registers[*idx].as_ref().unwrap()
            },
            StackObject::LookupVar(indices) => {
                let scope: &Scope = &self.scope_stack[indices[0]];
                let mut var: &Variable = scope.registers[indices[1]].as_ref().unwrap();
                for idx in indices.iter().skip(2) {
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
            StackObject::FreeVar(var) => var,
            StackObject::RegisterVar(idx) => {
                self.scope_stack.last().unwrap().registers[idx].as_ref().unwrap().clone()
            },
            StackObject::LookupVar(indices) => {
                let scope: &Scope = &self.scope_stack[indices[0]];
                let mut var: &Variable = scope.registers[indices[1]].as_ref().unwrap();
                for idx in indices.iter().skip(2) {
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
            StackObject::FreeVar(var) => var,
            StackObject::RegisterVar(idx) => {
                self.scope_stack.last_mut().unwrap().registers[*idx].as_mut().unwrap()
            },
            StackObject::LookupVar(indices) => {
                let scope: &mut Scope = &mut self.scope_stack[indices[0]];
                let mut var: &mut Variable = scope.registers[indices[1]].as_mut().unwrap();
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
        println!(
            "registers: {:#?}\nStack: {:#?}\n----------", 
            self.scope_stack.last().unwrap().registers,
            self.stack);
    }
}

