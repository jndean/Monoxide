
extern crate num_rational;
extern crate num_bigint;

use std::fmt;
use std::cell::{RefCell, Ref};
use std::mem::replace;
use num_traits::cast::ToPrimitive;
use std::ops::Index;
use std::rc::Rc;
use num_bigint::BigInt;


pub type Fraction = num_rational::BigRational;


#[derive(PartialEq, Clone)]
pub enum Variable {
    Frac(Fraction),
    Array(Vec<Rc<RefCell<Variable>>>),
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Frac(val) => write!(f, "{}", val),
            Variable::Array(vec) => write!(f, "Array({:#?})", vec)
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

    fn to_usize(&self) -> usize {
        match self {
            Variable::Frac(value) => value.to_integer().to_usize().unwrap(),
            Variable::Array(_) => panic!("Index is not a number")
        }
    }

    fn deep_copy(&self) -> Self {
        match self {
            Variable::Frac(value) => Variable::Frac(value.clone()),
            Variable::Array(items) => {
                Variable::Array(
                    items.iter().map(
                        |item| Rc::new(RefCell::new(item.borrow().deep_copy()))
                    ).collect()
                )
            }
        }
    }
}

impl Index<usize> for Variable {
    type Output = Rc<RefCell<Variable>>;

    fn index(&self, idx: usize) -> &Self::Output {
        match self {
            Variable::Array(items) => &items[idx],
            Variable::Frac(_) => panic!("Indexing into number")
        }
    }
}


#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst{idx: usize},
    LoadRegister{register: usize},
    StoreRegister{register: usize},
    FreeRegister{register: usize},
    Subscript{size: usize},
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
    CreateArray{size: usize},
    Call{idx: usize},
    Uncall{idx: usize},
    DuplicateRef,
    CopyVar,
    Quit,
    DebugPrint,
}

#[derive(Debug)]
pub enum StackObject {
    Var(Rc<RefCell<Variable>>),
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
    scope_stack: Vec<Scope<'a>>,

    code: &'a Code,
    ip: usize,
    forwards: bool,
    registers: Vec<Option<Rc<RefCell<Variable>>>>,
    consts: &'a Vec<Variable>
}


#[derive(Debug)]
pub struct Scope<'a> {
    code: &'a Code,
    ip: usize,
    forwards: bool,
    registers: Vec<Option<Rc<RefCell<Variable>>>>,
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
            let rhs = self.pop_var();
            let lhs = self.pop_var();
            let result = match (&*lhs.borrow(), &*rhs.borrow()) {
                (Variable::Frac(left), Variable::Frac(right)) => {
                    Variable::Frac(left $op right)
                },
                (Variable::Array(_), Variable::Array(_)) => {
                    unimplemented!()
                },
                _ => panic!("Adding incompatible types")
            };
            self.stack.push(StackObject::Var(Rc::new(RefCell::new(result))));
        }
    };
}

impl<'a> Interpreter<'a> {

    pub fn run(module: &Module) {
        let main = module.functions.get(0).expect("No main function");
        let mut interpreter = Interpreter {
            functions: &module.functions,
            stack: Vec::new(),
            scope_stack: Vec::new(),
            code: &main.code,
            ip: 0,
            forwards: true,
            registers: vec![None; main.num_registers],
            consts: &main.consts
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
                Instruction::Store => self.store(),
                Instruction::Subscript{size} => self.subscript(*size),
                Instruction::DuplicateRef => self.duplicate_ref(),
                Instruction::CopyVar => self.copy_var(),
                Instruction::BinopAdd => self.binop_add(),
                Instruction::BinopSub => self.binop_sub(),
                Instruction::BinopMul => self.binop_mul(),
                Instruction::BinopDiv => self.binop_div(),
                Instruction::CreateArray{size} => self.create_array(*size),
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
                code      : replace(&mut self.code     , &func.code),
                consts    : replace(&mut self.consts   , &func.consts),
                registers : replace(&mut self.registers, vec![None; func.num_registers]),
                ip        : replace(&mut self.ip       , 0),
                forwards  : replace(&mut self.forwards , forwards)
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
        if self.pop_var().borrow().to_bool() {
            self.jump(delta);
        }
    }

    #[inline]
    fn jump_if_false(&mut self, delta: isize) {
        if !self.pop_var().borrow().to_bool() {
            self.jump(delta);
        }
    }

    #[inline]
    fn load_const(&mut self, idx: usize) {
        self.stack.push(StackObject::Var(Rc::new(RefCell::new(
            self.consts[idx].clone()
        ))));
    }  

    #[inline]
    fn load_register(&mut self, idx: usize) {
        let new_var_ref = Rc::clone(self.registers[idx].as_ref().unwrap());
        self.stack.push(StackObject::Var(new_var_ref));
    }

    #[inline]
    fn store_register(&mut self, idx: usize) {
        self.registers[idx] = Some(self.pop_var());
    }

    #[inline]
    fn free_register(&mut self, idx: usize) {
        self.registers[idx] = None;
    }

    pub fn create_array(&mut self, size: usize) {
        let mut items = Vec::with_capacity(size);
        for _ in 0..size {
            let mut item = self.pop_var();
            if Rc::strong_count(&item) > 1 {
                let val = item.borrow().deep_copy();
                item = Rc::new(RefCell::new(val));
            };
            items.push(item);
        }
        self.stack.push(StackObject::Var(Rc::new(RefCell::new(
            Variable::Array(items)
        ))));
    }

    fn subscript(&mut self, size: usize) {
        let mut var_ref = self.pop_var();
        for _ in 0..size {
            let index = self.pop_var().borrow().to_usize();
            let new_ref = Rc::clone(&Ref::map(var_ref.borrow(), |var| &var[index]));
            var_ref = new_ref;
        }
        self.stack.push(StackObject::Var(var_ref));
    }

    fn store(&mut self) {
        let value = self.pop_var().borrow().clone();
        *self.pop_var().borrow_mut() = value;
    }

    fn duplicate_ref(&mut self) {
        let new = match self.stack.last().unwrap() {
            StackObject::Var(cell) => StackObject::Var(Rc::clone(cell)),
            _ => panic!("Trying to duplicate non-variable")
        };
        self.stack.push(new);
    }

    fn copy_var(&mut self) {
        let var = self.pop_var();
        if Rc::strong_count(&var) > 1 {
            self.stack.push(
                StackObject::Var(Rc::new(RefCell::new(
                    var.borrow().deep_copy()
                ))
            ));
        } else {
            self.stack.push(StackObject::Var(var));
        }
    }

    binop_method!(binop_add, +);
    binop_method!(binop_sub, -);
    binop_method!(binop_mul, *);
    binop_method!(binop_div, /);
    
    #[inline]
    fn pop(&mut self) -> StackObject {
        self.stack.pop().expect("Popped off empty stack")
    }

    #[inline]
    fn pop_var(&mut self) -> Rc<RefCell<Variable>> {
        match self.pop() {
            StackObject::Var(x) => x,
            _ => panic!("Non-variable found on the stack")
        }
    }

    #[inline]
    fn pull(&mut self) {
        let new_var = match &mut *self.pop_var().borrow_mut() {
            Variable::Array(items) => match items.pop() {
                Some(item) => item,
                None => panic!("Pulling from empty array")
            },
            Variable::Frac(_) => panic!("Pulling from number")
        };
        self.stack.push(StackObject::Var(new_var));
    }

    pub fn debug_print(&self) {
        println!(
            "registers: {:#?}\nStack: {:#?}\n----------", 
            self.registers,
            self.stack);
    }
}

