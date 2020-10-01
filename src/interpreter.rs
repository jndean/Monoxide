
extern crate num_rational;
extern crate num_bigint;

use std::fmt;
use std::cell::{RefCell, Ref};
use std::mem::replace;
use num_traits::cast::ToPrimitive;
use num_traits::identities::{Zero, One};
use std::ops::Index;
use std::rc::Rc;


pub type Fraction = num_rational::BigRational;

fn fraction_to_f64(x: &Fraction) -> f64 {
    match (x.numer().to_f64(), x.denom().to_f64()) {
        (Some(n), Some(d)) => n/d,
        _ => panic!("Rationals exceed f64 precision")
    }
}

#[derive(PartialEq, Clone)]
pub enum Variable {
    Frac(Fraction),
    Array(Vec<Rc<RefCell<Variable>>>)
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
            Variable::Frac(value) => !value.is_zero(),
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
struct IterState {
    pub idx: usize,
    pub register: usize,
    pub var: Rc<RefCell<Variable>>
}

#[derive(Debug)]
enum StackObject {
    Var(Rc<RefCell<Variable>>),
    Iter(IterState)
}


#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst{idx: usize},
    LoadRegister{register: usize},
    StoreRegister{register: usize},
    FreeRegister{register: usize},
    Subscript{size: usize},
    Store,
    Pull{register: usize},
    Push{register: usize},
    BinopAdd, BinopSub, BinopMul, BinopDiv,
    BinopOr, BinopAnd, BinopXor, 
    BinopLeq, BinopGeq, BinopLess, BinopGreat,
    BinopEq, BinopNeq,
    BinopIDiv, BinopMod, BinopPow,
    UniopNeg, UniopNot,
    Reverse{idx: usize},
    Jump{ip: usize},
    JumpIfTrue{ip: usize},
    JumpIfFalse{ip: usize},
    RelativeJump{delta: isize},
    RelativeJumpIfTrue{delta: isize},
    RelativeJumpIfFalse{delta: isize},
    CreateArray{size: usize},
    Call{idx: usize},
    Uncall{idx: usize},
    DuplicateRef,
    UniqueVar,
    CreateIter{register: usize},
    StepIter{ip: usize},
    Print{idx: usize},
    Quit,
    DebugPrint,
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
    consts: &'a Vec<Variable>,
    strings: &'a Vec<String>
}


#[derive(Debug)]
pub struct Scope<'a> {
    code: &'a Code,
    ip: usize,
    forwards: bool,
    registers: Vec<Option<Rc<RefCell<Variable>>>>,
    consts: &'a Vec<Variable>,
    strings: &'a Vec<String>
}


#[derive(Debug)]
pub struct Function {
    pub code: Code,
    pub consts: Vec<Variable>,
    pub strings: Vec<String>,
    pub num_registers: usize
}

#[derive(Debug)]
pub struct Module {
    pub main_idx: Option<usize>,
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
                    unimplemented!();
                },
                _ => panic!("Applying binop \"{}\" to incompatible types", stringify!($op))
            };
            self.stack.push(StackObject::Var(Rc::new(RefCell::new(result))));
        }
    };
}

macro_rules! bincomp_method {
    ($name:ident, $op:tt) => {
        fn $name (&mut self) {
            let rhs = self.pop_var();
            let lhs = self.pop_var();
            let result = match (&*lhs.borrow(), &*rhs.borrow()) {
                (Variable::Frac(left), Variable::Frac(right)) => {
                    if left $op right {Variable::Frac(Fraction::one())}
                    else              {Variable::Frac(Fraction::zero())}
                },
                _ => panic!("Applying binop \"{}\" to incompatible types", stringify!($op))
            };
            self.stack.push(StackObject::Var(Rc::new(RefCell::new(result))));
        }
    };
}

macro_rules! binlogic_method {
    ($name:ident, $op:tt) => {
        fn $name (&mut self) {
            let rhs = self.pop_var();
            let lhs = self.pop_var();
            let result = lhs.borrow().to_bool() $op rhs.borrow().to_bool();
            let var = if result {Variable::Frac(Fraction::one())}
                      else      {Variable::Frac(Fraction::zero())};
            self.stack.push(StackObject::Var(Rc::new(RefCell::new(var))));
        }
    };
}

impl<'a> Interpreter<'a> {

    pub fn run(module: &Module) {
        let main_idx = module.main_idx.expect("No main function");
        let main = module.functions.get(main_idx).unwrap();
        let mut interpreter = Interpreter {
            functions: &module.functions,
            stack: Vec::new(),
            scope_stack: Vec::new(),
            code: &main.code,
            ip: 0,
            forwards: true,
            registers: vec![None; main.num_registers],
            consts: &main.consts,
            strings: &main.strings
        };
        interpreter.execute();
        interpreter.debug_print();
    }

    pub fn execute(&mut self) -> () {

        'refresh_instructions: loop{

            let instructions = if self.forwards {&self.code.fwd} 
                               else             {&self.code.bkwd};

            loop {

                let instruction = match instructions.get(self.ip) {
                    Some(inst) => inst,
                    None => {
                        if self.scope_stack.is_empty() { 
                            break 'refresh_instructions;
                        } else {
                            self.end_call(); 
                            continue 'refresh_instructions;
                        };
                    }
                };

                println!("{} IP: {}, {:?}", if self.forwards {"FWD"} else {"BKWD"}, self.ip, instruction);

                match instruction {
                    Instruction::LoadConst{idx} => self.load_const(*idx),
                    Instruction::LoadRegister{register} => self.load_register(*register),
                    Instruction::StoreRegister{register} => self.store_register(*register),
                    Instruction::FreeRegister{register} => self.free_register(*register),
                    Instruction::Store => self.store(),
                    Instruction::Subscript{size} => self.subscript(*size),
                    Instruction::DuplicateRef => self.duplicate_ref(),
                    Instruction::UniqueVar => self.copy_var(),
                    Instruction::BinopAdd => self.binop_add(),
                    Instruction::BinopSub => self.binop_sub(),
                    Instruction::BinopMul => self.binop_mul(),
                    Instruction::BinopDiv => self.binop_div(),
                    Instruction::BinopMod => self.binop_mod(),
                    Instruction::BinopIDiv => self.binop_idiv(),
                    Instruction::BinopPow => self.binop_pow(),
                    Instruction::BinopLess => self.binop_less(),
                    Instruction::BinopLeq => self.binop_leq(),
                    Instruction::BinopGreat => self.binop_great(),
                    Instruction::BinopGeq => self.binop_geq(),
                    Instruction::BinopEq => self.binop_eq(),
                    Instruction::BinopNeq => self.binop_neq(),
                    Instruction::BinopAnd => self.binop_and(),
                    Instruction::BinopOr => self.binop_or(),
                    Instruction::BinopXor => self.binop_xor(),
                    Instruction::UniopNeg => self.uniop_neg(),
                    Instruction::UniopNot => self.uniop_not(),
                    Instruction::CreateArray{size} => self.create_array(*size),
                    Instruction::Pull{register} => self.pull(*register),
                    Instruction::Push{register} => self.push(*register),
                    Instruction::Print{idx} => self.print(*idx),
                    Instruction::CreateIter{register} => self.create_iter(*register),
                    Instruction::StepIter{ip} => {self.step_iter(*ip); continue 'refresh_instructions},
                    
                    Instruction::Jump{ip} => {self.jump(*ip); continue 'refresh_instructions},
                    Instruction::JumpIfTrue{ip} => {self.jump_if_true(*ip); continue 'refresh_instructions},
                    Instruction::JumpIfFalse{ip} => {self.jump_if_false(*ip); continue 'refresh_instructions},
                    Instruction::Call{idx} => {self.call(*idx, true); continue 'refresh_instructions},
                    Instruction::Uncall{idx} => {self.call(*idx, false); continue 'refresh_instructions},
                    Instruction::Reverse{idx} => {self.reverse(*idx); continue 'refresh_instructions;}
                    Instruction::Quit => break 'refresh_instructions,
                    Instruction::DebugPrint => self.debug_print(),

                    Instruction::RelativeJump{delta: _} => unimplemented!("RelativeJump"),
                    Instruction::RelativeJumpIfTrue{delta: _} => unimplemented!("RelativeJumpIfTrue"),
                    Instruction::RelativeJumpIfFalse{delta: _} => unimplemented!("RelativeJumpIfFalse")
                }
                
                self.ip += 1;
            }
        }
    }

    pub fn call(&mut self, func_idx: usize, forwards: bool) {
        let func: &'a Function = self.functions.get(func_idx)
                                               .expect("Call to undefined function");
        self.scope_stack.push(
            Scope{
                code      : replace(&mut self.code     , &func.code),
                consts    : replace(&mut self.consts   , &func.consts),
                strings   : replace(&mut self.strings  , &func.strings),
                registers : replace(&mut self.registers, vec![None; func.num_registers]),
                ip        : replace(&mut self.ip       , 0),
                forwards  : replace(&mut self.forwards , forwards)
            }
        );
    }

    pub fn end_call(&mut self) {
        let scope = self.scope_stack.pop().unwrap();
        self.code = scope.code;
        self.consts = scope.consts;
        self.registers = scope.registers;
        self.ip = scope.ip + 1;
        self.forwards = scope.forwards;
    }

    #[inline]
    fn jump(&mut self, ip: usize) {
        self.ip = ip;
    }

    #[inline]
    fn jump_if_true(&mut self, ip: usize) {
        if self.pop_var().borrow().to_bool() {
            self.jump(ip);
        } else {
            self.ip += 1;
        }
    }

    #[inline]
    fn jump_if_false(&mut self, ip: usize) {
        if !self.pop_var().borrow().to_bool() {
            self.jump(ip);
        } else {
            self.ip += 1;
        }
    }

    #[inline]
    fn reverse(&mut self, ip: usize) {
        self.forwards = !self.forwards;
        self.ip = ip;
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
    binop_method!(binop_mod, %);

    bincomp_method!(binop_less,  < );
    bincomp_method!(binop_leq,   <=);
    bincomp_method!(binop_great, > );
    bincomp_method!(binop_geq,   >=);
    
    binlogic_method!(binop_and, &&);
    binlogic_method!(binop_or,  ||);
    binlogic_method!(binop_xor, ^) ;

    fn binop_idiv(&mut self) {
        let rhs = self.pop_var();
        let lhs = self.pop_var();
        let result = match (&*lhs.borrow(), &*rhs.borrow()) {
            (Variable::Frac(left), Variable::Frac(right)) => {
                Variable::Frac((left/right).trunc())
            },
            _ => panic!("Applying binop \"//\" to incompatible types")
        };
        self.stack.push(StackObject::Var(Rc::new(RefCell::new(result))));
    }

    fn binop_pow(&mut self) {
        let rhs = self.pop_var();
        let lhs = self.pop_var();
        let result = match (&*lhs.borrow(), &*rhs.borrow()) {
            (Variable::Frac(left), Variable::Frac(right)) => {
                let value = fraction_to_f64(left).powf(fraction_to_f64(right));
                let value = Fraction::from_float(value).expect("Computing power created an infinite float");
                Variable::Frac(value)
            },
            _ => panic!("Applying binop \"**\" to incompatible types")
        };
        self.stack.push(StackObject::Var(Rc::new(RefCell::new(result))));
    }

    fn binop_eq(&mut self) {
        let rhs = self.pop_var();
        let lhs = self.pop_var();
        let value = if *lhs.borrow() == *rhs.borrow() {Fraction::one()}
                    else                              {Fraction::zero()};
        let var = Rc::new(RefCell::new(Variable::Frac(value)));
        self.stack.push(StackObject::Var(var));
    }

    fn binop_neq(&mut self) {
        let rhs = self.pop_var();
        let lhs = self.pop_var();
        let value = if *lhs.borrow() != *rhs.borrow() {Fraction::one()}
                    else                              {Fraction::zero()};
        let var = Rc::new(RefCell::new(Variable::Frac(value)));
        self.stack.push(StackObject::Var(var));
    }

    fn uniop_neg(&mut self) {
        let expr = self.pop_var();
        let result = match &*expr.borrow() {
            Variable::Frac(x) => Variable::Frac(-x),
            Variable::Array(_) => panic!("Takinging the negative of an array"),
        };
        self.stack.push(StackObject::Var(Rc::new(RefCell::new(result))));
    }

    fn uniop_not(&mut self) {
        let expr = self.pop_var();
        let result = if expr.borrow().to_bool() {
            Variable::Frac(Fraction::zero())
        } else {
            Variable::Frac(Fraction::one())
        };
        self.stack.push(
            StackObject::Var(Rc::new(RefCell::new(result)))
        );
    }

    fn pull(&mut self, register: usize) {
        let new_var = match &mut *self.pop_var().borrow_mut() {
            Variable::Array(items) => match items.pop() {
                Some(item) => item,
                None => panic!("Pulling from empty array")
            },
            Variable::Frac(_) => panic!("Pulling from number")
        };
        replace(
            self.registers.get_mut(register).unwrap(),
            Some(new_var)
        );
    }

    fn push(&mut self, register: usize) {
        let src_ref = replace(
            self.registers.get_mut(register).unwrap(),
            None
        ).unwrap();
        match &mut *self.pop_var().borrow_mut() {
            Variable::Array(items) => items.push(src_ref),
            Variable::Frac(_) => panic!("Pushing onto number")
        }
    }

    fn print(&mut self, idx: usize) {
        print!("{}", self.strings[idx]);
    } 

    fn create_iter(&mut self, register: usize) {
        let var = self.pop_var();
        let iter_state = IterState{register, var, idx: 0};
        self.stack.push(StackObject::Iter(iter_state));
    }

    fn step_iter(&mut self, ip: usize) {
        // Get iterator state off the stack
        let (idx, var, register) = match self.stack.last_mut() {
            Some(StackObject::Iter(IterState{idx, var, register})) => (idx, var.borrow(), *register),
            _ => panic!("No IterState on the stack")
        };
        let array = match &*var {
            Variable::Array(array) => array,
            Variable::Frac(_) => panic!("For loop iterator must be an array, not a number")
        };

        // Step iteration, or jump to after loop if iterator exhausted
        if (self.forwards && *idx >= array.len()) || (!self.forwards && *idx == 0) {
            drop(var);
            self.pop();
            self.registers[register] = None;
            self.jump(ip);
        } else {
            self.registers[register] = Some(Rc::clone(&array[*idx]));
            *idx += 1;
            self.ip += 1;
        };
    }

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

    pub fn debug_print(&self) {
        println!(
            "registers: {:#?}\nStack: {:#?}\n----------", 
            self.registers,
            self.stack);
    }
}

