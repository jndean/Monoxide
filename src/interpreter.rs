
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
    Array(Vec<Rc<RefCell<Variable>>>),
    Str(String)
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Frac(val) => write!(f, "{}", val),
            Variable::Array(vec) => write!(f, "Array({:#?})", vec),
            Variable::Str(string) => write!(f, "{}", string)
        }
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Frac(val) => write!(f, "{}", val),
            Variable::Str(string) => write!(f, "{}", string),
            Variable::Array(vec) => {
                let mut out = String::new();
                if vec.len() > 0 {
                    for item in vec[0..vec.len() - 1].iter() {
                        out.push_str(&item.borrow().to_string());
                        out.push_str(", ");
                    }
                    out.push_str(&vec.last().unwrap().borrow().to_string());
                }
                write!(f, "[{}]", out)
            }
        }
    }
}

impl Variable {
    fn to_bool(&self) -> bool {
        match self {
            Variable::Frac(value) => !value.is_zero(),
            Variable::Array(items) => items.len() > 0,
            Variable::Str(string) => string.len() > 0
        }
    }

    fn to_usize(&self) -> usize {
        match self {
            Variable::Frac(value) => value.to_integer().to_usize().unwrap(),
            _ => panic!("Index is not a number")
        }
    }

    fn deep_copy(&self) -> Self {
        match self {
            Variable::Frac(value) => Variable::Frac(value.clone()),
            Variable::Str(value) => Variable::Str(value.clone()),
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
            _ => panic!("Indexing is only supported by arrays")
        }
    }
}

#[derive(Debug, Clone)]
struct IterState {
    pub idx: isize,
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
    LoadGlobalRegister{register: usize},
    LoadRegister{register: usize},
    StoreRegister{register: usize},
    StoreGlobalRegister{register: usize},
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
    ArrayLiteral{size: usize},
    ArrayRepeat,
    Call{idx: usize},
    Uncall{idx: usize},
    DuplicateRef,
    UniqueVar,
    CreateIter{register: usize},
    StepIter{ip: usize},
    Print{count: isize},
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
    global_registers: Vec<Option<Rc<RefCell<Variable>>>>,
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
}


#[derive(Debug)]
pub struct Module {
    pub main_idx: Option<usize>,
    pub global_func_idx: usize,
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
        let global_func = module.functions.get(module.global_func_idx).unwrap();
        let mut interpreter = Interpreter {
            functions: &module.functions,
            stack: Vec::new(),
            scope_stack: Vec::new(),
            code: &global_func.code,
            ip: 0,
            forwards: true,
            registers: Vec::new(),
            global_registers: vec![None; global_func.num_registers],
            consts: &global_func.consts
        };
        interpreter.execute();
        interpreter.call(main_idx, true);
        interpreter.execute();
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

                // println!("{} IP: {}, {:?}", if self.forwards {"FWD"} else {"BKWD"}, self.ip, instruction);

                match instruction {
                    Instruction::LoadConst{idx} => self.load_const(*idx),
                    Instruction::LoadRegister{register} => self.load_register(*register),
                    Instruction::LoadGlobalRegister{register} => self.load_global_register(*register),
                    Instruction::StoreRegister{register} => self.store_register(*register),
                    Instruction::StoreGlobalRegister{register} => self.store_global_register(*register),
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
                    Instruction::ArrayLiteral{size} => self.array_literal(*size),
                    Instruction::ArrayRepeat => self.array_repeat(),
                    Instruction::Pull{register} => self.pull(*register),
                    Instruction::Push{register} => self.push(*register),
                    Instruction::Print{count} => self.print(*count),
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
        let func: &'a Function = self.functions.get(func_idx).expect("Call to undefined function");
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
    fn load_global_register(&mut self, idx: usize) {
        let new_var_ref = Rc::clone(self.global_registers[idx].as_ref().unwrap());
        self.stack.push(StackObject::Var(new_var_ref));
    }

    #[inline]
    fn store_register(&mut self, idx: usize) {
        self.registers[idx] = Some(self.pop_var());
    }

    #[inline]
    fn store_global_register(&mut self, idx: usize) {
        self.global_registers[idx] = Some(self.pop_var());
    }

    #[inline]
    fn free_register(&mut self, idx: usize) {
        self.registers[idx] = None;
    }

    pub fn array_literal(&mut self, size: usize) {
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

    pub fn array_repeat(&mut self) {

        let dimensions = self.pop_var();
        let content = self.pop_var();

        let dimensions: Vec<_> = match &*dimensions.borrow() {
            Variable::Array(array) => array.iter().map(|d| d.borrow().to_usize()).collect(),
            Variable::Frac(value) => vec![value.to_integer().to_usize().unwrap()],
            Variable::Str(_) => panic!("Array repetition dimensions must be specified in an array")
        };
        
        fn recursive_array_maker(content: &Variable, dims: &[usize]) -> Vec<Rc<RefCell<Variable>>> {
            let mut ret = Vec::with_capacity(dims[0]);
            if dims.len() == 1 {
                for _ in 0..dims[0] {
                    ret.push(Rc::new(RefCell::new(content.deep_copy())));
                }
            } else {
                for _ in 0..dims[0] {
                    ret.push(Rc::new(RefCell::new(Variable::Array(
                        recursive_array_maker(content, &dims[1..])
                    ))));
                }
            }
            ret
        }

        let array = recursive_array_maker(&*content.borrow(), dimensions.as_slice());
        let var = Rc::new(RefCell::new(Variable::Array(array)));
        self.stack.push(StackObject::Var(var));
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
            _ => panic!("The negation operation is only supported by numbers"),
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
            _ => panic!("Pulling is only supported by arrays")
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
            _ => panic!("Pushing is only supported by arrays")
        }
    }

    fn print(&mut self, count: isize) {
        for _ in 0..count.abs() {
            print!("{}", self.pop_var().borrow());
        }
        if count < 0 {
            print!("\n");
        }
    } 

    fn create_iter(&mut self, register: usize) {
        let var = self.pop_var();
        let array_len = match &*var.borrow() {
            Variable::Array(array) => array.len(),
            _ => panic!("For loop iterator is not an array")
        };
        let idx = if self.forwards {-1}
                  else {array_len as isize};
        let iter_state = IterState{register, var, idx};
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
            _ => panic!("For loop iterator is not an array")
        };

        // Step iteration, or jump to after loop if iterator exhausted
        if (self.forwards && *idx == array.len() as isize - 1) || 
           (!self.forwards && *idx == 0) {
            drop(var);
            self.pop();
            self.registers[register] = None;
            self.jump(ip);
        } else {
            *idx += if self.forwards {1} else {-1};
            self.registers[register] = Some(Rc::clone(&array[*idx as usize]));
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
            "registers: {:#?}\nglobals: {:#?}\nStack: {:#?}\n----------", 
            self.registers,
            self.global_registers,
            self.stack);
    }
}

