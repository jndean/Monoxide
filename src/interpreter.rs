
use std::fmt;
use std::mem::replace;
use num_traits::cast::ToPrimitive;

extern crate num_rational;
extern crate num_bigint;

pub type Fraction = num_rational::BigRational;
type BigInt = num_bigint::BigInt;


#[derive(PartialEq, Clone)]
pub enum Variable {
    Frac(Fraction),
    Array(Vec<Variable>),
    LocalArrayRef(usize, Vec<usize>)
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Frac(val) => write!(f, "{}", val),
            Variable::LocalArrayRef(register, indices) => {
                write!(f, "LocalArrayRef({}, {:?})", register, indices)
            },
            Variable::Array(vec) => write!(f, "Array({:?})", vec)
        }
    }
}


#[derive(Debug)]
pub enum Instruction {
    LoadConst{idx: usize},
    LoadLocal{idx: usize},
    StoreLocal{idx: usize},
    FreeLocal{idx: usize},
    BinopAdd,
    BinopSub,
    BinopMul,
    BinopDiv,
    Reverse,
    IJump{delta: isize},  // Instruction Jump //
    SJump{delta: isize},  // Statement Jump //
    IJumpIfBackwards{delta: isize},
    SJumpIfBackwards{delta: isize},
    CreateArrayRef{size: usize},
    ArrayRead,
    ArrayReadNoPop,
    ArrayWrite,
    Quit,
    DebugPrint,
}

pub type Statement = Vec<Instruction>;


#[derive(Debug)]
pub enum StackObject<'s> {
    FreeVar(Variable),
    LocalVar(usize),
    RefVar(&'s Variable)
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

            let statement = match self.code.get(self.stmt_pos) {
                Some(stmt) => stmt,
                None => break
            };

            'instruction_loop: loop {

                let instruction = match statement.get(self.inst_pos) {
                    Some(inst) => inst,
                    None => {
                        if self.forwards { 
                            self.s_jump(1)
                        } else { 
                            self.s_jump(-1)
                        };
                        break
                    }
                };

                match instruction {
                    Instruction::LoadConst{idx} => self.load_const(*idx),
                    Instruction::LoadLocal{idx} => self.load_local(*idx),
                    Instruction::StoreLocal{idx} => self.store_local(*idx),
                    Instruction::FreeLocal{idx} => self.free_local(*idx),
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
                    Instruction::CreateArrayRef{size} => self.create_array_lookup(*size),
                    Instruction::ArrayRead => self.array_read(),
                    Instruction::ArrayReadNoPop => self.array_read_nopop(),
                    Instruction::ArrayWrite => self.array_write(),

                    Instruction::Quit => break 'statement_loop
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
        self.stack.push(StackObject::RefVar(&self.consts[idx]));
    }  

    fn load_local(&mut self, idx: usize) {
        self.stack.push(StackObject::LocalVar(idx));
    }

    fn store_local(&mut self, idx: usize) {
        self.locals[idx] = Some(match self.pop() {
            StackObject::RefVar(var) => var.clone(),
            StackObject::LocalVar(idx) => self.locals[idx].clone().unwrap(),
            StackObject::FreeVar(var) => var,
            _ => panic!("Popped something other than a variable")
        });
    }

    fn free_local(&mut self, idx: usize) {
        self.locals[idx] = None;
    }

    fn create_array_lookup(&mut self, size: usize) {
        let mut list: Vec<usize> = Vec::with_capacity(size);
        for _ in 0..size {
            let var = self.pop();
            list.push(
                match self.stackobj_as_var(&var) {
                    Variable::Frac(val) => val.to_integer().to_usize().unwrap(),
                    _ => panic!("Trying to use non-number as array index")
                }
            );
        }
        list.reverse();
        let array_register = self.pop();
        let array_register = match array_register {
            StackObject::LocalVar(register) => register,
            _ => unreachable!()
        };
        self.stack.push(
            StackObject::FreeVar(
                Variable::LocalArrayRef(array_register, list)
            )
        );
    }

    fn array_read(&mut self) {
        let stack_lookup = self.pop();
        let stack_lookup = self.stackobj_as_var(&stack_lookup);
        let var: &Variable = self.deref_localarrayref(stack_lookup);
        let var = StackObject::FreeVar(var.clone());
        self.stack.push(var);
    }

    fn array_read_nopop(&mut self) {
        let stack_lookup: &StackObject = &self.stack[self.stack.len()-1];
        let stack_lookup: &Variable = self.stackobj_as_var(stack_lookup);
        let var: &Variable = self.deref_localarrayref(stack_lookup);
        let var = StackObject::FreeVar(var.clone());
        self.stack.push(var);
    }

    fn array_write(&mut self) {
        let value = self.pop();
        let value = self.stackobj_as_var(&value);
        let value = value.clone();

        let stack_lookup = self.pop();
        let var_lookup = match stack_lookup {
            StackObject::FreeVar(var) => var,
            _ => unreachable!()
        };
        let (register, indices) = match var_lookup {
            Variable::LocalArrayRef(reg, ind) => (reg, ind),
            _ => unreachable!()
        };
        let mut var_ref: &mut Variable = self.locals[register].as_mut().unwrap();

        for i in 0 .. indices.len() - 1 {
            var_ref = match var_ref {
                Variable::Array(vec) => &mut vec[indices[i]],
                _ => panic!("Trying to index into non-array")
            };
        }
        match var_ref {
            Variable::Array(vec) => vec[indices[indices.len() - 1]] = value,
            _ => panic!("Trying to index into non-array")
        };
    }

    fn binop_add(&mut self) {
        let rhs = self.pop();
        let lhs = self.pop();
        let lhs: &Variable = self.stackobj_as_var(&lhs);
        let rhs: &Variable = self.stackobj_as_var(&rhs);

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
        let lhs: &Variable = self.stackobj_as_var(&lhs);
        let rhs: &Variable = self.stackobj_as_var(&rhs);

        let result = match (lhs, rhs) {
            (Variable::Frac(left), Variable::Frac(right)) => Variable::Frac(left - right),
            (Variable::Array(_), Variable::Array(_)) => unimplemented!(),
            _ => panic!("Subtracting incompatible types")
        };

        self.stack.push(StackObject::FreeVar(result));
    }

    fn binop_mul(&mut self) {
        let rhs = self.pop();
        let lhs = self.pop();
        let lhs: &Variable = self.stackobj_as_var(&lhs);
        let rhs: &Variable = self.stackobj_as_var(&rhs);

        let result = match (lhs, rhs) {
            (Variable::Frac(left), Variable::Frac(right)) => Variable::Frac(left * right),
            (Variable::Array(_), Variable::Array(_)) => unimplemented!(),
            _ => panic!("Multiplying incompatible types")
        };

        self.stack.push(StackObject::FreeVar(result));
    }

    fn binop_div(&mut self) {
        let rhs = self.pop();
        let lhs = self.pop();
        let lhs: &Variable = self.stackobj_as_var(&lhs);
        let rhs: &Variable = self.stackobj_as_var(&rhs);

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

    fn stackobj_as_var(&'a self, var: &'a StackObject) -> &'a Variable {
        match &var {
            StackObject::RefVar(var_ref) => *var_ref,
            StackObject::LocalVar(idx) => self.locals[*idx].as_ref().unwrap(),
            StackObject::FreeVar(var) => &var,
            _ => panic!()
        }
    }

    fn deref_localarrayref(&'a self, localarrayref: &Variable) -> &'a Variable {
        let (register, indices) = match localarrayref {
            Variable::LocalArrayRef(register, indices) => (*register, indices),
            _ => panic!("Expected LocalArrayRef")
        };
        
        let mut var_ref: &Variable = self.locals[register].as_ref().unwrap();
        for index in indices.iter() {
            var_ref = match var_ref {
                Variable::Array(vector) => &vector[*index],
                /*Variable::LocalArrayRef(register, indices) => ,
                Variable::Frac(_) => panic!("Trying to index into a number")*/
                _ => panic!("Indexing into a non-array?")
            };
        };
        var_ref
    }

    fn debug_print(&self) {
        println!("Locals: {:#?}\nStack: {:#?}\n----------", self.locals, self.stack);
    }
}

