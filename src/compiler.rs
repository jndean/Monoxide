
use std::collections::HashMap;

use crate::ast;
use crate::interpreter;
use interpreter::{Instruction, Statement};


#[derive(Debug)]
struct Variable {
    pub register: usize
}

impl Variable {
    fn new(register: usize) -> Variable {
        Variable{
            register
        }
    }
}


#[derive(Debug)]
pub struct CompilerCtx {
    consts: Vec<interpreter::Variable>,

    free_registers: Vec<usize>,
    local_variables: HashMap<String, Variable>,
    num_registers: u16
}


impl CompilerCtx {
    pub fn new() -> CompilerCtx {
        CompilerCtx{
            consts: Vec::new(), 
            free_registers: Vec::new(),
            local_variables: HashMap::new(),
            num_registers: 0
        }
    }

    fn add_const(&mut self, val: interpreter::Variable) -> usize {
        for (i, existing) in self.consts.iter().enumerate() {
            if *existing == val {return i}
        }
        self.consts.push(val);
        self.consts.len() - 1
    }

    fn lookup_local(&mut self, name: &str) -> usize {
        match self.local_variables.get(name) {
            Some(var) => var.register,
            None => panic!("Accessing non-existant local")
        }
    }

    fn create_local(&mut self, name: &str) -> usize {
        if self.local_variables.contains_key(name) {
            panic!("Initialising a variable that already exists");
        };
        let register = match self.free_registers.pop() {
            Some(r) => r,
            None => {
                self.num_registers += 1;
               (self.num_registers - 1) as usize
            }
        };
        self.local_variables.insert(
            name.to_string(),
            Variable::new(register)
        );
        register
    }

    fn free_local(&mut self, name: &str) -> usize {
        match self.local_variables.remove(name) {
            Some(var) => {
                self.free_registers.push(var.register);
                var.register
            },
            None => panic!("Freeing non-existant local")
        }
    }

}


impl ast::ExpressionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        match &self {
            ast::ExpressionNode::Fraction(valbox) => valbox.compile(ctx),
            ast::ExpressionNode::Lookup(valbox) => valbox.compile(ctx),
            ast::ExpressionNode::Binop(valbox) => valbox.compile(ctx),
        }
    }
}

impl ast::FractionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let idx = ctx.add_const(
            interpreter::Variable::Frac(self.value.clone())
        );
        vec![Instruction::LoadConst{idx}]
    }
}


impl ast::LookupNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let idx = ctx.lookup_local(&self.name);
        let mut instructions = vec![Instruction::LoadRegister{idx}];
        
        // Handle array lookups //
        if !self.indices.is_empty() {
            for index in self.indices.iter() {
                instructions.extend(index.compile(ctx));
            }
            instructions.push(Instruction::CreateIndex{size: self.indices.len()});
            instructions.push(Instruction::Load);
        }
        instructions
    }
}


impl ast::BinopNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let mut ret = Vec::new();
        ret.extend(self.lhs.compile(ctx));
        ret.extend(self.rhs.compile(ctx));
        ret.push(self.op.clone());
        ret
    }
}


pub fn stmt_from_fwd_bkwd(fwd: Vec<Instruction>, bkwd: Vec<Instruction>) -> Statement {
    let mut stmt = Vec::with_capacity(fwd.len() + bkwd.len() + 2);
    stmt.push(Instruction::IJumpIfBackwards{delta: (fwd.len() + 1) as isize});
    stmt.extend(fwd);
    stmt.push(Instruction::IJump{delta: bkwd.len() as isize});
    stmt.extend(bkwd);
    stmt
}

impl ast::StatementNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Statement> {
        match &self {
            ast::StatementNode::LetUnlet(valbox) => valbox.compile(ctx)
        }
    }
}


impl ast::LetUnletNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Statement> {
        let register = if self.is_unlet {
            ctx.free_local(&self.name)
        } else {
            ctx.create_local(&self.name)
        };

        let mut fwd_stmt = Vec::new();
        fwd_stmt.extend(self.rhs.compile(ctx));
        fwd_stmt.push(Instruction::StoreRegister{idx:register});

        let bkwd_stmt = vec![Instruction::FreeRegister{idx: register}];

        if self.is_unlet {
            vec![stmt_from_fwd_bkwd(bkwd_stmt, fwd_stmt)]
        } else {
            vec![stmt_from_fwd_bkwd(fwd_stmt, bkwd_stmt)]
        }
    }
}
