
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
            ast::StatementNode::LetUnlet(valbox) => valbox.compile(ctx),
            ast::StatementNode::If(valbox) => valbox.compile(ctx)
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

impl ast::IfNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Statement> {
        let fwd_expr = self.fwd_expr.compile(ctx);
        let bkwd_expr = self.bkwd_expr.compile(ctx);
        let if_block = self.if_stmts.iter()
                                    .map(|stmt| stmt.compile(ctx))
                                    .flatten().collect::<Vec<Statement>>();
        let else_block = self.else_stmts.iter()
                                        .map(|stmt| stmt.compile(ctx))
                                        .flatten().collect::<Vec<Statement>>();
        
        let mut start_stmt = Vec::with_capacity(fwd_expr.len() + 2);
        start_stmt.push(Instruction::SJumpIfBackwards{delta: -1});
        start_stmt.extend(fwd_expr);
        start_stmt.push(Instruction::SJumpIfFalse{delta: (if_block.len() + 2) as isize});

        let mid_stmt = vec![
            Instruction::SJumpIfBackwards{delta: -(if_block.len() as isize) - 2},
            Instruction::SJump{delta: (else_block.len() + 2) as isize}
        ];

        let mut end_stmt = Vec::with_capacity(bkwd_expr.len() + 2);
        end_stmt.push(Instruction::SJumpIfForwards{delta: 1});
        end_stmt.extend(bkwd_expr);
        end_stmt.push(Instruction::SJumpIfTrue{delta: -(else_block.len() as isize) - 2});

        let mut ret = Vec::with_capacity(if_block.len() + else_block.len() + 3);
        ret.push(start_stmt);
        ret.extend(if_block);
        ret.push(mid_stmt);
        ret.extend(else_block);
        ret.push(end_stmt);
        ret
    }
}

impl ast::Module {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Statement> {
        self.stmts.iter()
                  .map(|stmt| stmt.compile(ctx))
                  .flatten()
                  .collect()
    }
}