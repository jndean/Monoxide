
use std::collections::HashMap;

use crate::ast;
use crate::interpreter;
use interpreter::Instruction;


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
pub struct CompilerCtx<'a> {
    func_names: &'a Vec<String>,
    consts: Vec<interpreter::Variable>,

    free_registers: Vec<usize>,
    local_variables: HashMap<String, Variable>,
    num_registers: u16
}


impl<'a> CompilerCtx<'a> {
    pub fn new(func_names: &Vec<String>) -> CompilerCtx {
        CompilerCtx{
            func_names,
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

#[derive(Default, Debug)]
pub struct Code {
    fwd: Vec<Instruction>,
    bkwd: Vec<Instruction>,
    f2b_links: Vec<(usize, usize)>,
    b2f_links: Vec<(usize, usize)>
}

impl Code {
    pub fn new() -> Code {
        Default::default()
    }

    pub fn with_capacity(l1: usize, l2: usize) -> Code {
        Code{
            fwd: Vec::with_capacity(l1),
            bkwd: Vec::with_capacity(l2),
            f2b_links: Vec::new(),
            b2f_links: Vec::new()
        }
    }

    pub fn link_fwd2bkwd(&mut self) {
        self.f2b_links.push((self.fwd.len(), self.bkwd.len()));
        // Insert dummy instruction //
        self.fwd.push(Instruction::Reverse{idx: 0});
    }
    
    pub fn link_bkwd2fwd(&mut self) {
        self.b2f_links.push((self.bkwd.len(), self.fwd.len()));
        // Insert dummy instruction //
        self.bkwd.push(Instruction::Reverse{idx: 0});
    }

    pub fn push_fwd(&mut self, x: Instruction) {
        self.fwd.push(x);
    }

    pub fn push_bkwd(&mut self, x: Instruction) {
        self.bkwd.push(x);
    }

    pub fn extend_fwd(&mut self, instructions: Vec<Instruction>) {
        self.fwd.extend(instructions);
    }
    
    pub fn extend_bkwd(&mut self, instructions: Vec<Instruction>) {
        self.bkwd.extend(instructions.into_iter().rev());
    }

    pub fn fwd_len(&mut self) -> usize {
        self.fwd.len()
    }

    pub fn bkwd_len(&mut self) -> usize {
        self.bkwd.len()
    }

    pub fn extend(&mut self, other: Code) {
        let Code{fwd, bkwd, f2b_links, b2f_links} = other;
        let (flen, blen) = (self.fwd.len(), self.bkwd.len());
        self.fwd.extend(fwd);
        self.bkwd.extend(bkwd);
        for (f, b) in f2b_links.into_iter() {
            self.f2b_links.push((f + flen, b + blen));
        }
        for (b, f) in b2f_links.into_iter() {
            self.b2f_links.push((b + blen, f + flen));
        }
    }

    pub fn finalise(code: Code) -> interpreter::Code {
        let Code{mut fwd, mut bkwd, f2b_links, b2f_links} = code;
        bkwd.reverse();
        for (f, b) in f2b_links.into_iter() {
            let b = bkwd.len() - b;
            match fwd[f] {
                Instruction::Reverse{idx: _} => fwd[f] = Instruction::Reverse{idx: b},
                _ => panic!()
            }
        }
        for (b, f) in b2f_links.into_iter() {
            let b = bkwd.len() - b;
            match bkwd[b] {
                Instruction::Reverse{idx: _} => bkwd[b] = Instruction::Reverse{idx: f},
                _ => panic!()
            }
        }
        interpreter::Code{fwd, bkwd}
    }
}


impl ast::ExpressionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        match &self {
            ast::ExpressionNode::Fraction(valbox) => valbox.compile(ctx),
            ast::ExpressionNode::Lookup(valbox) => valbox.compile(ctx),
            ast::ExpressionNode::Binop(valbox) => valbox.compile(ctx),
            ast::ExpressionNode::ArrayLiteral(valbox) => valbox.compile(ctx)
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
        let register = ctx.lookup_local(&self.name);
        let mut instructions = vec![Instruction::LoadRegister{register}];
        
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


impl ast::ArrayLiteralNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let mut ret = Vec::with_capacity(self.items.len() + 1);
        for item in self.items.iter().rev() {
            ret.extend(item.compile(ctx));
        }
        ret.push(Instruction::CreateArray{size: self.items.len()});
        ret
    }
}


impl ast::StatementNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        match self {
            ast::StatementNode::LetUnlet(valbox) => valbox.compile(ctx),
            ast::StatementNode::If(valbox) => valbox.compile(ctx),
            ast::StatementNode::Modop(valbox) => valbox.compile(ctx),
            ast::StatementNode::Catch(valbox) => valbox.compile(ctx)
        }
    }
}


impl ast::LetUnletNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        let mut code = Code::new();
        if self.is_unlet {
            let register = ctx.free_local(&self.name);
            code.push_fwd(Instruction::FreeRegister{register});
            code.push_bkwd(Instruction::StoreRegister{register});
            code.extend_bkwd(self.rhs.compile(ctx));
        } else {
            let register = ctx.create_local(&self.name);
            code.extend_fwd(self.rhs.compile(ctx));
            code.push_fwd(Instruction::StoreRegister{register});
            code.push_bkwd(Instruction::FreeRegister{register});
        }
        code
    }
}


impl ast::ModopNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        let lookup = self.lookup.compile(ctx);
        let rhs = self.rhs.compile(ctx);
        let bkwd_op = match self.op {
            Instruction::BinopAdd => Instruction::BinopSub,
            Instruction::BinopSub => Instruction::BinopAdd,
            Instruction::BinopMul => Instruction::BinopDiv,
            Instruction::BinopDiv => Instruction::BinopMul,
            _ => unreachable!()
        };

        let capacity = lookup.len() + rhs.len() + 3;
        let mut code = Code::with_capacity(capacity, capacity);

        code.extend_fwd(lookup.clone());
        code.push_fwd(Instruction::LoadNoPop);
        code.extend_fwd(rhs.clone());
        code.push_fwd(self.op.clone());
        code.push_fwd(Instruction::Store);

        code.push_bkwd(Instruction::Store);
        code.push_bkwd(bkwd_op);
        code.extend_bkwd(rhs);
        code.push_bkwd(Instruction::LoadNoPop);
        code.extend_bkwd(lookup);
        
        code
    }
}


impl ast::IfNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        let fwd_expr = self.fwd_expr.compile(ctx);
        let bkwd_expr = self.bkwd_expr.compile(ctx);
        let mut if_block = Code::new();
        for stmt in self.if_stmts.iter() {
            if_block.extend(stmt.compile(ctx));
        }
        let mut else_block = Code::new();
        for stmt in self.else_stmts.iter() {
            else_block.extend(stmt.compile(ctx));
        }
        let if_bkwd_len = if_block.bkwd_len() as isize;
        let else_bkwd_len = else_block.bkwd_len() as isize;
        
        let mut code = Code::with_capacity(
            if_block.fwd_len() + else_block.fwd_len() + fwd_expr.len() + 2, 
            if_block.bkwd_len() + else_block.bkwd_len() + bkwd_expr.len() + 2);
        
        code.extend_fwd(fwd_expr);
        code.push_fwd(Instruction::JumpIfFalse{
            delta: (if_block.fwd_len() + 1) as isize
        });
        code.extend(if_block);
        code.push_fwd(Instruction::Jump{
            delta: else_block.fwd_len() as isize
        });
        code.push_bkwd(Instruction::Jump{delta: if_bkwd_len});
        code.extend(else_block);
        code.push_bkwd(Instruction::JumpIfTrue{delta: else_bkwd_len + 1});
        code.extend_bkwd(bkwd_expr);

        code
    }
}

impl ast::CatchNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        let mut code = Code::new();
        code.extend_fwd(self.expr.compile(ctx));
        code.push_fwd(Instruction::JumpIfFalse{delta: 1});
        code.link_fwd2bkwd();
        code
    }
}

impl ast::FunctionNode {
    pub fn compile(&self, func_names: &Vec<String>) -> interpreter::Function {
        let mut ctx = CompilerCtx::new(func_names);
        let mut code = Code::new();
        for stmt in self.stmts.iter() {
            code.extend(stmt.compile(&mut ctx));
        }

        let CompilerCtx{consts, num_registers, ..} = ctx;
        interpreter::Function{
            consts,
            code: Code::finalise(code),
            num_registers: num_registers as usize
        }
    }
}

impl ast::Module {
    pub fn compile(&self) -> interpreter::Module {
        let func_names: Vec<String> = 
            self.functions.iter().map(|f| f.name.clone()).collect();
        interpreter::Module{
            functions: self.functions.iter().map(|f| f.compile(&func_names)).collect()
        }
    }
}