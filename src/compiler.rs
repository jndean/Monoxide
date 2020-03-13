

use crate::syntaxtree as ST;
use crate::interpreter;
use interpreter::Instruction;


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

    pub fn append_fwd(&mut self, mut instructions: Vec<Instruction>) {
        self.fwd.append(&mut instructions);
    }
    
    pub fn append_bkwd(&mut self, instructions: Vec<Instruction>) {
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


impl ST::ExpressionNode {
    pub fn compile(&self) -> Vec<Instruction> {
        match &self {
            ST::ExpressionNode::FractionNode(valbox) => valbox.compile(),
            ST::ExpressionNode::LookupNode(valbox) => valbox.compile(),
            ST::ExpressionNode::BinopNode(valbox) => valbox.compile(),
            ST::ExpressionNode::ArrayLiteralNode(valbox) => valbox.compile()
        }
    }
}

impl ST::FractionNode {
    pub fn compile(&self) -> Vec<Instruction> {
        vec![Instruction::LoadConst{idx: self.const_idx}]
    }
}


impl ST::LookupNode {
    pub fn compile(&self) -> Vec<Instruction> {
        let mut instructions = Vec::with_capacity(self.indices.len()+1);        
        for index in self.indices.iter().rev() {
            instructions.extend(index.compile());
        }
        instructions.push(Instruction::LoadRegister{register:self.register});
        if !self.indices.is_empty() {
            instructions.push(Instruction::Subscript{size: self.indices.len()});
        }
        instructions
    }
}


impl ST::BinopNode {
    pub fn compile(&self) -> Vec<Instruction> {
        let mut ret = Vec::new();
        ret.extend(self.lhs.compile());
        ret.extend(self.rhs.compile());
        ret.push(self.op.clone());
        ret
    }
}


impl ST::ArrayLiteralNode {
    pub fn compile(&self) -> Vec<Instruction> {
        let mut ret = Vec::with_capacity(self.items.len() + 1);
        for item in self.items.iter().rev() {
            ret.extend(item.compile());
        }
        ret.push(Instruction::CreateArray{size: self.items.len()});
        ret
    }
}


impl ST::StatementNode {
    pub fn compile(&self) -> Code {
        match self {
            ST::StatementNode::LetUnletNode(valbox) => valbox.compile(),
            ST::StatementNode::RefUnrefNode(valbox) => valbox.compile(),
            ST::StatementNode::IfNode(valbox) => valbox.compile(),
            ST::StatementNode::ModopNode(valbox) => valbox.compile(),
            ST::StatementNode::CatchNode(valbox) => valbox.compile(),
            ST::StatementNode::CallChainNode(valbox) => valbox.compile()
        }
    }
}


impl ST::LetUnletNode {
    pub fn compile(&self) -> Code {
        let mut code = Code::new();
        if self.is_unlet {
            code.push_fwd(Instruction::FreeRegister{register: self.register});

            code.push_bkwd(Instruction::StoreRegister{register: self.register});
            code.push_bkwd(Instruction::UniqueVar);
            code.append_bkwd(self.rhs.compile());
        } else {
            code.append_fwd(self.rhs.compile());
            code.push_fwd(Instruction::UniqueVar);
            code.push_fwd(Instruction::StoreRegister{register: self.register});

            code.push_bkwd(Instruction::FreeRegister{register: self.register});
        }
        code
    }
}


impl ST::RefUnrefNode {
    pub fn compile(&self) -> Code {
        let mut create_ref = self.rhs.compile();
        create_ref.push(Instruction::StoreRegister{register: self.register});
        let remove_ref = vec![Instruction::FreeRegister{register: self.register}];

        let mut code = Code::new();
        if self.is_unref{
            code.append_fwd(remove_ref);
            code.append_bkwd(create_ref);
        } else {
            code.append_fwd(create_ref);
            code.append_bkwd(remove_ref);
        }
        code
    }
}


impl ST::ModopNode {
    pub fn compile(&self) -> Code {
        let lookup = self.lookup.compile();
        let rhs = self.rhs.compile();
        let bkwd_op = match self.op {
            Instruction::BinopAdd => Instruction::BinopSub,
            Instruction::BinopSub => Instruction::BinopAdd,
            Instruction::BinopMul => Instruction::BinopDiv,
            Instruction::BinopDiv => Instruction::BinopMul,
            _ => unreachable!()
        };

        let capacity = lookup.len() + rhs.len() + 3;
        let mut code = Code::with_capacity(capacity, capacity);

        code.append_fwd(lookup.clone());
        code.push_fwd(Instruction::DuplicateRef);
        code.append_fwd(rhs.clone());
        code.push_fwd(self.op.clone());
        code.push_fwd(Instruction::Store);

        code.push_bkwd(Instruction::Store);
        code.push_bkwd(bkwd_op);
        code.append_bkwd(rhs);
        code.push_bkwd(Instruction::DuplicateRef);
        code.append_bkwd(lookup);
        
        code
    }
}


impl ST::IfNode {
    pub fn compile(&self) -> Code {
        let fwd_expr = self.fwd_expr.compile();
        let bkwd_expr = self.bkwd_expr.compile();
        let mut if_block = Code::new();
        for stmt in self.if_stmts.iter() {
            if_block.extend(stmt.compile());
        }
        let mut else_block = Code::new();
        for stmt in self.else_stmts.iter() {
            else_block.extend(stmt.compile());
        }
        let if_bkwd_len = if_block.bkwd_len() as isize;
        let else_bkwd_len = else_block.bkwd_len() as isize;
        
        let mut code = Code::with_capacity(
            if_block.fwd_len() + else_block.fwd_len() + fwd_expr.len() + 2, 
            if_block.bkwd_len() + else_block.bkwd_len() + bkwd_expr.len() + 2);
        
        code.append_fwd(fwd_expr);
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
        code.append_bkwd(bkwd_expr);

        code
    }
}

impl ST::CatchNode {
    pub fn compile(&self) -> Code {
        let mut code = Code::new();
        code.append_fwd(self.expr.compile());
        code.push_fwd(Instruction::JumpIfFalse{delta: 1});
        code.link_fwd2bkwd();
        code
    }
}


impl ST::CallUncallNode {
    pub fn compile(&self) -> Code {        
        let mut code = Code::new();
        if self.is_uncall {
            code.push_bkwd(Instruction::Call{idx: self.func_idx});
            code.push_fwd(Instruction::Uncall{idx: self.func_idx});
        } else {
            code.push_fwd(Instruction::Call{idx: self.func_idx});
            code.push_bkwd(Instruction::Uncall{idx: self.func_idx});
        }
        code
    }
}

impl ST::CallChainNode {
    pub fn compile(&self) -> Code {
        let mut code = Code::new();
        for call in self.calls.iter() {
            code.extend(call.compile());
        }
        code
    }
}

impl ST::FunctionNode {
    pub fn compile(&self, func_names: &Vec<String>) -> interpreter::Function {
        let mut code = Code::new();
        for stmt in self.stmts.iter() {
            code.extend(stmt.compile());
        }

        interpreter::Function{
            consts: self.consts.clone(),
            code: Code::finalise(code),
            num_registers: self.num_registers
        }
    }
}

impl ST::Module {
    pub fn compile(&self) -> interpreter::Module {
        let mut main_idx = None;
        let mut func_names = Vec::with_capacity(self.functions.len());
        for (i, f) in self.functions.iter().enumerate() {
            func_names.push(f.name.clone());
            if f.name == "main" {main_idx = Some(i)}
        }
        let functions = self.functions.iter()
                                      .map(|f| f.compile(&func_names))
                                      .collect();
        interpreter::Module{main_idx, functions}
    }
}