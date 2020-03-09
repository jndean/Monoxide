
use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::rc::Rc;

use crate::parsetree;
use crate::interpreter;
use interpreter::Instruction;


#[derive(Debug)]
pub enum Variable {
    Reg(usize, Vec<String>), // A variable that owns its own register
    Ref(String)  // A variable that is just a symbolic reference to another
}

#[derive(Debug)]
pub struct Reference {
    is_interior: bool,
    register: usize,
    var: Rc<NewVariable>
}

#[derive(Debug)]
pub struct NewVariable {
    exteriors: RefCell<HashSet<String>>,
    interiors: RefCell<HashSet<String>>
}

impl Reference {
    fn new(name: String, register: usize, is_interior: bool) -> Reference {
        let mut exteriors = HashSet::new();
        exteriors.insert(name);
        Reference {
            is_interior,
            register,
            var: Rc::new(NewVariable{
                exteriors: RefCell::new(exteriors),
                interiors: RefCell::new(HashSet::new())
            })
        }
    }
}


#[derive(Debug)]
pub struct CompilerCtx<'a> {
    func_names: &'a Vec<String>,
    consts: Vec<interpreter::Variable>,

    free_registers: Vec<usize>,
    local_variables: HashMap<String, Reference>,
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

    fn lookup_local(&self, name: &str) -> usize {
        let v_ref = self.local_variables.get(name);
        if let Some(Reference{register, ..}) = v_ref {
            *register
        } else { 
            panic!("Use of non-existant variable") 
        }
    }

    fn get_free_register(&mut self) -> usize {
        match self.free_registers.pop() {
            Some(r) => r,
            None => {
                self.num_registers += 1;
                (self.num_registers - 1) as usize
            }
        }
    }

    fn create_variable(&mut self, name: &str) -> usize {
        if self.local_variables.contains_key(name) {
            panic!("Initialising a variable that already exists");
        };
        let register = self.get_free_register();
        self.local_variables.insert(
            name.to_string(),
            Reference::new(name.to_string(), register, false)
        );
        register
    }

    pub fn create_ref(&mut self, name: &str, existing_name: &str, interior: bool) -> usize {
        if self.local_variables.contains_key(name) {
            panic!("Initialising a reference that already exists");
        };
        
        let (is_interior, mut register, var) = match self.local_variables.get(existing_name) {
            None => panic!("Referencing a non-existant variable"),
            Some(Reference{is_interior, register, var}) => {
                (*is_interior || interior, *register, Rc::clone(var))
            }
        };
        if is_interior { 
            register = self.get_free_register();
            var.interiors.borrow_mut().insert(name.to_string());
        } else {
            var.exteriors.borrow_mut().insert(name.to_string());
        }
    
        self.local_variables.insert(
            name.to_string(),
            Reference{is_interior, register, var}
        );
        register
    }


    pub fn remove_ref(&mut self, name: &str, other_name: &str, indices: bool) -> usize {

        match self.local_variables.remove(name) {
            None => panic!("Removing non-existant reference"),
            Some(Reference{is_interior, register, var}) => {
                let is_interior = is_interior || indices;
                
                // Check the other is a shared ref
                match self.local_variables.get(other_name) {
                    None => panic!("Unreferencing a non-existant variable"),
                    Some(Reference{var: other_var, is_interior: other_is_interior, ..}) => {
                        let mut ok = Rc::ptr_eq(&var, other_var);  // Point to the same var
                        ok &= !(*other_is_interior && !is_interior);  // Can't deref exterior using interior
                        if !ok { panic!("Unreferencing using incorrect variable") };
                    }
                }

                // Deref
                var.interiors.borrow_mut().remove(name);
                var.exteriors.borrow_mut().remove(name);
                register
            }
        }
    }

    fn remove_variable(&mut self, name: &str) -> usize {
        match self.local_variables.remove(name) {
            None => panic!("Uninitialising non-existant variable"),
            Some(Reference{var, register, ..}) => {
                if !var.interiors.borrow().is_empty() 
                        || var.exteriors.borrow().len() > 1 {
                    panic!("Uninitialising variable with other refs");
                }
                self.free_registers.push(register);
                register
            }
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


impl parsetree::ExpressionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        match &self {
            parsetree::ExpressionNode::Fraction(valbox) => valbox.compile(ctx),
            parsetree::ExpressionNode::Lookup(valbox) => valbox.compile(ctx),
            parsetree::ExpressionNode::Binop(valbox) => valbox.compile(ctx),
            parsetree::ExpressionNode::ArrayLiteral(valbox) => valbox.compile(ctx)
        }
    }
}

impl parsetree::FractionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let idx = ctx.add_const(
            interpreter::Variable::Frac(self.value.clone())
        );
        vec![Instruction::LoadConst{idx}]
    }
}


impl parsetree::LookupNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let register = ctx.lookup_local(&self.name);
        let mut instructions = Vec::with_capacity(self.indices.len()+1);        
        for index in self.indices.iter().rev() {
            instructions.extend(index.compile(ctx));
        }
        instructions.push(Instruction::LoadRegister{register});
        if !self.indices.is_empty() {
            instructions.push(Instruction::Subscript{size: self.indices.len()});
        }
        instructions
    }
}


impl parsetree::BinopNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let mut ret = Vec::new();
        ret.extend(self.lhs.compile(ctx));
        ret.extend(self.rhs.compile(ctx));
        ret.push(self.op.clone());
        ret
    }
}


impl parsetree::ArrayLiteralNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let mut ret = Vec::with_capacity(self.items.len() + 1);
        for item in self.items.iter().rev() {
            ret.extend(item.compile(ctx));
        }
        ret.push(Instruction::CreateArray{size: self.items.len()});
        ret
    }
}


impl parsetree::StatementNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        match self {
            parsetree::StatementNode::LetUnlet(valbox) => valbox.compile(ctx),
            parsetree::StatementNode::RefUnref(valbox) => valbox.compile(ctx),
            parsetree::StatementNode::If(valbox) => valbox.compile(ctx),
            parsetree::StatementNode::Modop(valbox) => valbox.compile(ctx),
            parsetree::StatementNode::Catch(valbox) => valbox.compile(ctx)
        }
    }
}


impl parsetree::LetUnletNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        let mut code = Code::new();
        if self.is_unlet {
            let register = ctx.remove_variable(&self.name);
            code.push_fwd(Instruction::FreeRegister{register});
            code.push_bkwd(Instruction::StoreRegister{register});
            code.append_bkwd(self.rhs.compile(ctx));
        } else {
            let register = ctx.create_variable(&self.name);
            code.append_fwd(self.rhs.compile(ctx));
            code.push_fwd(Instruction::StoreRegister{register});
            code.push_bkwd(Instruction::FreeRegister{register});
        }
        code
    }
}


impl parsetree::RefUnrefNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        let ctx_method = if self.is_unref { CompilerCtx::remove_ref }
                         else             { CompilerCtx::create_ref };
        let register = ctx_method(ctx,
            &self.name,
            &self.rhs.name,
            self.rhs.indices.len() > 0
        );

        let mut create_ref = self.rhs.compile(ctx);
        create_ref.push(Instruction::StoreRegister{register});
        let remove_ref = vec![Instruction::FreeRegister{register}];

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


impl parsetree::ModopNode {
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

        code.append_fwd(lookup.clone());
        code.push_fwd(Instruction::Duplicate);
        code.append_fwd(rhs.clone());
        code.push_fwd(self.op.clone());
        code.push_fwd(Instruction::Store);

        code.push_bkwd(Instruction::Store);
        code.push_bkwd(bkwd_op);
        code.append_bkwd(rhs);
        code.push_bkwd(Instruction::Duplicate);
        code.append_bkwd(lookup);
        
        code
    }
}


impl parsetree::IfNode {
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

impl parsetree::CatchNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Code {
        let mut code = Code::new();
        code.append_fwd(self.expr.compile(ctx));
        code.push_fwd(Instruction::JumpIfFalse{delta: 1});
        code.link_fwd2bkwd();
        code
    }
}

impl parsetree::FunctionNode {
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

impl parsetree::Module {
    pub fn compile(&self) -> interpreter::Module {
        let func_names: Vec<String> = 
            self.functions.iter().map(|f| f.name.clone()).collect();
        interpreter::Module{
            functions: self.functions.iter().map(|f| f.compile(&func_names)).collect()
        }
    }
}