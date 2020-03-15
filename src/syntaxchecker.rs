
use std::collections::{HashSet, HashMap};
use std::cell::RefCell;
use std::rc::Rc;

use crate::interpreter;
use crate::parsetree as PT;
use crate::syntaxtree as ST;


#[derive(Debug)]
pub struct Variable {
    exteriors: RefCell<HashSet<String>>,
    interiors: RefCell<HashSet<String>>
}

#[derive(Debug)]
pub struct Reference {
    is_interior: bool,
    register: usize,
    var: Rc<Variable>
}

impl Reference {
    fn new(name: String, register: usize, is_interior: bool) -> Reference {
        let mut exteriors = HashSet::new();
        exteriors.insert(name);
        Reference {
            is_interior,
            register,
            var: Rc::new(Variable{
                exteriors: RefCell::new(exteriors),
                interiors: RefCell::new(HashSet::new())
            })
        }
    }
}


#[derive(Debug)]
pub struct SyntaxContext<'a> {
    functions: &'a HashMap<String, ST::FunctionPrototype>,
    consts: Vec<interpreter::Variable>,
    free_registers: Vec<usize>,
    local_variables: HashMap<String, Reference>,
    num_registers: usize
}


impl<'a> SyntaxContext<'a> {
    pub fn new(functions: &'a HashMap<String, ST::FunctionPrototype>) -> SyntaxContext<'a> {
        SyntaxContext{
            functions,
            consts: Vec::new(),
            free_registers: Vec::new(),
            local_variables: HashMap::new(),
            num_registers: 0
        }
    }

    fn init_func(&mut self, borrows: &Vec<String>, steals: &Vec<String>) 
                 -> (Vec<usize>, Vec<usize>) {
        //TODO: create_variable is not the write thing to use //
        let borrows = borrows.iter().map(|name| self.create_variable(name)).collect();
        let steals = steals.iter().map(|name| self.create_variable(name)).collect();
        (borrows, steals)
    }

    fn end_func(&mut self, returns: &Vec<String>) -> Vec<usize> {
        //TODO: create_variable is not the write thing to use //
        returns.iter().map(|name| self.lookup_local(name)).collect()
    }

    fn add_const(&mut self, val: interpreter::Variable) -> usize {
        for (i, existing) in self.consts.iter().enumerate() {
            if *existing == val {return i}
        }
        self.consts.push(val);
        self.consts.len() - 1
    }

    fn lookup_function(&self, name: &str) -> usize {
        println!("Name: {}", name);
        println!("Prototypes: {:#?}", self.functions);
        let prototype = self.functions.get(name)
                                      .expect("Undefined function");
        prototype.id
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

    pub fn create_ref(&mut self, name: &str, lookup: &PT::LookupNode) -> usize {
        if self.local_variables.contains_key(name) {
            panic!("Initialising a reference that already exists");
        };
        
        let (is_interior, mut register, var) = match self.local_variables.get(&lookup.name) {
            None => panic!("Referencing a non-existant variable"),
            Some(Reference{is_interior, register, var}) => {
                (*is_interior || lookup.indices.len() > 0, *register, Rc::clone(var))
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


    pub fn remove_ref(&mut self, name: &str, lookup: &PT::LookupNode) -> usize {

        match self.local_variables.remove(name) {
            None => panic!("Removing non-existant reference"),
            Some(Reference{is_interior, register, var}) => {
                let is_interior = is_interior || lookup.indices.len() > 0;
                
                // Check the other name is a shared ref
                match self.local_variables.get(&lookup.name) {
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


impl ST::FractionNode {
    fn from(node: PT::FractionNode, ctx: &mut SyntaxContext) -> ST::FractionNode {
        let const_idx = ctx.add_const(
            interpreter::Variable::Frac(node.value)
        );
        ST::FractionNode{const_idx}
    }
}

impl ST::BinopNode {
    fn from(node: PT::BinopNode, ctx: &mut SyntaxContext) -> ST::BinopNode {
        ST::BinopNode{
            lhs: ST::ExpressionNode::from(node.lhs, ctx),
            rhs: ST::ExpressionNode::from(node.rhs, ctx),
            op: node.op
        }
    }
}

impl ST::ArrayLiteralNode {
    fn from(node: PT::ArrayLiteralNode, ctx: &mut SyntaxContext) -> ST::ArrayLiteralNode {
        ST::ArrayLiteralNode{
            items: node.items.into_iter()
                             .map(|i| ST::ExpressionNode::from(i, ctx))
                             .collect()
        }
    }
}

impl ST::LookupNode {
    fn from(node: PT::LookupNode, ctx: &mut SyntaxContext) -> ST::LookupNode {
        ST::LookupNode{
            register: ctx.lookup_local(&node.name),
            indices: node.indices.into_iter()
                                 .map(|i| ST::ExpressionNode::from(i, ctx))
                                 .collect()
        }
    }
}



impl ST::ExpressionNode {
    fn from(node: PT::ExpressionNode, ctx: &mut SyntaxContext) -> ST::ExpressionNode {
        macro_rules! passthrough {
            ($( $x:ident ),*) => {
                match node {
                    $(
                        PT::ExpressionNode::$x(valbox) =>
                        ST::ExpressionNode::$x(Box::new(ST::$x::from(*valbox, ctx)))
                    ,)*
        }   }   }
        passthrough! {FractionNode, BinopNode, ArrayLiteralNode, LookupNode}
    }
}


impl ST::LetUnletNode {
    fn from(node: PT::LetUnletNode, ctx: &mut SyntaxContext) -> ST::LetUnletNode {
        ST::LetUnletNode{
            is_unlet: node.is_unlet,
            register: if node.is_unlet {ctx.remove_variable(&node.name)}
                      else             {ctx.create_variable(&node.name)},
            rhs: ST::ExpressionNode::from(node.rhs, ctx)
        }
    }
}

impl ST::RefUnrefNode {
    fn from(node: PT::RefUnrefNode, ctx: &mut SyntaxContext) -> ST::RefUnrefNode {
        ST::RefUnrefNode{
            is_unref: node.is_unref, 
            register: if node.is_unref {ctx.remove_ref(&node.name, &node.rhs)}
                      else             {ctx.create_ref(&node.name, &node.rhs)},
            rhs: ST::LookupNode::from(node.rhs, ctx)
        }
    }
}

impl ST::ModopNode {
    fn from(node: PT::ModopNode, ctx: &mut SyntaxContext) -> ST::ModopNode {
        ST::ModopNode{
            lookup: ST::LookupNode::from(node.lookup, ctx),
            op: node.op,
            rhs: ST::ExpressionNode::from(node.rhs, ctx)
        }
    }
}

impl ST::IfNode {
    fn from(node: PT::IfNode, ctx: &mut SyntaxContext) -> ST::IfNode {
        let fwd_expr = ST::ExpressionNode::from(node.fwd_expr, ctx);
        let bkwd_expr = ST::ExpressionNode::from(node.bkwd_expr, ctx);
        let if_stmts = node.if_stmts.into_iter().map(|s| ST::StatementNode::from(s, ctx)).collect();
        let else_stmts = node.else_stmts.into_iter().map(|s| ST::StatementNode::from(s, ctx)).collect();
        ST::IfNode{fwd_expr, if_stmts, else_stmts, bkwd_expr}
    }
}

impl ST::CatchNode {
    fn from(node: PT::CatchNode, ctx: &mut SyntaxContext) -> ST::CatchNode {
        ST::CatchNode{expr: ST::ExpressionNode::from(node.expr, ctx)}
    }
}


impl ST::CallUncallNode {
    fn from(node: PT::CallUncallNode, ctx: &mut SyntaxContext) -> ST::CallUncallNode {
        ST::CallUncallNode{
            is_uncall: node.is_uncall,
            func_idx: ctx.lookup_function(&node.name),
            borrow_args: node.borrow_args.into_iter()
                                         .map(|a| ST::LookupNode::from(a, ctx))
                                         .collect()
        }
    }
}

impl ST::CallChainNode {
    fn from(node: PT::CallChainNode, ctx: &mut SyntaxContext) -> ST::CallChainNode {
        let mut stolen_args = Vec::with_capacity(node.stolen_args.len());
        for arg in node.stolen_args.into_iter() {
            stolen_args.push(ctx.lookup_local(&arg));
            ctx.local_variables.remove(&arg);
        }
        let calls = node.calls.into_iter()
                              .map(|c| ST::CallUncallNode::from(c, ctx))
                              .collect();                  
        let mut return_args = Vec::with_capacity(node.return_args.len());
        for arg in node.return_args.into_iter() {
            return_args.push(ctx.create_variable(&arg));
            // TODO: Using create variable is WRONG
        }
        ST::CallChainNode{calls, stolen_args, return_args}
    }
}

impl ST::FunctionNode {
    fn from(
        node: PT::FunctionNode, 
        func_lookup: &HashMap<String, ST::FunctionPrototype>
    ) -> ST::FunctionNode {

        let mut ctx = SyntaxContext::new(func_lookup);
        let (borrow_registers, steal_registers) = ctx.init_func(&node.borrow_params,
                                                                &node.steal_params);
        let stmts = node.stmts.into_iter()
                              .map(|s| ST::StatementNode::from(s, &mut ctx))
                              .collect();
        let return_registers = ctx.end_func(&node.return_params);

        ST::FunctionNode{
            stmts, borrow_registers, steal_registers, return_registers,
            consts: ctx.consts,
            num_registers: ctx.num_registers
        }
    }
}

impl ST::FunctionPrototype {
    fn from(function: &PT::FunctionNode, id: usize) -> ST::FunctionPrototype {
        ST::FunctionPrototype{
            id,
            borrow_params: function.borrow_params.clone(),
            steal_params: function.steal_params.clone(),
            return_params: function.return_params.clone()
        }
    }
}

impl ST::StatementNode {
    fn from(node: PT::StatementNode, ctx: &mut SyntaxContext) -> ST::StatementNode {
        macro_rules! passthrough {
            ($( $x:ident ),*) => {
                match node {
                    $(
                        PT::StatementNode::$x(valbox) => 
                        ST::StatementNode::$x(Box::new(ST::$x::from(*valbox, ctx)))
                    ,)*
        }   }   }
        passthrough! {
            LetUnletNode, RefUnrefNode, ModopNode, IfNode, CatchNode,
            CallChainNode
        }
    }
}

pub fn check_syntax(module: PT::Module) -> ST::Module{
    let mut func_prototypes = HashMap::new();
    
    for f in module.functions.iter() {
        if func_prototypes.insert(
            f.name.clone(),
            ST::FunctionPrototype::from(&f, func_prototypes.len())
        ).is_some() {
            panic!("Duplicate function definition");
        }
    }

    let mut main_idx = None;
    let mut functions = Vec::with_capacity(module.functions.len());

    for (i, f) in module.functions.into_iter().enumerate() {
        if f.name == "main" {main_idx = Some(i)}
        functions.push(ST::FunctionNode::from(f, &func_prototypes));
    }

    ST::Module{functions, main_idx}
}
