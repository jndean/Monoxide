
use std::collections::HashSet;
use std::cell::RefCell;

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
    func_names: &'a Vec<String>,
    free_registers: Vec<usize>,
    local_variables: HashMap<String, Reference>,
    num_registers: u16
}


impl<'a> SyntaxContext<'a> {
    pub fn new(func_names: &Vec<String>) -> SyntaxContext {
        SyntaxContext{
            func_names,
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

    pub fn create_ref(&mut self, name: &str, lookup: &PT::LookupNode) -> usize {
        if self.local_variables.contains_key(name) {
            panic!("Initialising a reference that already exists");
        };
        
        let (is_interior, mut register, var) = match self.local_variables.get(lookup.name) {
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
                match self.local_variables.get(lookup.name) {
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


impl PT::FractionNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::FractionNode {
        ST::FractionNode{value: self.value}
    }
}

impl PT::BinopNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::BinopNode {
        ST::BinopNode{
            lhs: self.lhs.check(ctx),
            rhs: self.rhs.check(ctx),
            op: self.op
        }
    }
}

impl PT::ArrayLiteralNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::ArrayLiteralNode {
        ST::ArrayLiteralNode{
            items: self.items.map(|i| i.check(ctx)).collect()
        }
    }
}

impl PT::LookupNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::LookupNode {
        ST::ArrayLiteralNode{
            register: ctx.lookup_local(self.name),
            indices: self.indices.map(|i| i.check(ctx)).collect()
        }
    }
}

impl PT::ExpressionNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::ExpressionNode {
        match self {
            PT::ExpressionNode::Fraction(valbox) => 
                ST::ExpressionNode::Fraction{Box::new(valbox.check(ctx))},
            PT::ExpressionNode::Binop(valbox) => 
                ST::ExpressionNode::Binop{Box::new(valbox.check(ctx))},
            PT::ExpressionNode::ArrayLiteral(valbox) => 
                ST::ExpressionNode::ArrayLiteral{Box::new(valbox.check(ctx))},
            PT::ExpressionNode::Lookup(valbox) => 
                ST::ExpressionNode::Lookup{Box::new(valbox.check(ctx))},
        }
    }
}


impl PT::LetUnletNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::LetUnletNode {
        ST::LetUnletNode{
            is_unlet: self.is_unlet,
            register: if self.is_unlet {ctx.remove_variable(self.name)}
                      else             {ctx.create_variable(self.name)},
            rhs: self.rhs.check(ctx)
        }
    }
}

impl PT::RefUnrefNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::RefUnrefNode {
        let rhs = self.rhs.check(ctx);
        let register = if self.is_unref {ctx.remove_ref(self.name, &rhs)}
                       else             {ctx.create_ref(self.name, &rhs)};
        ST::RefUnrefNode{is_unref: self.is_unref, register, rhs}
    }
}

impl PT::ModopNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::ModopNode {
        ST::ModopNode{
            lookup: self.rhs.check(ctx),
            op: self.op,
            rhs: self.rhs.check(ctx)
        }
    }
}

impl PT::IfNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::IfNode {
        let fwd_expr = self.fwd_expr.check(ctx);
        let bkwd_expr = self.bkwd_expr.check(ctx);
        let if_stmts = self.if_stmts.map(|s| s.check(ctx)).collect();
        let else_stmts = self.else_stmts.map(|s| s.check(ctx)).collect();
        ST::IfNode{fwd_expr, if_stmts, else_stmts, bkwd_stmts}
    }
}

impl PT::CatchNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::CatchNode {
        ST::CatchNode{expr: self.expr.check(ctx)}
    }
}

impl PT::FunctionNode {
    fn check(&self, func_names: &'a Vec<String>) -> ST::FunctionNode {
        let ctx = SyntaxContext::new(func_names);
        ST::FunctionNode{
            name: self.name,
            borrow_params: self.borrow_params,
            steal_params: self.steal_params,
            return_params: self.return_params,
            stmts: self.stmts.map(|s| s.check(ctx)).collect())
    }
}

impl PT::Module {
    fn check(&self) -> ST::Module {
        let func_names = self.functions.map(|f| f.name).collect();
        ST::Module{
            functions: self.functions.map(|f| f.check(&func_names)).collect()
        }
    }
}
/*
impl PT::StatementNode {
    fn check(&self, ctx: &mut SyntaxContext) -> ST::StatementNode {
        match self {
            PT::StatementNode::LetUnlet(valbox) =>
                
        }
    }
}
*/