
use std::collections::{HashSet, HashMap};
use std::cell::RefCell;
use std::iter::FromIterator;
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
    is_borrowed: bool,
    register: usize,
    var: Rc<Variable>
}

impl Reference {
    fn new(name: String, register: usize, is_borrowed: bool) -> Reference {
        let mut exteriors = HashSet::new();
        exteriors.insert(name);
        Reference {
            is_interior: false,
            register,
            is_borrowed,
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

    fn init_func(
        &mut self,
        owned_links_raw: Vec<String>, 
        borrows: Vec<PT::FunctionParam>,
        steals: Vec<PT::FunctionParam>
    ) -> (HashMap<String, Rc<Variable>>, Vec<usize>, Vec<usize>) {

        // Check links //
        let mut owned_links = HashSet::new();
        for link in owned_links_raw {
            let link = exterior_link_name(&link);
            if owned_links.insert(link) { 
                panic!("Duplicate owned links")
            };
        }
        
        let mut linked: HashMap<String, Rc<Variable>> = HashMap::new();

        // Init borrowed params //
        let mut borrow_registers = Vec::with_capacity(borrows.len());
        let mut steal_registers = Vec::with_capacity(steals.len());
        for (params, registers) in vec![(borrows, &mut borrow_registers), (steals, &mut steal_registers)] {
            for p in params {    
                if self.local_variables.contains_key(&p.name) {
                    panic!("Duplicate function parameter names");
                };        
                let register = self.get_free_register();
                registers.push(register);

                if !p.is_ref {
                    // Singly owned //
                    self.local_variables.insert(
                        p.name.clone(),
                        Reference::new(p.name, register, true)
                    );
                } else if let Some(link) = p.link {
                    let is_interior = is_interior_link(&link);
                    let ext_link = exterior_link_name(&link);
                    match linked.get(&ext_link) {
                        Some(var) => {
                            // Existing link name //
                            if is_interior {var.interiors.borrow_mut().insert(p.name.clone())}
                            else           {var.exteriors.borrow_mut().insert(p.name.clone())};
                            self.local_variables.insert(
                                p.name,
                                Reference{is_interior, register, is_borrowed: true, var: Rc::clone(var)}
                            );
                        },
                        None => {
                            let (mut interiors, mut exteriors) = (HashSet::new(), HashSet::new());
                            if is_interior {interiors.insert(p.name.clone())}
                            else           {exteriors.insert(p.name.clone())};
                            if !owned_links.contains(&ext_link) {
                                // Unowned link group, insert a dummy interior link to prevent reshapes //
                                interiors.insert(String::from("caller anchor"));
                            }
                            let var = Rc::new(Variable{
                                exteriors: RefCell::new(exteriors),
                                interiors: RefCell::new(interiors),
                            });
                            linked.insert(ext_link, Rc::clone(&var));
                            self.local_variables.insert(
                                p.name,
                                Reference{is_interior, register, is_borrowed: true, var}
                            );
                        }
                    }

                } else {
                    // Unbound ref //
                    let varref = Reference::new(p.name.clone(), register, true);
                    varref.var.interiors.borrow_mut().insert(String::from("calling scope"));
                    self.local_variables.insert(p.name, varref); 
                }
            }
        }

        // Still need to check all the owned link groups have an exterior ref //

        (linked, borrow_registers, steal_registers)
    }

    fn end_func(
        &mut self,
        input_links: HashMap<String, Rc<Variable>>,
        returns: Vec<PT::FunctionParam>
    ) -> Vec<usize> {
        // Check the links to input variables are valid //
        let mut return_registers = Vec::with_capacity(returns.len());

        for p in returns {
            let reference = self.local_variables.get(&p.name).expect(
                "Returning non-existant variable");
            return_registers.push(reference.register);

            if let Some(link) = p.link {
                let ext_link = exterior_link_name(&link);
                if let Some(linked_var) = input_links.get(&ext_link) {
                    if !Rc::ptr_eq(&reference.var, linked_var) {
                        panic!("Wrong reference link group on returned variable");
                    }
                }
            }
        }

        return_registers
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
            Some(Reference{is_interior, register, var, ..}) => {
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
            Reference{is_interior, register, var, is_borrowed: false}
        );
        register
    }


    pub fn remove_ref(&mut self, name: &str, lookup: &PT::LookupNode) -> usize {

        match self.local_variables.remove(name) {
            None => panic!("Removing non-existant reference"),
            Some(Reference{is_borrowed: true, ..}) => panic!("Removing borrowed reference"),
            Some(Reference{is_interior, register, var, ..}) => {
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
            Some(Reference{is_borrowed: true, ..}) => panic!("Uninitialising borrowed variable"),
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
        let (link_set, borrow_registers, steal_registers) = ctx.init_func(
            node.owned_links, node.borrow_params, node.steal_params);
        let stmts = node.stmts.into_iter()
                              .map(|s| ST::StatementNode::from(s, &mut ctx))
                              .collect();
        let return_registers = ctx.end_func(link_set, node.return_params);

        ST::FunctionNode{
            stmts, borrow_registers, steal_registers, return_registers,
            consts: ctx.consts,
            num_registers: ctx.num_registers
        }
    }
}

impl ST::FunctionPrototype {
    fn from(function: &PT::FunctionNode, id: usize) -> ST::FunctionPrototype {

        let mut linked_borrows = HashMap::new();
        let mut owned_link_groups = HashMap::new();
        for name in &function.owned_links {
            owned_link_groups.insert(
                name.clone(), [Vec::new(), Vec::new(), Vec::new()]);
        }

        fn process_params(
            params: &Vec<PT::FunctionParam>,
            linked_borrows: &mut HashMap<String, usize>,
            owned_link_groups: &mut HashMap<String, [Vec<usize>; 3]>,
            is_io: bool,
            link_group_type: usize,
        ) -> Vec<Option<ST::ParamLink>> {

            let mut out_vec = Vec::new();
            let mut self_links = HashMap::new();
            for (idx, param) in params.iter().enumerate() {
                out_vec.push(
                    param.link.clone().map(|link| {
                        let ext_name = exterior_link_name(&link);
                        let linked_borrow = linked_borrows.get(&ext_name).map(|x|*x);
                        if !is_io {linked_borrows.insert(ext_name.clone(), idx);};
                        let linked_io = if is_io {
                            let res = self_links.get(&ext_name).map(|x|*x);
                            self_links.insert(ext_name.clone(), idx);
                            res
                        } else {None};
                        if let Some(groups) = owned_link_groups.get_mut(&ext_name) {
                            groups[link_group_type].push(idx);
                        };

                        Some(ST::ParamLink {
                            is_interior: is_interior_link(&link),
                            link: Some(ext_name),
                            linked_borrow, linked_io
                        })
                    }).flatten()
                );
            };
            out_vec
        };

        let borrow_params = process_params(
            &function.borrow_params,
            &mut linked_borrows,
            &mut owned_link_groups,
            false, 0);

        let steal_params = process_params(
            &function.steal_params,
            &mut linked_borrows,
            &mut owned_link_groups,
            true, 1);
            
        let return_params = process_params(
            &function.return_params,
            &mut linked_borrows,
            &mut owned_link_groups,
            true, 2);

        let owned_link_groups = owned_link_groups.into_iter().map(|(_, v)| v).collect();

        ST::FunctionPrototype{
            id, borrow_params, steal_params, return_params, owned_link_groups
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

    println!("{:#?}", func_prototypes);
    panic!("here");

    let mut main_idx = None;
    let mut functions = Vec::with_capacity(module.functions.len());

    for (i, f) in module.functions.into_iter().enumerate() {
        if f.name == "main" {main_idx = Some(i)}
        functions.push(ST::FunctionNode::from(f, &func_prototypes));
    }

    ST::Module{functions, main_idx}
}


fn exterior_link_name(link_name: &str) -> String {
    let mut c = link_name.chars();
    match c.next() {
        None => panic!("Empty link name?"),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn is_interior_link(link_name: &String) -> bool {
    char::is_lowercase(link_name.chars().next().expect("Empty link name?"))
}