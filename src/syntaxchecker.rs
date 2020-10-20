
use std::collections::{HashSet, HashMap};
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::mem;
use std::rc::Rc;

use num_traits::identities::Zero;

use crate::interpreter;
use crate::parsetree as PT;
use crate::syntaxtree as ST;

use PT::Expression as PTExpression;



#[derive(Debug)]
pub struct Variable {
    id: isize,
    exteriors: RefCell<HashSet<String>>,
    interiors: RefCell<HashSet<String>>,
}

impl Hash for Variable{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Variable {}


#[derive(Debug)]
pub struct Reference {
    is_interior: bool,
    is_borrowed: bool,
    is_global: bool,
    register: usize,
    var: Rc<Variable>
}

#[derive(Debug)]
pub struct SyntaxError {
    pub line: usize,
    pub col: usize,
    pub desc: String
}


#[derive(Debug)]
pub struct SyntaxContext<'a> {
    functions: &'a HashMap<String, ST::FunctionPrototype>,
    consts: Vec<interpreter::Variable>,
    strings: Vec<String>,
    free_registers: Vec<usize>,
    locals: HashMap<String, Reference>,
    locals_stack: Vec<HashMap<String, Reference>>,
    globals: &'a HashMap<String, Reference>,
    num_registers: usize,
    last_var_id: isize
}


impl<'a> SyntaxContext<'a> {
    pub fn new(
        functions: &'a HashMap<String, ST::FunctionPrototype>,
        globals: &'a HashMap<String, Reference>
    ) -> SyntaxContext<'a> {
        SyntaxContext {
            functions,
            consts: Vec::new(),
            strings: Vec::new(),
            free_registers: Vec::new(),
            locals: HashMap::new(),
            locals_stack: Vec::new(),
            globals,
            num_registers: 0,
            last_var_id: 0
        }
    }

    pub fn new_variable_id(&mut self) -> isize {
        self.last_var_id += 1;
        self.last_var_id
    }

    pub fn new_variable(&mut self, name: String, register: usize, is_borrowed: bool) -> Reference {
        let mut exteriors = HashSet::new();
        exteriors.insert(name);
        Reference {
            is_interior: false,
            register,
            is_borrowed,
            is_global: false,
            var: Rc::new(Variable{
                id: self.new_variable_id(),
                exteriors: RefCell::new(exteriors),
                interiors: RefCell::new(HashSet::new())
            })
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
            if !owned_links.insert(link) {
                panic!("Duplicate owned links")
            };
        }

        let mut linked: HashMap<String, Rc<Variable>> = HashMap::new();

        // Init borrowed params //
        let mut borrow_registers = Vec::with_capacity(borrows.len());
        let mut steal_registers = Vec::with_capacity(steals.len());
        for (params, registers) in vec![(borrows, &mut borrow_registers), (steals, &mut steal_registers)] {
            for p in params {
                if self.locals.contains_key(&p.name) {
                    panic!("Duplicate function parameter names");
                };
                let register = self.get_free_register();
                registers.push(register);

                if !p.is_ref {
                    // Singly owned //
                    let new_var = self.new_variable(p.name.clone(), register, true);
                    self.locals.insert(p.name, new_var);

                } else if let Some(link) = p.link {
                    let is_interior = is_interior_link(&link);
                    let ext_link = exterior_link_name(&link);
                    match linked.get(&ext_link) {
                        Some(var) => {
                            // Existing link name //
                            if is_interior {var.interiors.borrow_mut().insert(p.name.clone())}
                            else           {var.exteriors.borrow_mut().insert(p.name.clone())};
                            self.locals.insert(
                                p.name,
                                Reference{is_interior, register, is_borrowed: true, is_global: false, var: Rc::clone(var)}
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
                                id: self.new_variable_id(),
                                exteriors: RefCell::new(exteriors),
                                interiors: RefCell::new(interiors)
                            });
                            linked.insert(ext_link, Rc::clone(&var));
                            self.locals.insert(
                                p.name,
                                Reference{is_interior, register, is_borrowed: true, is_global: false, var}
                            );
                        }
                    }

                } else {
                    // Unbound ref //
                    let varref = self.new_variable(p.name.clone(), register, true);
                    varref.var.interiors.borrow_mut().insert(String::from("calling scope"));
                    self.locals.insert(p.name, varref);
                }
            }
        }

        // TODO: Still need to check all the owned link groups have an exterior ref //

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
            let reference = self.locals.get(&p.name).expect(
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

    fn lookup_function_prototype(&self, name: &str) -> &ST::FunctionPrototype {
        self.functions.get(name).expect("Undefined function")
    }

    fn check_singly_owned(&self, name: &str) -> bool {
        let var = &self.lookup_variable(name).var;
        
        var.interiors.borrow().len() == 0 && var.exteriors.borrow().len() == 1
    }

    fn lookup_variable(&self, name: &str) -> &Reference {
        if let Some(var) = self.locals.get(name) { return var; }
        for locals in self.locals_stack.iter().rev() {
            if let Some(var) = locals.get(name) { return var; }
        }
        if let Some(var) = self.globals.get(name) { return var; }
        panic!(format!("Looking up non-existant variable \"{}\"", name));
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
        if self.locals.contains_key(name) {
            panic!("Initialising a variable that already exists");
        };
        let register = self.get_free_register();
        let new_var = self.new_variable(name.to_string(), register, false);
        self.locals.insert(name.to_string(), new_var);
        register
    }

    pub fn create_ref(&mut self, name: &str, lookup: &PT::LookupNode) -> usize {
        if self.locals.contains_key(name) {
            panic!("Initialising a reference that already exists");
        };

        let src = self.lookup_variable(&lookup.name);
        
        let is_interior = src.is_interior || lookup.indices.len() > 0;
        let var = Rc::clone(&src.var);
        let is_borrowed = false;
        let is_global = false;

        let register = if is_interior || src.is_global {
            self.get_free_register()
        } else {
            src.register
        };

        if is_interior {
            var.interiors.borrow_mut().insert(name.to_string());
        } else {
            var.exteriors.borrow_mut().insert(name.to_string());
        }

        self.locals.insert(
            name.to_string(),
            Reference{is_interior, register, var, is_borrowed, is_global}
        );
        register
    }


    pub fn remove_ref(&mut self, name: &str, lookup: &PT::LookupNode) -> usize {

        match self.locals.remove(name) {
            None => panic!("Removing non-existant reference"),
            Some(Reference{is_borrowed: true, ..}) => panic!("Removing borrowed reference"),
            Some(Reference{is_interior, register, var, ..}) => {
                let is_interior = is_interior || lookup.indices.len() > 0;

                // Check they reference the same variable, and they're not deref'ing an exterior using an interior
                let Reference{var: other_var, is_interior: other_is_interior, ..} = self.lookup_variable(&lookup.name);
                if !Rc::ptr_eq(&var, other_var) || (*other_is_interior && !is_interior) {
                    panic!("Unreferencing using incorrect variable");
                }

                // Deref
                var.interiors.borrow_mut().remove(name);
                var.exteriors.borrow_mut().remove(name);
                register
            }
        }
    }

    fn remove_variable(&mut self, name: &str) -> usize {
        match self.locals.remove(name) {
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

    fn check_ref_is_resizable(&self, name: &str) -> bool {
        let varref = self.lookup_variable(name);
        let num_interiors = varref.var.interiors.borrow().len();
        num_interiors == 0 || (num_interiors == 1 && varref.is_interior)
    }

    fn get_var_id(&self, name: &str) -> isize {
        self.lookup_variable(name).var.id
    }

    fn enter_block(&mut self) {
        let locals = HashMap::new();
        self.locals_stack.push(mem::replace(&mut self.locals, locals));
    }

    fn exit_block(&mut self) {
        if self.locals.len() > 0 {
            panic!("Leaving block with dangling variable references");
        }
        self.exit_block_nocheck();
    }

    fn exit_block_nocheck(&mut self) {
        mem::replace(&mut self.locals, self.locals_stack.pop().expect("Failed to pop from locals_stack"));
    }
}


// ---------------------------- Expression Nodes ---------------------------- //

impl PT::Expression for PT::FractionNode {
    fn get_src_pos(&self) -> (usize, usize) { (self.line, self.col) }

    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Expression>, SyntaxError> {
        let const_idx = ctx.add_const(
            interpreter::Variable::Frac(self.value)
        );
        Ok(Box::new(ST::FractionNode{const_idx, used_vars: HashSet::new()}))
    }
}

impl PT::Expression for PT::StringNode {
    fn get_src_pos(&self) -> (usize, usize) { (self.line, self.col) }

    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Expression>, SyntaxError> {
        let const_idx = ctx.add_const(
            interpreter::Variable::Str(self.value)
        );
        Ok(Box::new(ST::StringNode{const_idx, used_vars: HashSet::new()}))
    }
}

impl PT::Expression for PT::BinopNode {
    fn get_src_pos(&self) -> (usize, usize) { self.lhs.get_src_pos() }

    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Expression>, SyntaxError> {
        let lhs = self.lhs.to_syntax_node(ctx)?;
        let rhs = self.rhs.to_syntax_node(ctx)?;
        let is_mono = lhs.is_mono() || rhs.is_mono();
        let used_vars = lhs.used_vars().iter()
                        .chain(rhs.used_vars().iter())
                        .cloned().collect();
        Ok(Box::new(ST::BinopNode{lhs, rhs, is_mono, used_vars, op: self.op}))
    }
}

impl PT::Expression for PT::UniopNode {
    fn get_src_pos(&self) -> (usize, usize) { (self.line, self.col) }

    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Expression>, SyntaxError> {
        let expr = self.expr.to_syntax_node(ctx)?;
        let is_mono = expr.is_mono();
        let used_vars = expr.used_vars().clone();
        Ok(Box::new(ST::UniopNode{expr, is_mono, used_vars, op: self.op}))
    }
}

impl PT::Expression for PT::ArrayLiteralNode {
    fn get_src_pos(&self) -> (usize, usize) { (self.line, self.col) }

    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Expression>, SyntaxError> {
        let items = self.items.into_iter()
                              .map(|i| i.to_syntax_node(ctx))
                              .collect::<Result<Vec<_>, _>>()?;
        let is_mono = items.iter().any(|x| x.is_mono());
        let used_vars = items.iter().map(|x| x.used_vars())
                                    .flat_map(|it| it.clone())
                                    .collect();
        Ok(Box::new(ST::ArrayLiteralNode{items, used_vars, is_mono}))
    }
}

impl PT::Expression for PT::ArrayRepeatNode {
    fn get_src_pos(&self) -> (usize, usize) { (self.line, self.col) }

    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Expression>, SyntaxError> {
        let item = self.item.to_syntax_node(ctx)?;
        let dimensions = self.dimensions.to_syntax_node(ctx)?;
        let is_mono = item.is_mono() || dimensions.is_mono();
        let mut used_vars = item.used_vars().clone();
        used_vars.extend(dimensions.used_vars());

        Ok(Box::new(ST::ArrayRepeatNode{item, dimensions, used_vars, is_mono}))
    }
}

impl PT::Expression for PT::LookupNode {
    fn get_src_pos(&self) -> (usize, usize) { (self.line, self.col) }

    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Expression>, SyntaxError> {
        Ok(Box::new(self.to_syntax_node_unboxed(ctx)?))
    }
}
impl PT::LookupNode {
    fn to_syntax_node_unboxed(self, ctx: &mut SyntaxContext) -> Result<ST::LookupNode, SyntaxError> {
        let var = ctx.lookup_variable(&self.name);
        let register = var.register;
        let is_global = var.is_global;
        let indices = self.indices.into_iter()
                                  .map(|i| i.to_syntax_node(ctx))
                                  .collect::<Result<Vec<_>, _>>()?;
        let var_is_mono = self.name.starts_with(".");
        let is_mono = var_is_mono || indices.iter().any(|x| x.is_mono());
        let mut used_vars = indices.iter().map(|x| x.used_vars())
                                          .flat_map(|it| it.clone())
                                          .collect::<HashSet<_>>();
        used_vars.insert(ctx.get_var_id(&self.name));
        Ok(ST::LookupNode{register, is_global, indices, used_vars, is_mono, var_is_mono})
    }
}


// ---------------------------- Statement Nodes ---------------------------- //


impl PT::Statement for PT::PrintNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {
        let items: Result<Vec<_>, _> = self.items.into_iter()
                                                 .map(|i| i.to_syntax_node(ctx))
                                                 .collect();
        let items = items?;
        let newline = self.newline;
        let is_mono = items.iter().any(|i| i.is_mono());

        Ok(Box::new(ST::PrintNode{items, newline, is_mono}))
    }
}

impl PT::Statement for PT::LetUnletNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {
        let is_unlet = self.is_unlet;
        let register = if self.is_unlet {ctx.remove_variable(&self.name)}
                       else             {ctx.create_variable(&self.name)};
        let rhs = self.rhs.to_syntax_node(ctx)?;
        let is_mono = self.name.starts_with(".");

        if !is_mono && rhs.is_mono() {
            let verb = if is_unlet {"Uninitialising"} else {"Initialising"};
            return Err(SyntaxError{
                line: self.line, col: self.col,
                desc: format!("{} variable \"{}\" using mono information", verb, self.name)
            });
        }

        Ok(Box::new(ST::LetUnletNode{is_unlet, register, rhs, is_mono}))
    }
}

impl PT::Statement for PT::RefUnrefNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {
        let is_unref = self.is_unref;
        let register = if self.is_unref {ctx.remove_ref(&self.name, &self.rhs)}
                       else             {ctx.create_ref(&self.name, &self.rhs)};
        let rhs = self.rhs.to_syntax_node_unboxed(ctx)?;
        let is_mono = self.name.starts_with(".");

        let problem = if      is_mono != rhs.is_mono     {Some("RHS")}
                      else if is_mono != rhs.var_is_mono {Some("RHS variable")}
                      else                               {None};

        if let Some(problem) = problem {
            return Err(SyntaxError{
                line: self.line, col: self.col,
                desc: format!("Reference \"{}\" has different mono-ness to {}", self.name, problem)
            });
        }

        Ok(Box::new(ST::RefUnrefNode{is_unref, register, rhs, is_mono}))
    }
}

impl PT::Statement for PT::ModopNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {
        let (line, col) = self.lookup.get_src_pos();
        let varname = self.lookup.name.clone();
        let lookup = self.lookup.to_syntax_node_unboxed(ctx)?;
        let rhs = self.rhs.to_syntax_node(ctx)?;
        let is_mono = lookup.var_is_mono;

        if !is_mono && (lookup.is_mono || rhs.is_mono()) {
            return Err(SyntaxError{line, col,
                desc: format!("Modifying variable \"{}\" using mono information", varname)
            })
        }

        Ok(Box::new(ST::ModopNode{lookup, rhs, is_mono, op: self.op}))
    }
}

impl PT::Statement for PT::PushPullNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {

        // The lookup may have no other interior references
        // (it may be an interior reference itself)
        assert!(
            ctx.check_ref_is_resizable(&self.lookup.name),
            "Resizing {} when other references to its interior exist", self.lookup.name
        );

        let register = if self.is_push {ctx.remove_variable(&self.name)}
                       else            {ctx.create_variable(&self.name)};
        let lookup = self.lookup.to_syntax_node_unboxed(ctx)?;
        let is_mono = self.name.starts_with(".");

        assert!(is_mono == lookup.var_is_mono,
            "Can only push to / pull from a variable of matching mono-ness");
        assert!(is_mono == lookup.is_mono,
                "Mono information used to push/pull non-mono variable \"{}\"", self.name);

        Ok(Box::new(ST::PushPullNode{register, lookup, is_mono, is_push: self.is_push}))
    }
}

impl PT::Statement for PT::IfNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {
        let fwd_expr = self.fwd_expr.to_syntax_node(ctx)?;
        ctx.enter_block();
        let if_stmts = self.if_stmts.into_iter()
                                    .map(|s| s.to_syntax_node(ctx))
                                    .collect::<Result<Vec<_>, _>>()?;
        ctx.exit_block();
        ctx.enter_block();
        let else_stmts = self.else_stmts.into_iter()
                                    .map(|s| s.to_syntax_node(ctx))
                                    .collect::<Result<Vec<_>, _>>()?;
        ctx.exit_block();
        let bkwd_expr = self.bkwd_expr.to_syntax_node(ctx)?;
        let is_mono = fwd_expr.is_mono();

        let all_mono_stmts = if_stmts.iter().chain(else_stmts.iter()).all(|s| s.is_mono());
        assert!(!is_mono || all_mono_stmts, "Non-mono statement in mono if-statement");
        assert!(!bkwd_expr.is_mono(), "Backward condition in if statement is mono");

        Ok(Box::new(ST::IfNode{fwd_expr, if_stmts, else_stmts, bkwd_expr, is_mono}))
    }
}

impl PT::Statement for PT::WhileNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {
        let fwd_expr = self.fwd_expr.to_syntax_node(ctx)?;
        ctx.enter_block();
        let stmts = self.stmts.into_iter()
                              .map(|s| s.to_syntax_node(ctx))
                              .collect::<Result<Vec<_>, _>>()?;
        ctx.exit_block();
        let bkwd_expr = match self.bkwd_expr {
            Some(expr) => Some(expr.to_syntax_node(ctx)?),
            None => None
        };
        
        let is_mono = fwd_expr.is_mono();
        let all_mono_stmts = stmts.iter().all(|s| s.is_mono());
        assert!(!is_mono || all_mono_stmts, "Non-mono statement in mono while loop");
        if let Some(expr) = &bkwd_expr {
            assert!(!expr.is_mono(), "Backward condition in while loop is mono");
        }

        Ok(Box::new(ST::WhileNode{fwd_expr, stmts, bkwd_expr, is_mono}))
    }
}

impl PT::Statement for PT::ForNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {

        let mut zero_lookup = self.iterator.clone();
        zero_lookup.indices.push(Box::new(PT::FractionNode{
            value: interpreter::Fraction::zero(),
            line: 0, col: 0
        }));
        
        let register = ctx.create_ref(&self.iter_var, &zero_lookup);
        let iterator = self.iterator.to_syntax_node_unboxed(ctx)?;
        ctx.enter_block();
        let stmts = self.stmts.into_iter()
                              .map(|s| s.to_syntax_node(ctx))
                              .collect::<Result<Vec<_>, _>>()?;
        ctx.exit_block();
        let is_mono = self.iter_var.starts_with(".");

        ctx.remove_ref(&self.iter_var, &zero_lookup);
        
        if is_mono {
            assert!(iterator.var_is_mono, "Mono for loop iterating over non-mono iterator");
            assert!(stmts.iter().all(|s| s.is_mono()), "Non-mono statement in mono for loop");
        } else {
            assert!(!iterator.is_mono, "Assigning to non-mono iteration variable using mono information");
        }
        /* 
        TODO: disallow modification of iterator indices in for-loop body e.g. 
            for (_ in array[i]) {
                i += 1;
            };
        is not invertible

        TODO: Bug. It seems the itertation var is not properly linked as a ref to the iterator... can do
            for (x in X) {
                X[0] -= x;
            };
        */

        Ok(Box::new(ST::ForNode{register, iterator, stmts, is_mono}))
    }
}

impl PT::Statement for PT::DoYieldNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {

        ctx.enter_block();
        let do_stmts = self.do_stmts.into_iter()
                                    .map(|s| s.to_syntax_node(ctx))
                                    .collect::<Result<Vec<_>, _>>()?;
        ctx.enter_block();
        let yield_stmts = self.yield_stmts.into_iter()
                                          .map(|s| s.to_syntax_node(ctx))
                                          .collect::<Result<Vec<_>, _>>()?;
        ctx.exit_block();
        ctx.exit_block_nocheck();  // The undo WILL free locals properly

        Ok(Box::new(ST::DoYieldNode{do_stmts, yield_stmts}))
    }
}

impl PT::Statement for PT::CatchNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {
        Ok(Box::new(ST::CatchNode{expr: self.expr.to_syntax_node(ctx)?}))
    }
}


impl PT::Statement for PT::CallNode {
    fn to_syntax_node(self: Box<Self>, ctx: &mut SyntaxContext) -> Result<Box<dyn ST::Statement>, SyntaxError> {

        /* 
        TODO:
            ✓ Check singly owned params are singly owned
            ✓ Check owned groups have exterior ref
            ✓ Check two inputs of the same var share a link
            ✓ Check interiors aren't passed as exteriors
            - Check owned link groups take all refs to the var
            - Check not stealing borrowed refs
            - Check linked params share a var
        */

        let proto = ctx.lookup_function_prototype(&self.name);
        let func_idx = proto.id;
        let mut used_links: HashMap<Rc<Variable>, Option<String>> = HashMap::new();
        let mut used_vars: HashMap<String, Rc<Variable>> = HashMap::new();

        for (param, proto_link) in self.borrow_args.iter().zip(proto.borrow_params.iter()) {

            let var = &ctx.lookup_variable(&param.name).var;
            let link = proto_link.clone().map(|pl| pl.link).flatten();
            if let Some(other_link) = used_links.get(var) {
                if link != *other_link {
                    panic!("Passing incorrectly linked references")
            }};
            used_links.insert(Rc::clone(var), link.clone());
            if let Some(link) = &link {
                if let Some(other_var) = used_vars.get(link) {
                    if *var != *other_var {
                        panic!("Passing incorrectly linked references");
                    }
                }
                used_vars.insert(link.clone(), Rc::clone(var));
                // done here?
            };


            match proto_link {
                Some(proto_link) => {
                    if !proto_link.is_interior && ctx.lookup_variable(&param.name).is_interior {
                        panic!("Passing interior to function marked as exterior")
                    }
                },
                None => {
                    if !ctx.check_singly_owned(&param.name) {
                        panic!("Call uses non-singly owned variable");
                    }

                }
            }
        }

        let mut stolen_args = Vec::with_capacity(self.stolen_args.len());
        for arg in self.stolen_args.into_iter() {
            stolen_args.push(ctx.lookup_variable(&arg).register);
            ctx.locals.remove(&arg);
        }
        let borrow_args = self.borrow_args.into_iter()
                                          .map(|a| a.to_syntax_node_unboxed(ctx))
                                          .collect::<Result<Vec<_>, _>>()?;
        let mut return_args = Vec::with_capacity(self.return_args.len());
        for arg in self.return_args.into_iter() {
            return_args.push(ctx.create_variable(&arg));
            // TODO: Using create variable is WRONG
        }
        // TODO: Get is_mono from function prototype
        let is_mono = false;

        Ok(Box::new(ST::CallNode{
            is_uncall: self.is_uncall,
            func_idx, borrow_args, stolen_args, return_args, is_mono
        }))
    }
}

impl PT::FunctionNode {
    fn to_syntax_node(
        self,
        func_lookup: &HashMap<String, ST::FunctionPrototype>,
        global_vars: &HashMap<String, Reference>,
    ) -> Result<ST::FunctionNode, SyntaxError> {
        let (syntax_node, _) = self.to_syntax_node_and_locals(func_lookup, global_vars)?;
        Ok(syntax_node)
    }

    fn to_syntax_node_and_locals(
        self,
        func_lookup: &HashMap<String, ST::FunctionPrototype>,
        global_vars: &HashMap<String, Reference>
    ) -> Result<
        (ST::FunctionNode, HashMap<String, Reference>),
        SyntaxError
    > {

        let mut ctx = SyntaxContext::new(func_lookup, global_vars);
        let (link_set, borrow_registers, steal_registers) = ctx.init_func(
            self.owned_links, self.borrow_params, self.steal_params);
        let stmts = self.stmts.into_iter()
                              .map(|s| s.to_syntax_node(&mut ctx))
                              .collect::<Result<Vec<_>, _>>()?;
        let return_registers = ctx.end_func(link_set, self.return_params);

        let function_node = ST::FunctionNode{
            stmts, borrow_registers, steal_registers, return_registers,
            consts: ctx.consts,
            num_registers: ctx.num_registers
        };

        Ok((function_node, ctx.locals))
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
                let mut param_link = param.link.clone().map(|link| {
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
                }).flatten();
                if param_link.is_none() && param.is_ref {
                    param_link = Some(ST::ParamLink{
                        is_interior: true, link: None, linked_borrow: None, linked_io: None
                    });
                }
                out_vec.push(param_link);
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

        let owned_link_groups = owned_link_groups.into_iter().map(|(_, v)| v)
                                                 .collect::<Vec<[Vec<usize>; 3]>>();

        // Check all owned link groups have an exterior ref //
        'group_iter: for link_group in &owned_link_groups {
            for i in &link_group[0] {
                if let Some(paramlink) = &borrow_params[*i] {
                    if !paramlink.is_interior {
                        continue 'group_iter;
            }   }   }
            panic!("Owned link group without borowed exterior ref");
        }

        ST::FunctionPrototype{
            id, borrow_params, steal_params, return_params, owned_link_groups
        }
    }
}

pub fn check_syntax(module: PT::Module) -> Result<ST::Module, SyntaxError> {
    // Collect the properties of all the module functions
    let mut func_prototypes = HashMap::new();
    for f in module.functions.iter() {
        if func_prototypes.insert(
            f.name.clone(),
            ST::FunctionPrototype::from(&f, func_prototypes.len())
        ).is_some() {
            panic!("Duplicate function definition");
        }
    }

    // Check the syntax of the global scope pseudo function, and convert the variable into globals
    let (global_func, mut global_refs) 
        = module.global_func.to_syntax_node_and_locals(&func_prototypes, &HashMap::new())?;
    let mut global_vars: HashMap<isize, Rc<Variable>> = HashMap::new();
    for (_, reference) in global_refs.iter_mut() {
        reference.is_global = true;
        reference.is_borrowed = true;
        reference.var = match global_vars.get(&reference.var.id) {
            Some(var) => Rc::clone(var),
            None => {
                let var = Rc::new(Variable{
                    id: -reference.var.id,  // Negative id for globals
                    interiors: RefCell::new(reference.var.interiors.borrow().clone()),
                    exteriors: RefCell::new(reference.var.exteriors.borrow().clone())
                });
                global_vars.insert(reference.var.id, Rc::clone(&var));
                var
            }
        }
    }
    drop(global_vars);

    

    // Check the syntax of each function, and find the main function
    let mut main_idx = None;
    let mut functions = Vec::with_capacity(module.functions.len());
    for (i, f) in module.functions.into_iter().enumerate() {
        if f.name == "main" {main_idx = Some(i)}
        functions.push(f.to_syntax_node(&func_prototypes, &global_refs)?);
    }

    Ok(ST::Module{functions, main_idx, global_func})
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