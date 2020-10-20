use std::cmp;
use std::collections::HashMap;
use std::str::FromStr;

use crate::tokeniser::Token;
use crate::parsetree::{
    StatementNode, ExpressionNode, LookupNode, LetUnletNode,
    FractionNode, BinopNode, IfNode, ModopNode, FunctionNode,
    CatchNode, ArrayLiteralNode, Module, RefUnrefNode, CallNode,
    FunctionParam, PushPullNode, UniopNode, WhileNode, ForNode,
    PrintNode, StringNode, DoYieldNode, ArrayRepeatNode
};
use crate::interpreter::{Fraction, Instruction};


pub struct Parser {
    tokens: Vec<Token>,
    token_pos: usize,
    max_token_pos: usize,
    memo: HashMap<(usize, String), (usize, Parsed)>
}

#[derive(Debug)]
pub struct ParseError {
    line: usize,
    col: usize
}

type VecStatementNode = Vec<StatementNode>;
#[derive(Clone)]
pub enum Parsed {
    String(Option<String>),
    Token(Option<Token>),
    VecStatementNode(Option<VecStatementNode>),
    StatementNode(Option<StatementNode>),
    ExpressionNode(Option<ExpressionNode>),
    Instruction(Option<Instruction>),
    LookupNode(Option<LookupNode>),
    PushPullNode(Option<PushPullNode>),
    IfNode(Option<IfNode>),
    DoYieldNode(Option<DoYieldNode>),
    CatchNode(Option<CatchNode>),
    CallNode(Option<CallNode>),
    FunctionNode(Option<FunctionNode>),
    FunctionParam(Option<FunctionParam>),
    ArrayLiteralNode(Option<ArrayLiteralNode>),
    ArrayRepeatNode(Option<ArrayRepeatNode>),
    Module(Option<Module>)
}


#[allow(unused_macros)]
macro_rules! rule {
    ($func_name:ident -> $ret_type:ident $body:block) => {
        fn $func_name(&mut self) -> Option<$ret_type> {
            fn raw_func(parser: &mut Parser) -> Option<$ret_type> $body;
            let pos = self.mark();
            let key = (pos, String::from(stringify!($func_name)));
            match self.memo.get(&key) {
                Some((end, result)) => {
                    let end = *end;
                    let result = (*result).clone();
                    if let Parsed::$ret_type(ret) = result {
                        self.reset(end);
                        return ret;
                    } else {unreachable!()}
                },
                None => {
                    let result = raw_func(self);
                    let new_pos = self.mark();
                    let memo = Parsed::$ret_type(result.clone());
                    self.memo.insert(key, (new_pos, memo));
                    result
                }
            }
        }
    }
}


macro_rules! parse {
    ($sel:ident ; $($tail:tt)*) => {
        let pos = $sel.mark();
        parse_recurse!($sel; $($tail)*);
        $sel.reset(pos);
    };
}

macro_rules! parse_recurse {
    ($sel:ident ; $res_name:ident : $rule_expr:expr, $($tail:tt)*) => {
        if let Some($res_name) = $rule_expr {
	        parse_recurse!($sel; $($tail)*);
        }
    };
    ($sel:ident ; ? $res_name:ident : $rule_expr:expr , $($tail:tt)*) => {
        let $res_name = $rule_expr;
        parse_recurse!($sel; $($tail)*);
    };
    ($sel:ident ; $string:tt, $($tail:tt)*) => {
        if $sel.expect_literal($string) {
	        parse_recurse!($sel; $($tail)*);
	    }
    };
    ($sel:ident ; $body:block) => {$body};
}


macro_rules! memoise {
    ($raw_func:ident as $out_func:ident -> $ret_type:ident) => {
        fn $out_func(&mut self) -> Option<$ret_type> {
            let pos = self.mark();
            let key = (pos, String::from(stringify!($raw_func)));
            match self.memo.get(&key) {
                Some((end, result)) => {
                    let end = *end;
                    let result = (*result).clone();
                    if let Parsed::$ret_type(ret) = result {
                        self.reset(end);
                        return ret;
                    } else {unreachable!()}
                },
                None => {
                    let result = self.$raw_func();
                    let new_pos = self.mark();
                    let memo = Parsed::$ret_type(result.clone());
                    self.memo.insert(key, (new_pos, memo));
                    result
                }
            }
        }
    }
}

macro_rules! memoise_recursive {
    ($raw_func:ident as $out_func:ident -> $ret_type:ident) => {
        fn $out_func(&mut self) -> Option<$ret_type> {
            let pos = self.mark();
            let key = (pos, String::from(stringify!($raw_func)));
            match self.memo.get(&key) {
                Some((end, result)) => {
                    let end = *end;
                    let result = (*result).clone();
                    if let Parsed::$ret_type(ret) = result {
                        self.reset(end);
                        return ret;
                    } else {unreachable!()}
                },
                None => {
                    let (mut lastres, mut lastpos) = (None, pos);
                    let memo = Parsed::$ret_type(lastres.clone());
                    self.memo.insert(key.clone(), (lastpos, memo));
                    loop {
                        self.reset(pos);
                        let result = self.$raw_func();
                        let endpos = self.mark();
                        if endpos <= lastpos {break};
                        lastres = result;
                        lastpos = endpos;
                        let memo = Parsed::$ret_type(lastres.clone());
                        self.memo.insert(key.clone(), (lastpos, memo));
                    }
                    self.reset(lastpos);
                    return lastres;
                }
            }
        }
    }
}


pub fn parse(tokens: Vec<Token>) -> Result<Module, ParseError>{
    let mut parser = Parser{tokens, token_pos: 0, max_token_pos: 0, memo: HashMap::new()};
    if let Some(module) = parser.module() {
        return Ok(module);
    }
    let max_token = parser.max_token();
    Err(ParseError{
        line: max_token.line,
        col: max_token.col
    })
}


impl Parser {

    fn mark(&self) -> usize {
        self.token_pos
    }

    fn reset(&mut self, pos: usize) {
        self.token_pos = pos;
        self.max_token_pos = cmp::max(pos, self.max_token_pos);
    }

    fn max_token(&self) -> Token {
        self.tokens[self.max_token_pos].clone()
    }

    fn expect_literal_with_src_position(&mut self, value: &str) -> Option<(usize, usize)> {
        let pos = self.mark();
        if let Some(tokenref) =  self.tokens.get(pos).as_ref() {
            if tokenref.string_ == value {
                let result = Some((tokenref.line.clone(), tokenref.col.clone()));
                self.reset(pos + 1);
                return result;
            };
        };
        None
    }

    fn expect_literal(&mut self, value: &str) -> bool {
        self.expect_literal_with_src_position(value).is_some()
    }

    fn expect_type(&mut self, type_: &str) -> Option<Token> {
        let pos = self.mark();
        if let Some(tokenref) =  self.tokens.get(pos).as_ref() {
            if tokenref.type_ == type_ {
                let result = Some((*tokenref).clone());
                self.reset(pos + 1);
                return result;
            }
        }
        None
    }

    fn repeat<F, R>(&mut self, method: F, allow_empty: bool) -> Option<Vec<R>>
        where F: Copy + Fn(&mut Parser) -> Option<R>
    {
        let pos = self.mark();
        let mut results = Vec::new();
        loop {
            match method(self) {
                Some(result) => results.push(result),
                None => break
            };
        };
        if results.is_empty() && !allow_empty {
            self.reset(pos);
            None
        } else {
            Some(results)
        }
    }

    fn join<F, R>(&mut self, item_method: F, seperator: &str) -> Vec<R>
        where F: Copy + Fn(&mut Parser) -> Option<R>
    {
        let mut ret = Vec::new();
        match item_method(self) {
            Some(item) => ret.push(item),
            None => return ret
        }
        loop {
            let pos = self.mark();
            if !self.expect_literal(seperator) {return ret}
            match item_method(self) {
                Some(item) => ret.push(item),
                None => {
                    self.reset(pos);
                    return ret
                }
            }
        }
    }

    
    pub fn module(&mut self) -> Option<Module> {
        parse!(self;
            global_stmts: self.repeat(Parser::global_statement, true),
            functions: self.repeat(Parser::function, false),
            _end: self.expect_type("END_MARKER!"),
            {
                let global_func = FunctionNode {
                    name: String::from("!global!"),
                    owned_links: Vec::new(),
                    borrow_params: Vec::new(),
                    steal_params: Vec::new(),
                    return_params: Vec::new(),
                    stmts: global_stmts                 
                };
                return Some(Module{global_func, functions});
            }
        );
        None
    }

    memoise!(function_ as function -> FunctionNode);
    pub fn function_(&mut self) -> Option<FunctionNode> {
        let pos = self.mark();

        if self.expect_literal("fn") {
        if let Some(name) = self.name() {
        let owned_links = self.links();
        if self.expect_literal("(") {
        let borrow_params = self.join(Parser::function_param, ",");
        if self.expect_literal(")") {
        if self.expect_literal("(") {
        let steal_params = self.join(Parser::function_param, ",");
        if self.expect_literal(")") {
        if self.expect_literal("{") {
        let stmts = self.repeat(Parser::statement, true).unwrap();
        if self.expect_literal("}") {
        if self.expect_literal("~") {
        if self.name() == Some(name.clone()) {
        if self.expect_literal("(") {
        let return_params = self.join(Parser::function_param, ",");
        if self.expect_literal(")") {
            return Some(FunctionNode{
                name, owned_links, borrow_params, steal_params, return_params, stmts
            });
        }}}}}}}}}}}};

        self.reset(pos);
        None
    }

    pub fn links(&mut self) -> Vec<String> {
        let pos = self.mark();
        if self.expect_literal("<") {
        let links = self.join(Parser::name, ",");
        if self.expect_literal(">") {
            return links;
        }}
        self.reset(pos);
        Vec::new()
    }

    memoise!(statement_ as statement -> StatementNode);
    pub fn statement_(&mut self) -> Option<StatementNode> {
        if let Some(stmt) = self.print_stmt() {return Some(stmt);}
        if let Some(stmt) = self.letunlet_stmt() {return Some(stmt);}
        if let Some(stmt) = self.refunref_stmt() {return Some(stmt);}
        if let Some(stmt) = self.modop_stmt() {return Some(stmt);}
        if let Some(stmt) = self.pull_stmt() {return Some(stmt);}
        if let Some(stmt) = self.if_stmt() {return Some(stmt);}
        if let Some(stmt) = self.while_stmt() {return Some(stmt);}
        if let Some(stmt) = self.for_stmt() {return Some(stmt);}
        if let Some(stmt) = self.doyield_stmt() {return Some(stmt);}
        if let Some(stmt) = self.catch_stmt() {return Some(stmt);}
        if let Some(stmt) = self.call_stmt() {return Some(stmt);}
        None
    }  

    memoise!(global_statement_ as global_statement -> StatementNode);
    pub fn global_statement_(&mut self) -> Option<StatementNode> {
        if let Some(stmt) = self.letunlet_stmt() {return Some(stmt);}
        if let Some(stmt) = self.refunref_stmt() {return Some(stmt);}
        if let Some(stmt) = self.modop_stmt() {return Some(stmt);}
        if let Some(stmt) = self.pull_stmt() {return Some(stmt);}
        None
    }  

    memoise!(function_param_ as function_param -> FunctionParam);
    pub fn function_param_(&mut self) -> Option<FunctionParam> {
        let pos = self.mark();

        if self.expect_literal("&") {
            if let Some(token) = self.expect_type("NAME") {
                if let Some(name) = self.name() {
                    let link = Some(token.string_);
                    return Some(FunctionParam{name, link, is_ref: true});
                } else {
                    return Some(FunctionParam{name: token.string_, is_ref: true, link: None});
                }
            }
        } else if let Some(name) = self.name() {
            return Some(FunctionParam{name, is_ref: false, link: None});
        }

        self.reset(pos);
        None
    }

    pub fn stolen_args(&mut self) -> Vec<String> {
        let pos = self.mark();
        let args = self.join(Parser::name, ",");
        if self.expect_literal("=>") {
            return args;
        }
        self.reset(pos);
        Vec::new()
    }

    pub fn return_args(&mut self) -> Vec<String> {
        let pos = self.mark();
        if self.expect_literal("=>") {
            let args = self.join(Parser::name, ",");
            return args;
        }
        self.reset(pos);
        Vec::new()
    }
    

    memoise!(call_stmt_ as call_stmt -> StatementNode);
    pub fn call_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        let stolen_args = self.stolen_args();
        let is_uncall = self.expect_literal("~");
        if let Some(name) = self.expect_type("NAME") {
        if self.expect_literal("(") {
        let borrow_args = self.join(Parser::lookup, ",");
        if self.expect_literal(")") {
        let return_args = self.return_args();
        if self.expect_literal(";") {
            let name = name.string_;
            return Some(Box::new(
                CallNode{is_uncall, name, borrow_args, stolen_args, return_args}
            ));
        }}}};

        self.reset(pos);
        None
    }


    memoise!(catch_stmt_ as catch_stmt -> StatementNode);
    pub fn catch_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        if self.expect_literal("catch") {
        if self.expect_literal("(") {
        if let Some(expr) = self.expression() {
        if self.expect_literal(")") {
        if self.expect_literal(";") {
            return Some(Box::new(
                CatchNode{expr}
            ));
        }}}}};

        self.reset(pos);
        None
    }

    memoise!(doyield_stmt_ as doyield_stmt -> StatementNode);
    pub fn doyield_stmt_(&mut self) -> Option<StatementNode> {
        parse!(self;
            "do",
            "{",
            do_stmts : self.repeat(Parser::statement, true),
            "}",
            yield_stmts : self.yield_block(),
            "~",
            "do",
            ";",
            {
                return Some(Box::new(
                    DoYieldNode{do_stmts, yield_stmts}
                ));
            }
        );
        None
    }
    
    memoise!(yield_block_ as yield_block -> VecStatementNode);
    pub fn yield_block_(&mut self) -> Option<Vec<StatementNode>> {
        let pos = self.mark();

        if self.expect_literal("yield") {
        if self.expect_literal("{") {
        let stmts = self.repeat(Parser::statement, true).unwrap();
        if self.expect_literal("}") {
            return Some(stmts);
        }}};
        self.reset(pos);

        Some(Vec::new())
    }

    memoise!(for_stmt_ as for_stmt -> StatementNode);
    pub fn for_stmt_(&mut self) -> Option<StatementNode> {
        parse!(self;
            "for",
            "(",
            iter_var : self.name(),
            "in",
            iterator : self.lookup(),
            ")",
            "{",
            stmts : self.repeat(Parser::statement, true),
            "}",
            ";",
            {
                return Some(Box::new(
                    ForNode{iter_var, iterator, stmts}
                ));
            }
        );
        None
    }

    memoise!(while_stmt_ as while_stmt -> StatementNode);
    pub fn while_stmt_(&mut self) -> Option<StatementNode> {
        parse!(self;
            "while",
            "(",
            fwd_expr : self.expression(),
            ")",
            "{",
            stmts : self.repeat(Parser::statement, true),
            "}",
            "~",
            "while",
            "(",
            ?bkwd_expr : self.expression(),
            ")",
            ";",
            {
                return Some(Box::new(
                    WhileNode{fwd_expr, stmts, bkwd_expr}
                ));
            }
        );
        None
    }

    memoise!(if_stmt_ as if_stmt -> StatementNode);
    pub fn if_stmt_(&mut self) -> Option<StatementNode> {
        parse!(self;
            "if",
            "(",
            fwd_expr : self.expression(),
            ")",
            "{",
            if_stmts : self.repeat(Parser::statement, true),
            "}",
            ? else_stmts : self.else_block(),
            "~",
            "if",
            "(",
            ? bkwd_expr : self.expression(),
            ")",
            ";",
            {
                let else_stmts = match else_stmts {
                    Some(stmts) => stmts,
                    None => Vec::new()
                };
                let bkwd_expr = match bkwd_expr {
                    Some(expr) => expr,
                    None => fwd_expr.clone()
                };
                return Some(Box::new(
                    IfNode{fwd_expr, if_stmts, else_stmts, bkwd_expr}
                ));
            }
        );
        None
    }

    memoise!(else_block_ as else_block -> VecStatementNode);
    pub fn else_block_(&mut self) -> Option<Vec<StatementNode>> {
        let pos = self.mark();

        if self.expect_literal("else") {
        if self.expect_literal("{") {
        let stmts = self.repeat(Parser::statement, true).unwrap();
        if self.expect_literal("}") {
            return Some(stmts);
        }}};
        self.reset(pos);

        None
    }

    memoise!(pull_stmt_ as pull_stmt -> StatementNode);
    pub fn pull_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();
        
        if let Some(name) = self.name() {
        if self.expect_literal("<=") {
        if let Some(lookup) = self.lookup() {
        if self.expect_literal(";") {
            return Some(Box::new(
                PushPullNode{is_push: false, name, lookup}
            ));    
        }}}};
        self.reset(pos);
        
        if let Some(name) = self.name() {
        if self.expect_literal("=>") {
        if let Some(lookup) = self.lookup() {
        if self.expect_literal(";") {
            return Some(Box::new(
                PushPullNode{is_push: true, name, lookup}
            ));    
        }}}};
        self.reset(pos);
                    
        None
    }

    memoise!(print_stmt_ as print_stmt -> StatementNode);
    pub fn print_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();
        
        if self.expect_literal("print") {
        if self.expect_literal("(") {
        let items = self.join(Parser::expression, ",");
        if self.expect_literal(")") {
        if self.expect_literal(";") {
            return Some(Box::new(
                PrintNode{items, newline: false}
            ));
        }}}};
        self.reset(pos);

        if self.expect_literal("println") {
        if self.expect_literal("(") {
        let items = self.join(Parser::expression, ",");
        if self.expect_literal(")") {
        if self.expect_literal(";") {
            return Some(Box::new(
                PrintNode{items, newline: true}
            ));
        }}}};

        self.reset(pos);
        None
    }

    memoise!(modop_stmt_ as modop_stmt -> StatementNode);
    pub fn modop_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        if let Some(lookup) = self.lookup() {
        if let Some(op) = self.modop() {
        if let Some(rhs) = self.expression() {
        if self.expect_literal(";") {
            return Some(Box::new(
                ModopNode{lookup, op, rhs}
            ));
        }}}};

        self.reset(pos);
        None
    }

    memoise!(refunref_stmt_ as refunref_stmt -> StatementNode);
    pub fn refunref_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        if let Some((name, (line, col))) = self.name_with_src_position() {
        if self.expect_literal(":=") {
        if self.expect_literal("&") {
        if let Some(rhs) = self.lookup() {
        if self.expect_literal(";") {
            return Some(Box::new(
                RefUnrefNode{name, rhs, line, col, is_unref: false}
            ));
        }}}}};
        self.reset(pos);

        if let Some((name, (line, col))) = self.name_with_src_position() {
        if self.expect_literal("=:") {
        if self.expect_literal("&") {
        if let Some(rhs) = self.lookup() {
        if self.expect_literal(";") {
            return Some(Box::new(
                RefUnrefNode{name, rhs, line, col, is_unref: true}
            ));
        }}}}};
        self.reset(pos);

        None
    }


    memoise!(letunlet_stmt_ as letunlet_stmt -> StatementNode);
    pub fn letunlet_stmt_(&mut self) -> Option<StatementNode> {
        let pos = self.mark();

        if let Some((name, (line, col))) = self.name_with_src_position() {
        if self.expect_literal(":=") {
        if let Some(rhs) = self.expression() {
        if self.expect_literal(";") {
            return Some(Box::new(
                LetUnletNode{name, rhs, line, col, is_unlet: false}
            ));
        }}}};
        self.reset(pos);

        if let Some((name, (line, col))) = self.name_with_src_position() {
        if self.expect_literal("=:") {
        if let Some(rhs) = self.expression() {
        if self.expect_literal(";") {
            return Some(Box::new(
                LetUnletNode{name, rhs, line, col, is_unlet: true}
            ));
        }}}};
        self.reset(pos);

        None
    }


    memoise_recursive!(expression_ as expression -> ExpressionNode);
    pub fn expression_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();
        
        if let Some(lhs) = self.expression() {
        if self.expect_literal("|") {
        if let Some(rhs) = self.expr0() {
            return Some(Box::new(
                BinopNode{lhs, rhs, op: Instruction::BinopOr}
            ));
        }}};
        self.reset(pos);

        self.reset(pos);
        self.expr0()
    }

    memoise_recursive!(expr0_ as expr0 -> ExpressionNode);
    pub fn expr0_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if let Some(lhs) = self.expr0() {
        if self.expect_literal("&") {
        if let Some(rhs) = self.expr1() {
            return Some(Box::new(
                BinopNode{lhs, rhs, op: Instruction::BinopAnd}
            ));
        }}};
        self.reset(pos);

        self.expr1()
    }

    memoise_recursive!(expr1_ as expr1 -> ExpressionNode);
    pub fn expr1_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if let Some(lhs) = self.expr1() {
        if self.expect_literal("^") {
        if let Some(rhs) = self.expr2() {
            return Some(Box::new(
                BinopNode{lhs, rhs, op: Instruction::BinopXor}
            ));
        }}};
        self.reset(pos);

        self.expr2()
    }
    
    memoise_recursive!(expr2_ as expr2 -> ExpressionNode);
    pub fn expr2_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if let Some(lhs) = self.expr2() {
        let instruction_match =
            if      self.expect_literal("<")  {Some(Instruction::BinopLess)}
            else if self.expect_literal("<=") {Some(Instruction::BinopLeq)}
            else if self.expect_literal(">")  {Some(Instruction::BinopGreat)}
            else if self.expect_literal(">=") {Some(Instruction::BinopGeq)}
            else if self.expect_literal("!=") {Some(Instruction::BinopNeq)}
            else if self.expect_literal("==") {Some(Instruction::BinopEq)}
            else                              {None};
        if let Some(instruction) = instruction_match {
        if let Some(rhs) = self.expr3() {
            return Some(Box::new(
                BinopNode{lhs, rhs, op: instruction}
            ));
        }}};
        self.reset(pos);

        self.expr3()
    }
    
    memoise_recursive!(expr3_ as expr3 -> ExpressionNode);
    pub fn expr3_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if let Some(lhs) = self.expr3() {
        if self.expect_literal("+") {
        if let Some(rhs) = self.expr4() {
            return Some(Box::new(
                BinopNode{lhs, rhs, op: Instruction::BinopAdd}
            ));
        }}};
        self.reset(pos);

        if let Some(lhs) = self.expr3() {
        if self.expect_literal("-") {
        if let Some(rhs) = self.expr4() {
            return Some(Box::new(
                BinopNode{lhs, rhs, op: Instruction::BinopSub}
            ));
        }}};
        self.reset(pos);

        self.expr4()
    }
    
    memoise_recursive!(expr4_ as expr4 -> ExpressionNode);
    pub fn expr4_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if let Some(lhs) = self.expr4() {
        let instruction_match =
            if      self.expect_literal("*")  {Some(Instruction::BinopMul)}
            else if self.expect_literal("/")  {Some(Instruction::BinopDiv)}
            else if self.expect_literal("//") {Some(Instruction::BinopIDiv)}
            else if self.expect_literal("%")  {Some(Instruction::BinopMod)}
            else                              {None};
        if let Some(instruction) = instruction_match {
        if let Some(rhs) = self.expr5() {
            return Some(Box::new(
                BinopNode{lhs, rhs, op: instruction}
            ));
        }}};
        self.reset(pos);

        self.expr5()
    }

    memoise_recursive!(expr5_ as expr5 -> ExpressionNode);
    pub fn expr5_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if let Some(lhs) = self.expr5() {
        if self.expect_literal("**") {
        if let Some(rhs) = self.atom() {
            return Some(Box::new(
                BinopNode{lhs, rhs, op: Instruction::BinopPow}
            ));
        }}};
        self.reset(pos);

        self.atom()
    }

    memoise_recursive!(atom_ as atom -> ExpressionNode);
    pub fn atom_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();
        
        if self.expect_literal("(") {
        if let Some(expr) = self.expression() {
        if self.expect_literal(")") {
            return Some(expr);
        }}};
        self.reset(pos);

        if let Some(array) = self.array_literal() {
            return Some(Box::new(array));
        };

        if let Some(array) = self.array_repeat() {
            return Some(Box::new(array));
        };

        if let Some(lookup) = self.lookup() {
            return Some(Box::new(lookup));
        };
        
        if let Some(token) = self.expect_type("NUMBER") {
            return Some(Box::new(
                FractionNode{
                    value: Fraction::from_str(&token.string_[..]).unwrap(),
                    line: token.line,
                    col: token.col
                }
            ));
        };

        if let Some(token) = self.expect_type("STRING") {
            return Some(Box::new(
                StringNode{
                    value: token.string_.clone(),
                    line: token.line,
                    col: token.col
                }
            ));
        };

        if let Some((line, col)) = self.expect_literal_with_src_position("-") {
        if let Some(expr) = self.atom() {
            return Some(Box::new(
                UniopNode{expr, line, col, op: Instruction::UniopNeg}
            ));
        }};
        self.reset(pos);

        if let Some((line, col)) = self.expect_literal_with_src_position("!") {
        if let Some(expr) = self.atom() {
            return Some(Box::new(
                UniopNode{expr, line, col, op: Instruction::UniopNot}
            ));
        }};
        self.reset(pos);

        if let Some((line, col)) = self.expect_literal_with_src_position("#") {
        if let Some(expr) = self.atom() {
            return Some(Box::new(
                UniopNode{expr, line, col, op: Instruction::UniopLen}
            ));
        }};
        self.reset(pos);

        None
    }

    memoise!(array_literal_ as array_literal -> ArrayLiteralNode);
    pub fn array_literal_(&mut self) -> Option<ArrayLiteralNode> {
        let pos = self.mark();

        if let Some((line, col)) = self.expect_literal_with_src_position("[") {
        let items = self.join(Parser::expression, ",");
        if self.expect_literal("]") {
            return Some(ArrayLiteralNode{line, col, items});
        }}

        self.reset(pos);
        None
    }

    memoise!(array_repeat_ as array_repeat -> ArrayRepeatNode);
    pub fn array_repeat_(&mut self) -> Option<ArrayRepeatNode> {
        let pos = self.mark();

        if let Some((line, col)) = self.expect_literal_with_src_position("[") {
        if let Some(item) = self.expression() {
        if self.expect_literal("repeat") {
        if let Some(dimensions) = self.expression() {
        if self.expect_literal("]") {
            return Some(ArrayRepeatNode{item, dimensions, line, col});
        }}}}}

        self.reset(pos);
        None
    }


    memoise!(modop_ as modop -> Instruction);
    pub fn modop_(&mut self) -> Option<Instruction> {
        if self.expect_literal("+=") { return Some(Instruction::BinopAdd) };
        if self.expect_literal("-=") { return Some(Instruction::BinopSub) };
        if self.expect_literal("*=") { return Some(Instruction::BinopMul) };
        if self.expect_literal("/=") { return Some(Instruction::BinopDiv) };
        None
    }


    memoise!(lookup_ as lookup -> LookupNode);
    pub fn lookup_(&mut self) -> Option<LookupNode> {
        let pos = self.mark();

        if let Some((name, (line, col))) = self.name_with_src_position() {
        if let Some(indices) = self.repeat(Parser::index, true) {
            return Some(LookupNode{name, indices, line, col});
        }};

        self.reset(pos);
        None
    }

    memoise!(index_ as index -> ExpressionNode);
    pub fn index_(&mut self) -> Option<ExpressionNode> {
        let pos = self.mark();

        if self.expect_literal("[") {
        if let Some(expr) = self.expression() {
        if self.expect_literal("]") {
            return Some(expr);
        }}};

        self.reset(pos);
        None
    }

    memoise!(name_ as name -> String);
    pub fn name_(&mut self) -> Option<String> {
        self.name_with_src_position().map(|x| x.0)
    }

    pub fn name_with_src_position(&mut self) -> Option<(String, (usize, usize))> {
        let pos = self.mark();

        let dot_pos = self.expect_literal_with_src_position(".");
        if let Some(token) = self.expect_type("NAME") {
            return Some( match dot_pos {
                Some(dot_pos) => (String::from(".") + &token.string_, dot_pos),
                None => (token.string_, (token.line, token.col))
            })
        };

        self.reset(pos);
        None
    }

}