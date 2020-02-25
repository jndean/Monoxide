
use std::fmt;

use crate::interpreter;


#[derive(Clone, Debug)]
pub enum ExpressionNode {
    Fraction(Box<FractionNode>),
    Lookup(Box<LookupNode>),
    Binop(Box<BinopNode>)
}

#[derive(Clone)]
pub struct FractionNode {
    pub value: interpreter::Fraction
}

impl fmt::Debug for FractionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct LookupNode {
    pub name: String,
    pub indices: Vec<ExpressionNode>
}

#[derive(Clone, Debug)]
pub struct BinopNode {
    pub lhs: ExpressionNode,
    pub rhs: ExpressionNode,
    pub op: interpreter::Instruction
}


#[derive(Clone, Debug)]
pub enum StatementNode {
    LetUnlet(Box<LetUnletNode>),
    If(Box<IfNode>),
    Modop(Box<ModopNode>),
    Catch(Box<CatchNode>)
}

#[derive(Clone, Debug)]
pub struct LetUnletNode {
    pub is_unlet: bool,
    pub name: String,
    pub rhs: ExpressionNode
}

#[derive(Clone, Debug)]
pub struct ModopNode {
    pub lookup: LookupNode,
    pub op: interpreter::Instruction,
    pub rhs: ExpressionNode
}

#[derive(Clone, Debug)]
pub struct IfNode {
    pub fwd_expr: ExpressionNode,
    pub if_stmts: Vec<StatementNode>,
    pub else_stmts: Vec<StatementNode>,
    pub bkwd_expr: ExpressionNode
}

#[derive(Clone, Debug)]
pub struct CatchNode {
    pub expr: ExpressionNode
}


#[derive(Clone, Debug)]
pub struct FunctionNode {
    pub name: String,
    pub borrow_params: Vec<String>,
    pub steal_params: Vec<String>,
    pub return_params: Vec<String>,
    pub stmts: Vec<StatementNode>
}

#[derive(Clone, Debug)]
pub struct Module {
    pub functions: Vec<FunctionNode>
}