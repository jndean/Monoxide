
use std::fmt;

use crate::interpreter;


#[derive(Clone, Debug)]
pub enum ExpressionNode {
    FractionNode(Box<FractionNode>),
    LookupNode(Box<LookupNode>),
    BinopNode(Box<BinopNode>),
    ArrayLiteralNode(Box<ArrayLiteralNode>)
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
pub struct ArrayLiteralNode {
    pub items: Vec<ExpressionNode>
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
    LetUnletNode(Box<LetUnletNode>),
    RefUnrefNode(Box<RefUnrefNode>),
    IfNode(Box<IfNode>),
    ModopNode(Box<ModopNode>),
    CatchNode(Box<CatchNode>)
}

#[derive(Clone, Debug)]
pub struct LetUnletNode {
    pub is_unlet: bool,
    pub name: String,
    pub rhs: ExpressionNode
}

#[derive(Clone, Debug)]
pub struct RefUnrefNode {
    pub is_unref: bool,
    pub name: String,
    pub rhs: LookupNode
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