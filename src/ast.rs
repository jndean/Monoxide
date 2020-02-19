
use std::fmt;

use crate::interpreter;


#[derive(Debug)]
pub enum ExpressionNode {
    Fraction(Box<FractionNode>),
    Lookup(Box<LookupNode>),
    Binop(Box<BinopNode>)
}


pub struct FractionNode {
    pub value: interpreter::Fraction
}

impl fmt::Debug for FractionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct LookupNode {
    pub name: String,
    pub indices: Vec<ExpressionNode>
}

#[derive(Debug)]
pub struct BinopNode {
    pub lhs: ExpressionNode,
    pub rhs: ExpressionNode,
    pub op: interpreter::Instruction
}


#[derive(Debug)]
pub enum StatementNode {
    LetUnlet(Box<LetUnletNode>)
}

#[derive(Debug)]
pub struct LetUnletNode {
    pub is_unlet: bool,
    pub name: String,
    pub rhs: ExpressionNode
}