
use std::fmt;

use crate::interpreter;


#[derive(Debug)]
pub enum ExpressionNode {
    Fraction(Box<FractionNode>),
    Lookup(Box<ExpressionNode>),
    Binop(Box<BinopNode>)
}


#[derive(Debug)]
pub struct FractionNode {
    pub value: interpreter::Fraction
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