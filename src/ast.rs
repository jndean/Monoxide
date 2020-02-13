
use std::fmt;

use crate::interpreter;


/*trait Expression {
    fn compile() -> Vec<interpreter::Instruction>;
}*/


#[derive(Debug)]
pub enum ExpressionNode {
    Fraction(Box<FractionNode>),
    Binop(Box<BinopNode>)
}


#[derive(Debug)]
pub struct FractionNode {
    pub value: interpreter::Fraction
}


#[derive(Debug)]
pub enum Binop {
    Add, Sub, Mul, Div
}


#[derive(Debug)]
pub struct BinopNode {
    pub lhs: ExpressionNode,
    pub rhs: ExpressionNode,
    pub op: Binop
}