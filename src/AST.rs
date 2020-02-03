
use crate::interpreter;


/*trait Expression {
    fn compile() -> Vec<interpreter::Instruction>;
}*/



pub enum ExpressionNode {
    Fraction(Box<FractionNode>),
    Binop(Box<BinopNode>)
}

pub struct FractionNode {
    pub value: interpreter::Fraction
}


pub enum Binop {
    Add, Sub, Mul, Div
}

pub struct BinopNode {
    pub lhs: ExpressionNode,
    pub rhs: ExpressionNode,
    pub op: Binop
}