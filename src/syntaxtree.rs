

use crate::interpreter;


#[derive(Clone, Debug)]
pub enum ExpressionNode {
    FractionNode(Box<FractionNode>),
    LookupNode(Box<LookupNode>),
    BinopNode(Box<BinopNode>),
    ArrayLiteralNode(Box<ArrayLiteralNode>)
}

#[derive(Clone, Debug)]
pub struct FractionNode {
    pub const_idx: usize
}

#[derive(Clone, Debug)]
pub struct ArrayLiteralNode {
    pub items: Vec<ExpressionNode>
}

#[derive(Clone, Debug)]
pub struct LookupNode {
    pub register: usize,
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
    PushPullNode(Box<PushPullNode>),
    ModopNode(Box<ModopNode>),
    CatchNode(Box<CatchNode>),
    CallNode(Box<CallNode>)
}

#[derive(Clone, Debug)]
pub struct LetUnletNode {
    pub is_unlet: bool,
    pub register: usize,
    pub rhs: ExpressionNode
}

#[derive(Clone, Debug)]
pub struct RefUnrefNode {
    pub is_unref: bool,
    pub register: usize,
    pub rhs: LookupNode
}

#[derive(Clone, Debug)]
pub struct ModopNode {
    pub lookup: LookupNode,
    pub op: interpreter::Instruction,
    pub rhs: ExpressionNode
}

#[derive(Clone, Debug)]
pub struct PushPullNode {
    pub is_push: bool,
    pub register: usize,
    pub lookup: LookupNode
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
pub struct CallNode {
    pub is_uncall: bool,
    pub func_idx: usize,
    pub borrow_args: Vec<LookupNode>,
    pub stolen_args: Vec<usize>,
    pub return_args: Vec<usize>
}


#[derive(Clone, Debug)]
pub struct ParamLink {
    pub is_interior: bool,
    pub link: Option<String>,  // None if unbound link //
    pub linked_borrow: Option<usize>,
    pub linked_io: Option<usize>,
}

#[derive(Clone, Debug, Default)]
pub struct FunctionPrototype {
    pub id: usize,
    pub owned_link_groups: Vec<[Vec<usize>; 3]>,
    pub borrow_params: Vec<Option<ParamLink>>,
    pub steal_params: Vec<Option<ParamLink>>,
    pub return_params: Vec<Option<ParamLink>>
}

#[derive(Clone, Debug)]
pub struct FunctionNode {
    pub stmts: Vec<StatementNode>,
    pub consts: Vec<interpreter::Variable>,
    pub num_registers: usize,

    pub borrow_registers: Vec<usize>,
    pub steal_registers: Vec<usize>,
    pub return_registers: Vec<usize>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub functions: Vec<FunctionNode>,
    pub main_idx: Option<usize>
}