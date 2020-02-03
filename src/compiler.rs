
use crate::AST;
use crate::interpreter;
use interpreter::Instruction;


#[derive(Debug)]
pub struct CompilerCtx {
    consts: Vec<interpreter::Variable>,
    locals: Vec<interpreter::Variable>,
    local_use_names: Vec<Option<String>>
}


impl CompilerCtx {
    pub fn new() -> CompilerCtx {
        CompilerCtx{consts: Vec::new(), locals: Vec::new(), local_use_names: Vec::new()}
    }

    fn add_const(&mut self, val: interpreter::Variable) -> u16 {
        for (i, existing) in self.consts.iter().enumerate() {
            if *existing == val {return i as u16}
        }
        self.consts.push(val);
        (self.consts.len() - 1) as u16
    }
}


impl AST::ExpressionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        match &self {
            AST::ExpressionNode::Fraction(valbox) => valbox.compile(ctx),
            AST::ExpressionNode::Binop(valbox) => valbox.compile(ctx)
        }
    }
}

impl AST::FractionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let idx = ctx.add_const(
            interpreter::Variable::new_frac(self.value.clone())
        );
        vec![Instruction::LoadConst{idx}]
    }
}


impl AST::BinopNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let mut ret = Vec::new();
        ret.extend(self.lhs.compile(ctx));
        ret.extend(self.rhs.compile(ctx));
        ret.push(
            match self.op {
                AST::Binop::Add => Instruction::BinopAdd,
                AST::Binop::Sub => Instruction::BinopSub,
                AST::Binop::Mul => Instruction::BinopMul,
                AST::Binop::Div => Instruction::BinopDiv,
            }
        );
        ret
    }
}