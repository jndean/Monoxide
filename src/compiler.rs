
use crate::ast;
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

    fn add_const(&mut self, val: interpreter::Variable) -> usize {
        for (i, existing) in self.consts.iter().enumerate() {
            if *existing == val {return i}
        }
        self.consts.push(val);
        self.consts.len() - 1
    }
}


impl ast::ExpressionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        match &self {
            ast::ExpressionNode::Fraction(valbox) => valbox.compile(ctx),
            ast::ExpressionNode::Binop(valbox) => valbox.compile(ctx)
        }
    }
}

impl ast::FractionNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let idx = ctx.add_const(
            interpreter::Variable::new_frac(self.value.clone())
        );
        vec![Instruction::LoadConst{idx}]
    }
}


impl ast::BinopNode {
    pub fn compile(&self, ctx: &mut CompilerCtx) -> Vec<Instruction> {
        let mut ret = Vec::new();
        ret.extend(self.lhs.compile(ctx));
        ret.extend(self.rhs.compile(ctx));
        ret.push(
            match self.op {
                ast::Binop::Add => Instruction::BinopAdd,
                ast::Binop::Sub => Instruction::BinopSub,
                ast::Binop::Mul => Instruction::BinopMul,
                ast::Binop::Div => Instruction::BinopDiv,
            }
        );
        ret
    }
}