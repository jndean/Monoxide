
mod interpreter;

trait Expression {
    fn compile() -> Vec<interpreter::Instruction>;
}

enum Literal {
    Frac()
}

struct Binop {

}