

enum Variable {
    Frac(i32),
    Array(i32),
    Str(i32)
}

enum Instruction {
    LoadConst{idx: u16},
    LoadLocal{idx: u16},
    StoreLocal{idx: u16}
}

enum StackObject {
    Variable(Variable),
    Other
}


struct Scope {
    code: Vec<Instruction>,
    ip: usize,

    stack: Vec<StackObject>,
    locals: Vec<Variable>,
    consts: Vec<Variable>
}


impl Scope {
    fn run(&mut self) -> () {
        while self.ip < self.code.len() {
            match self.code[self.ip] {
                Instruction::LoadConst{idx} => self.load_const(idx),
                Instruction::LoadLocal{idx} => println!("LoadLocal({})", idx),
                Instruction::StoreLocal{idx} => println!("StoreLocal({})", idx),

            }
            self.ip += 1;
        }
    }

        
    fn load_const(&mut self, idx: u16) {
        let var = &self.consts[idx as usize];
        match var {
            Variable::Frac(val) => println!("ConstLoading {}", val),
            Variable::Array(val) => println!("ConstLoading {}", val),
            Variable::Str(val) => println!("ConstLoading {}", val),
        }
    }
}


fn main() {
    let code = vec![
        Instruction::LoadLocal {idx: 0},
        Instruction::StoreLocal {idx: 1},
        Instruction::LoadConst {idx: 0},
    ];

    let mut locals = vec![
        Variable::Frac(1701),
        Variable::Array(99),
    ];

    let consts = vec![
        Variable::Frac(101)
    ];

    let mut scope = Scope{
        code,
        ip: 0,
        stack: Vec::new(),
        locals,
        consts
    };
    scope.run()
    

}
