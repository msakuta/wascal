use crate::{parser::Expression, OpCode};

pub struct Compiler {
    code: Vec<u8>,
    target_stack: usize,
    locals: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: vec![],
            target_stack: 0,
            locals: 0,
        }
    }

    pub fn compile(&mut self, ast: &Expression) {
        // let mut target_stack = vec![];
        let last = self.emit_expr(ast);
        self.code
            .extend_from_slice(&[OpCode::LocalGet as u8, last as u8]);
        self.code.push(OpCode::End as u8);
    }

    pub fn get_code(&self) -> &[u8] {
        &self.code
    }

    pub fn get_locals(&self) -> usize {
        self.locals
    }

    fn emit_expr(&mut self, ast: &Expression) -> usize {
        match ast {
            Expression::Literal(num) => {
                // target_stack.push(());
                self.code.push(OpCode::I32Const as u8);
                self.code.push(*num as u8);
                self.code
                    .extend_from_slice(&[OpCode::LocalSet as u8, self.locals as u8]);
                let ret = self.locals;
                self.locals += 1;
                ret
            }
            Expression::Add(lhs, rhs) => {
                let lhs = self.emit_expr(lhs);
                let rhs = self.emit_expr(rhs);
                self.code
                    .extend_from_slice(&[OpCode::LocalGet as u8, lhs as u8]);
                self.code
                    .extend_from_slice(&[OpCode::LocalGet as u8, rhs as u8]);
                self.code.push(OpCode::I32Add as u8);
                self.code
                    .extend_from_slice(&[OpCode::LocalSet as u8, self.locals as u8]);
                let ret = self.locals;
                self.locals += 1;
                ret
            }
        }
    }
}
