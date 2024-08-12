use std::io::Write;

use crate::parser::Expression;

#[repr(C)]
pub(crate) enum OpCode {
    LocalGet = 0x20,
    LocalSet = 0x21,
    I32Const = 0x41,
    I32Add = 0x6a,
    I32Mul = 0x6c,
    End = 0x0b,
}

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

    pub fn disasm(&self, f: &mut impl Write) -> std::io::Result<()> {
        #![allow(non_upper_case_globals)]
        const LocalGet: u8 = OpCode::LocalGet as u8;
        const LocalSet: u8 = OpCode::LocalSet as u8;
        const I32Const: u8 = OpCode::I32Const as u8;
        const I32Add: u8 = OpCode::I32Add as u8;
        const I32Mul: u8 = OpCode::I32Mul as u8;
        const End: u8 = OpCode::End as u8;
        let mut i = 0;
        while i < self.code.len() {
            let op_code = self.code[i];
            i += 1;
            match op_code {
                LocalGet => {
                    let arg = self.code[i];
                    i += 1;
                    writeln!(f, "  local.get {arg}")?;
                }
                LocalSet => {
                    let arg = self.code[i];
                    i += 1;
                    writeln!(f, "  local.set {arg}")?;
                }
                I32Const => {
                    let arg = self.code[i];
                    i += 1;
                    writeln!(f, "  i32.const {arg}")?;
                }
                I32Add => {
                    writeln!(f, "  i32.add")?;
                }
                I32Mul => {
                    writeln!(f, "  i32.mul")?;
                }
                End => return Ok(()),
                _ => panic!("code is broken: unknown opcode {op_code}"),
            }
        }
        Ok(())
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
                if let (Expression::Literal(lhs), Expression::Literal(rhs)) = (&**lhs, &**rhs) {
                    self.code
                        .extend_from_slice(&[OpCode::I32Const as u8, *lhs as u8]);
                    self.code
                        .extend_from_slice(&[OpCode::I32Const as u8, *rhs as u8]);
                } else {
                    let lhs = self.emit_expr(lhs);
                    let rhs = self.emit_expr(rhs);
                    self.code
                        .extend_from_slice(&[OpCode::LocalGet as u8, lhs as u8]);
                    self.code
                        .extend_from_slice(&[OpCode::LocalGet as u8, rhs as u8]);
                }
                self.code.push(OpCode::I32Add as u8);
                self.code
                    .extend_from_slice(&[OpCode::LocalSet as u8, self.locals as u8]);
                let ret = self.locals;
                self.locals += 1;
                ret
            }
            Expression::Mul(lhs, rhs) => {
                if let (Expression::Literal(lhs), Expression::Literal(rhs)) = (&**lhs, &**rhs) {
                    self.code
                        .extend_from_slice(&[OpCode::I32Const as u8, *lhs as u8]);
                    self.code
                        .extend_from_slice(&[OpCode::I32Const as u8, *rhs as u8]);
                } else {
                    let lhs = self.emit_expr(lhs);
                    let rhs = self.emit_expr(rhs);
                    self.code
                        .extend_from_slice(&[OpCode::LocalGet as u8, lhs as u8]);
                    self.code
                        .extend_from_slice(&[OpCode::LocalGet as u8, rhs as u8]);
                }
                self.code.push(OpCode::I32Mul as u8);
                self.code
                    .extend_from_slice(&[OpCode::LocalSet as u8, self.locals as u8]);
                let ret = self.locals;
                self.locals += 1;
                ret
            }
        }
    }
}
