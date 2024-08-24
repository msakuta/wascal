use crate::{const_table::ConstTable, model::FuncDef, parser::VarDecl, FuncImport, FuncType, Type};

use super::{Compiler, OpCode};

impl<'a> Compiler<'a> {
    /// Define `set` standard library function, which sets a byte in the specified address of memory.
    pub fn compile_strcat(
        types: &mut Vec<FuncType>,
        imports: &[FuncImport],
        const_table: &mut ConstTable,
        funcs: &mut Vec<FuncDef>,
    ) -> Result<(usize, usize), String> {
        let set_ty = types.len();
        types.push(Compiler::type_strcat());

        let mut compiler = Compiler::new(
            vec![
                VarDecl {
                    name: "lhs_ptr".to_string(),
                    ty: Type::I32.into(),
                },
                VarDecl {
                    name: "lhs_len".to_string(),
                    ty: Type::I32.into(),
                },
                VarDecl {
                    name: "rhs_ptr".to_string(),
                    ty: Type::I32.into(),
                },
                VarDecl {
                    name: "rhs_len".to_string(),
                    ty: Type::I32.into(),
                },
            ],
            Type::Str,
            types,
            imports,
            const_table,
            funcs,
        );
        compiler.codegen_strcat()?;
        compiler.code.push(OpCode::End as u8);

        let func = FuncDef {
            name: "strcat".to_string(),
            ty: set_ty,
            locals: compiler.locals,
            code: compiler.code,
            public: true,
        };

        let set_fn = funcs.len();

        funcs.push(func);

        Ok((set_ty, set_fn))
    }

    fn type_strcat() -> FuncType {
        FuncType {
            params: vec![Type::I32, Type::I32, Type::I32, Type::I32, Type::I32],
            results: vec![],
        }
    }

    /// Pop [lhs_ptr, lhs_len, rhs_ptr, rhs_len] from the stack, create a string concatenated, returns the address
    /// of newly created buffer.
    fn codegen_strcat(&mut self) -> Result<(), String> {
        self.local_get(1); // [lhs_len]
        self.local_get(3); // [lhs_len, rhs_len]
        self.code.push(OpCode::I32Add as u8); // [lhs_len + rhs_len]
        let total_len = self.add_local("", Type::I32);

        self.local_get(total_len);
        let malloc_ty = self.call_func("malloc")?;
        let new_ptr = self.add_local("", malloc_ty);

        self.i32const(0);
        let idx = self.add_local("", Type::I32);

        self.emit_for_loop(idx, total_len, Type::I32, |this| {
            this.local_get(idx); // [idx]
            this.local_get(0); // [idx, lhs_ptr]

            this.code.push(OpCode::I32Add as u8); // [idx + lhs_ptr]

            this.i32load8_s(4)?; // [mem[idx + lhs_ptr]]
            let data = this.add_local("", Type::I32); // []

            this.local_get(idx); // [idx]
            this.local_get(new_ptr); // [idx + new_ptr]
            this.code.push(OpCode::I32Add as u8); // [idx + new_ptr]
            this.local_get(data); // [idx + new_ptr, mem[idx + ptr]]

            this.i32store8(4)?; // []

            Ok(())
        })?;

        // Store the result to return address
        self.local_get(4);
        self.local_get(new_ptr);
        self.i32store(4)?;

        // Store the result to return address + 4
        self.local_get(4);
        self.local_get(total_len);
        self.i32store(8)?;
        Ok(())
    }
}
