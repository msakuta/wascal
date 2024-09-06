use std::collections::HashMap;

use crate::{const_table::ConstTable, model::FuncDef, parser::VarDecl, FuncImport, FuncType, Type};

use super::{encode_leb128, Compiler, OpCode};

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

        let args = vec![
            VarDecl {
                name: "lhs".to_string(),
                ty: Type::Str.into(),
            },
            VarDecl {
                name: "rhs".to_string(),
                ty: Type::Str.into(),
            },
        ];
        let num_args = args.len();

        let structs = HashMap::new();

        let mut compiler = Compiler::new(
            args,
            Type::Str,
            types,
            imports,
            const_table,
            funcs,
            &structs,
        );
        compiler.codegen_strcat()?;
        compiler.code.push(OpCode::End as u8);

        let func = FuncDef {
            name: "strcat".to_string(),
            ty: set_ty,
            ret_ty: Type::Str,
            args: num_args,
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
            params: vec![Type::I32, Type::I32],
            results: vec![Type::I32],
        }
    }

    /// Pop [lhs, rhs] from the stack, create a string concatenated, returns the address
    /// of newly created buffer.
    fn codegen_strcat(&mut self) -> Result<(), String> {
        self.local_get(0); // [lhs]
        self.i32load(0)?; // [lhs_len]
        let lhs_len = self.add_local("", Type::I32);
        self.local_get(1); // [lhs_len, rhs]
        self.i32load(0)?; // [lhs_len, rhs_len]
        let rhs_len = self.add_local("", Type::I32);
        self.local_get(lhs_len);
        self.local_get(rhs_len);
        self.code.push(OpCode::I32Add as u8); // [lhs_len + rhs_len]
        let total_len = self.add_local("", Type::I32);

        self.local_get(total_len);
        self.i32const(4);
        self.code.push(OpCode::I32Add as u8);
        let malloc_ty = self.call_func("malloc")?;
        let new_ptr = self.add_local("", malloc_ty);

        // Store total length at the beginning of buffer
        self.local_get(new_ptr);
        self.local_get(total_len);
        self.i32store(0)?;

        self.i32const(0);
        let idx = self.add_local("", Type::I32);

        self.emit_for_loop(idx, lhs_len, Type::I32, |this| {
            this.local_get(idx); // [idx]
            this.local_get(0); // [idx, lhs]

            this.code.push(OpCode::I32Add as u8); // [idx + lhs]

            this.i32load8_s(4)?; // [mem[idx + lhs]]
            let data = this.add_local("", Type::I32); // []

            this.local_get(idx); // [idx]
            this.local_get(new_ptr); // [idx, new_ptr]
            this.code.push(OpCode::I32Add as u8); // [idx + new_ptr]
            this.local_get(data); // [idx + new_ptr, mem[idx + lhs]]

            this.i32store8(4)?; // []

            Ok(())
        })?;

        self.i32const(0);
        self.code.push(OpCode::LocalSet as u8);
        encode_leb128(&mut self.code, idx as u32).unwrap();

        self.emit_for_loop(idx, rhs_len, Type::I32, |this| {
            this.local_get(idx); // [idx]
            this.local_get(1); // [idx, rhs]

            this.code.push(OpCode::I32Add as u8); // [idx + rhs]

            this.i32load8_s(4)?; // [mem[idx + rhs]]
            let data = this.add_local("", Type::I32); // []

            this.local_get(idx); // [idx]
            this.local_get(new_ptr); // [idx, new_ptr]
            this.code.push(OpCode::I32Add as u8); // [idx + new_ptr]
            this.local_get(lhs_len); // [idx + new_ptr, lhs_len]
            this.code.push(OpCode::I32Add as u8); // [idx + new_ptr + lhs_len]
            this.local_get(data); // [idx + new_ptr + lhs_len, mem[idx + rhs]]

            this.i32store8(4)?; // []

            Ok(())
        })?;

        self.local_get(new_ptr);
        Ok(())
    }
}
