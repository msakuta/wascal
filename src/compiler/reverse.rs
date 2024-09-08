use std::collections::HashMap;

use crate::{const_table::ConstTable, model::FuncDef, parser::VarDecl, FuncImport, FuncType, Type};

use super::{Compiler, OpCode};

impl<'a> Compiler<'a> {
    /// Define `reverse` standard library function, which creates a new string with bytes reversed.
    /// Note that it is not UTF-8 aware, so if you try reversing multi-byte characters, it would break!
    pub fn compile_reverse(
        types: &mut Vec<FuncType>,
        imports: &[FuncImport],
        const_table: &mut ConstTable,
        funcs: &mut Vec<FuncDef>,
    ) -> Result<(usize, usize), String> {
        let set_ty = types.len();
        types.push(FuncType {
            params: vec![Type::I32],
            results: vec![Type::I32],
        });

        let args = vec![VarDecl {
            name: "x".to_string(),
            ty: Type::Str.into(),
        }];
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
        compiler.codegen_reverse()?;
        compiler.code.push(OpCode::End as u8);

        let func = FuncDef {
            name: "reverse".to_string(),
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

    fn codegen_reverse(&mut self) -> Result<(), String> {
        // get length of the string
        self.local_get(0);
        self.i32load(0)?;

        let len = self.add_local("", Type::I32);

        // Allocate another chunk of memory with the same length
        self.local_get(len);
        let malloc_ty = self.call_func("malloc")?;
        let new_buf = self.add_local("", malloc_ty);

        self.local_get(new_buf);
        self.local_get(len);
        self.i32store(0)?;

        self.i32const(0);
        let idx = self.add_local("", Type::I32);

        self.emit_for_loop(idx, len, Type::I32, |this| {
            this.local_get(idx); // [idx]
            this.local_get(0); // [idx, ptr]

            this.code.push(OpCode::I32Add as u8); // [idx + ptr]

            this.i32load8_s(4)?; // [mem[idx + ptr + 4]]
            let data = this.add_local("", Type::I32); // []

            this.local_get(len); // [len]
            this.local_get(idx); // [len, idx]
            this.code.push(OpCode::I32Sub as u8); // [len - idx]
            this.i32const(1); // [len - idx, 1]
            this.code.push(OpCode::I32Sub as u8); // [len - idx - 1]
            this.local_get(new_buf); // [len - idx - 1, new_buf]
            this.code.push(OpCode::I32Add as u8); // [len - idx - 1 + new_buf]
            this.local_get(data); // [len - idx + new_buf, mem[idx + ptr + 4]]

            this.i32store8(4)?; // []

            Ok(())
        })?;

        self.local_get(new_buf);
        Ok(())
    }
}
