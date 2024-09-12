use std::collections::HashMap;

use crate::{const_table::ConstTable, model::FuncDef, parser::VarDecl, FuncImport, FuncType, Type};

use super::{Compiler, OpCode};

impl<'a> Compiler<'a> {
    /// Define a function malloc, which allocates a block of heap memory
    /// with the size given by the argument.
    pub fn malloc(
        types: &mut Vec<FuncType>,
        imports: &[FuncImport],
        const_table: &mut ConstTable,
        funcs: &mut Vec<FuncDef>,
    ) -> Result<(usize, usize), String> {
        let malloc_ty = types.len();
        types.push(Compiler::type_malloc());

        let args = vec![VarDecl {
            name: "len".to_string(),
            ty: Type::I32.into(),
        }];
        let num_args = args.len();

        let structs = HashMap::new();

        let mut compiler = Compiler::new(
            args,
            Type::I32,
            types,
            imports,
            const_table,
            funcs,
            &structs,
        )?;
        compiler.local_get(0);
        let ret = compiler.codegen_malloc()?;
        compiler.local_get(ret);
        compiler.code.push(OpCode::End as u8);

        let func = FuncDef {
            name: "malloc".to_string(),
            ty: malloc_ty,
            ret_ty: Type::I32,
            args: num_args,
            locals: compiler.locals,
            code: compiler.code,
            public: true,
        };

        let malloc_fn = funcs.len();

        funcs.push(func);

        Ok((malloc_ty, malloc_fn))
    }

    fn type_malloc() -> FuncType {
        FuncType {
            params: vec![Type::I32],
            results: vec![Type::I32],
        }
    }

    /// Assumes there is a value length in i32 on top of the stack, returning local index storing the address of the new buffer
    pub(super) fn codegen_malloc(&mut self) -> Result<usize, String> {
        const ALLOC_ALIGN: u32 = 4;
        let stashed = self.add_local("", Type::I32); // []

        self.i32const(0); // [0]
        self.i32load(0)?; // [mem[0]]
        let start_addr = self.add_local("", Type::I32); // []

        self.i32const(0); // [0]

        // Round the length up to 4 bytes for the next allocation
        self.local_get(stashed);
        self.i32const(ALLOC_ALIGN - 1);
        self.code.push(OpCode::I32Add as u8);
        self.i32const(ALLOC_ALIGN);
        self.code.push(OpCode::I32DivU as u8);
        self.i32const(ALLOC_ALIGN);
        self.code.push(OpCode::I32Mul as u8); // [0, rounded_size = (stashed - 3) / 4 * 4]
        self.local_get(start_addr); // [0, rounded_size, start_addr]
        self.code.push(OpCode::I32Add as u8); // [0, new_size = rounded_size + start_addr]
        let new_size = self.add_local("", Type::I32);

        self.local_get(new_size);
        self.i32store(0)?;

        self.local_get(new_size);
        self.i32const(65536);
        self.code.push(OpCode::I32DivU as u8);
        self.code.push(OpCode::MemorySize as u8);
        self.code.push(0);
        self.code.push(OpCode::I32GeU as u8);
        self.code.push(OpCode::If as u8);
        self.code.push(Type::Void.code());
        self.local_get(new_size);
        self.i32const(65536);
        self.code.push(OpCode::I32DivS as u8);
        self.code.push(OpCode::MemoryGrow as u8);
        self.code.push(0);
        self.code.push(OpCode::Drop as u8);
        self.code.push(OpCode::End as u8);

        Ok(start_addr)
    }
}
