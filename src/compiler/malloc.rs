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

        let mut compiler = Compiler::new(
            vec![VarDecl {
                name: "len".to_string(),
                ty: Type::I32.into(),
            }],
            Type::I32,
            types,
            imports,
            const_table,
            funcs,
        );
        compiler.local_get(0);
        let ret = compiler.codegen_malloc()?;
        compiler.local_get(ret);
        compiler.code.push(OpCode::End as u8);

        let func = FuncDef {
            name: "malloc".to_string(),
            ty: malloc_ty,
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
                          // self.local_get(stashed); // [0, stashed]

        // Round the length up to 4 bytes for the next allocation
        self.local_get(stashed);
        self.i32const(ALLOC_ALIGN - 1);
        self.code.push(OpCode::I32Add as u8);
        self.i32const(ALLOC_ALIGN);
        self.code.push(OpCode::I32DivS as u8);
        self.i32const(ALLOC_ALIGN);
        self.code.push(OpCode::I32Mul as u8);
        // self.i32and(!(0x3));

        self.i32const(0); // [0, stashed, 0]
        self.i32load(0)?; // [0, stashed, mem[0]]

        self.code.push(OpCode::I32Add as u8); // [0, stashed + mem[0]]

        self.i32store(0)?; // []

        Ok(start_addr)
    }
}
