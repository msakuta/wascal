use crate::{const_table::ConstTable, model::FuncDef, parser::VarDecl, FuncImport, FuncType, Type};

use super::{Compiler, OpCode};

impl<'a> Compiler<'a> {
    /// Define `set` standard library function, which sets a byte in the specified address of memory.
    pub fn compile_set(
        types: &mut Vec<FuncType>,
        imports: &[FuncImport],
        const_table: &mut ConstTable,
        funcs: &mut Vec<FuncDef>,
    ) -> Result<(usize, usize), String> {
        let set_ty = types.len();
        types.push(Compiler::type_set());

        let args = vec![
            VarDecl {
                name: "ptr".to_string(),
                ty: Type::I32.into(),
            },
            VarDecl {
                name: "char".to_string(),
                ty: Type::I32.into(),
            },
        ];
        let num_args = args.len();

        let mut compiler = Compiler::new(args, Type::Void, types, imports, const_table, funcs);
        compiler.codegen_set()?;
        compiler.code.push(OpCode::End as u8);

        let func = FuncDef {
            name: "set".to_string(),
            ty: set_ty,
            ret_ty: Type::Void,
            args: num_args,
            locals: compiler.locals,
            code: compiler.code,
            public: true,
        };

        let set_fn = funcs.len();

        funcs.push(func);

        Ok((set_ty, set_fn))
    }

    fn type_set() -> FuncType {
        FuncType {
            params: vec![Type::I32, Type::I32],
            results: vec![],
        }
    }

    /// Assumes there is a value length in i32 on top of the stack, returning local index storing the address of the new buffer
    pub(super) fn codegen_set(&mut self) -> Result<(), String> {
        self.local_get(0);
        self.i32const(4);
        self.code.push(OpCode::I32Add as u8);
        self.local_get(1);
        self.i32store(0)?;
        Ok(())
    }
}
