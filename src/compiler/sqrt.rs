use std::collections::HashMap;

use crate::{const_table::ConstTable, model::FuncDef, parser::VarDecl, FuncImport, FuncType, Type};

use super::{Compiler, OpCode};

impl<'a> Compiler<'a> {
    /// Define `strcat` standard library function, which concatenates 2 strings.
    pub fn compile_sqrt(
        types: &mut Vec<FuncType>,
        imports: &[FuncImport],
        const_table: &mut ConstTable,
        funcs: &mut Vec<FuncDef>,
    ) -> Result<(usize, usize), String> {
        let set_ty = types.len();
        types.push(FuncType {
            params: vec![Type::F64],
            results: vec![Type::F64],
        });

        let args = vec![VarDecl {
            name: "v".to_string(),
            ty: Type::F64.into(),
        }];
        let num_args = args.len();

        let structs = HashMap::new();

        let mut compiler = Compiler::new(
            args,
            Type::F64,
            types,
            imports,
            const_table,
            funcs,
            &structs,
        )?;
        compiler.codegen_sqrt()?;
        compiler.code.push(OpCode::End as u8);

        let func = FuncDef {
            name: "sqrt".to_string(),
            ty: set_ty,
            ret_ty: Type::F64,
            args: num_args,
            locals: compiler.locals,
            code: compiler.code,
            public: true,
        };

        let set_fn = funcs.len();

        funcs.push(func);

        Ok((set_ty, set_fn))
    }

    /// Pop [lhs, rhs] from the stack, create a string concatenated, returns the address
    /// of newly created buffer.
    fn codegen_sqrt(&mut self) -> Result<(), String> {
        self.local_get(0); // [lhs]
        self.code.push(OpCode::F64Sqrt as u8);
        Ok(())
    }
}
