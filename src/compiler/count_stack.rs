//! Methods to calculate the size of the linear memory stack (not wasm operand stack!).
//! It has to exactly match the logic of emit_*, otherwise stack overflow can occur.

use crate::{
    parser::{Expression, Statement},
    Type,
};

use super::Compiler;

impl<'a> Compiler<'a> {
    fn count_stack_expr(&mut self, ast: &Expression) -> Result<usize, String> {
        use Expression::*;
        Ok(match ast {
            LiteralInt(_, _) | LiteralFloat(_, _) => 0,
            // String literals are baked into constant table, so it doesn't count in the stack.
            StrLiteral(_) => 0,
            StructLiteral(stname, fields) => {
                let Some(stdef) = self.structs.get(*stname) else {
                    return Err(format!("Struct {stname} not found"));
                };

                println!("String literal size {}", stdef.size);

                let mut stack_size = stdef.size;
                for (_, field) in fields {
                    stack_size += self.count_stack_expr(field)?;
                }
                stack_size
            }
            // Referencing a variable does not cost a stack.
            Variable(_) => 0,
            FnInvoke(fname, args) => {
                let mut stack_size = 0;
                for arg in args {
                    stack_size += self.count_stack_expr(arg)?;
                }

                let (_, ret_ty, _) = self
                    .find_func(fname)
                    .ok_or_else(|| format!("Calling undefined function {fname}"))?;

                println!("count_stack: Calling function {}", fname);

                if let Some(stname) = ret_ty.struct_name() {
                    let st = self
                        .structs
                        .get(stname)
                        .ok_or_else(|| format!("Struct {stname} not found"))?;
                    println!("Adding struct {stname} size {}", st.size);
                    stack_size += st.size;
                }

                stack_size
            }
            Cast(ex, _ty) => self.count_stack_expr(ex)?,
            FieldAccess(ex, _fname) => self.count_stack_expr(ex)?,
            // TODO: operator overloading for neg
            Neg(ex) => self.count_stack_expr(ex)?,
            Add(lhs, rhs) => self.count_stack_bin_op(lhs, rhs, "add")?,
            Sub(lhs, rhs) => self.count_stack_bin_op(lhs, rhs, "sub")?,
            Mul(lhs, rhs) => self.count_stack_bin_op(lhs, rhs, "mul")?,
            Div(lhs, rhs) => self.count_stack_bin_op(lhs, rhs, "div")?,
            // We assume logical operators use primitive i32, which consumes no linear memory stack.
            // TODO: operator overloading for comparison operators
            And(_, _) | Or(_, _) | Gt(_, _) | Lt(_, _) => 0,
            Conditional(cond, t_branch, f_branch) => {
                let mut stack_size =
                    self.count_stack_expr(cond)? + self.count_stack_stmts(t_branch)?;
                if let Some(stmts) = f_branch {
                    stack_size += self.count_stack_stmts(stmts)?;
                }
                stack_size
            }
        })
    }

    fn type_expr(&self, ex: &Expression) -> Result<Type, String> {
        use Expression::*;
        Ok(match ex {
            LiteralInt(_, ts) | LiteralFloat(_, ts) => ts
                .determine()
                .ok_or_else(|| format!("Literal type could not be determined: {ts}"))?,
            StrLiteral(_) => Type::Str,
            StructLiteral(stname, _fields) => Type::Struct(stname.to_string()),
            Variable(vname) => self
                .local_ty
                .get(*vname)
                .ok_or_else(|| format!("Variable {vname} not found"))?
                .clone(),
            FnInvoke(fname, _args) => {
                let (_, ret_ty, _) = self
                    .find_func(fname)
                    .ok_or_else(|| format!("Calling undefined function {fname}"))?;
                ret_ty
            }
            Cast(_, ty) => ty.clone(),
            FieldAccess(ex, fname) => {
                let ty = self.type_expr(ex)?;
                let Type::Struct(stname) = ty else {
                    return Err("Field access operator got non-struct".to_string());
                };
                let st = self
                    .structs
                    .get(&stname)
                    .ok_or_else(|| format!("Struct {} not found", stname))?;
                st.fields
                    .iter()
                    .find_map(|field| {
                        if field.name == *fname {
                            Some(field.ty.clone())
                        } else {
                            None
                        }
                    })
                    .ok_or_else(|| format!("Field {fname} not found"))?
            }
            // TODO: operator overloading for neg
            Neg(ex) => self.type_expr(ex)?,
            Add(lhs, _rhs) => self.type_expr(lhs)?,
            Sub(lhs, _rhs) => self.type_expr(lhs)?,
            Mul(lhs, _rhs) => self.type_expr(lhs)?,
            Div(lhs, _rhs) => self.type_expr(lhs)?,
            // We assume logical operators use primitive i32, which consumes no linear memory stack.
            // TODO: operator overloading for comparison operators
            And(_, _) | Or(_, _) | Gt(_, _) | Lt(_, _) => Type::I32,
            Conditional(_, t_branch, _) => t_branch
                .last()
                .map_or(Ok(Type::Void), |stmt| self.type_stmt(stmt))?,
        })
    }

    fn type_stmt(&self, stmt: &Statement) -> Result<Type, String> {
        use Statement::*;
        Ok(match stmt {
            VarDecl(_, _, _) | VarAssign(_, _) | Struct(_) | FnDecl(_) | For(_) => Type::Void,
            Expr(ex, _) => self.type_expr(ex)?,
            Brace(stmts) => stmts
                .last()
                .map_or(Ok(Type::Void), |stmt| self.type_stmt(stmt))?,
            Return(ex) => ex
                .as_ref()
                .map_or(Ok(Type::Void), |ex| self.type_expr(ex))?,
        })
    }

    fn count_stack_bin_op(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        name: &str,
    ) -> Result<usize, String> {
        let lhs_sz = self.count_stack_expr(lhs)?;
        let lhs_ty = self.type_expr(lhs)?;
        let rhs_sz = self.count_stack_expr(rhs)?;
        let rhs_ty = self.type_expr(rhs)?;

        // If either side of the expression has a type that requires linear memory stack,
        // we should call operator overloading function
        Ok(match (&lhs_ty, &rhs_ty) {
            (Type::Struct(_), Type::Struct(_)) => {
                let dunder = format!("__{name}__");
                let (_, ret_ty, _) = self
                    .find_func(&dunder)
                    .ok_or_else(|| format!("Calling undefined function {}", dunder))?;

                println!(
                    "count_stack: Calling function {} adding stack size {}",
                    dunder,
                    lhs_sz + rhs_sz
                );

                let mut stack_size = lhs_sz + rhs_sz;

                // Reserve stack space for returned value
                if let Some(stname) = ret_ty.struct_name() {
                    let st = self
                        .structs
                        .get(stname)
                        .ok_or_else(|| format!("Struct {stname} not found"))?;
                    stack_size += st.size;
                }

                stack_size
            }
            _ => lhs_sz + rhs_sz,
        })
    }

    fn count_stack_stmt(&mut self, stmt: &Statement) -> Result<usize, String> {
        use Statement::*;
        Ok(match stmt {
            VarDecl(name, ty, ex) => {
                // Variable declaration actually won't consume stack, because it just stores a point to it
                // in Wasm locals.
                let stack_size = self.count_stack_expr(ex)?;

                self.local_ty.insert(
                    name.to_string(),
                    ty.determine()
                        .ok_or_else(|| format!("Type could not be determined {ty}"))?,
                );

                stack_size
            }
            // Similarly, lvalue expression does not consume stack, because it returns i32 pointing to the address
            VarAssign(_, ex) => self.count_stack_expr(ex)?,
            Expr(ex, _) => self.count_stack_expr(ex)?,
            FnDecl(_) => 0,
            For(for_stmt) => {
                self.local_ty
                    .insert(for_stmt.name.to_string(), self.type_expr(&for_stmt.start)?);
                self.count_stack_expr(&for_stmt.start)?
                    + self.count_stack_expr(&for_stmt.end)?
                    + self.count_stack_stmts(&for_stmt.stmts)?
            }
            Brace(stmts) => self.count_stack_stmts(stmts)?,
            Return(ex) => ex.as_ref().map_or(Ok(0), |ex| self.count_stack_expr(ex))?,
            Struct(_) => 0,
        })
    }

    pub(super) fn count_stack_stmts(&mut self, stmts: &[Statement]) -> Result<usize, String> {
        let mut stack_size = 0;
        for stmt in stmts {
            stack_size += self.count_stack_stmt(stmt)?;
        }
        Ok(stack_size)
    }
}
