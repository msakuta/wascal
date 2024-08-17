//! Implementation of type inference using TypeSet

use std::{collections::HashMap, sync::Condvar};

use crate::{
    model::TypeSet,
    parser::{Expression, Statement},
    Type,
};

#[derive(Debug, PartialEq)]
pub struct TypeInferFn {
    pub(crate) params: Vec<Type>,
    pub(crate) ret_ty: TypeSet,
}

pub fn get_type_infer_fns(stmt: &Statement, funcs: &mut HashMap<String, TypeInferFn>) {
    match stmt {
        Statement::FnDecl(fn_decl) => {
            funcs.insert(
                fn_decl.name.to_string(),
                TypeInferFn {
                    params: fn_decl
                        .params
                        .iter()
                        .map(|p| p.ty.determine().unwrap())
                        .collect(),
                    ret_ty: fn_decl.ret_ty,
                },
            );
        }
        _ => {}
    }
}

pub struct TypeInferer<'a> {
    funcs: &'a HashMap<String, TypeInferFn>,
    locals: HashMap<String, TypeSet>,
}

impl<'a> TypeInferer<'a> {
    pub fn new(funcs: &'a HashMap<String, TypeInferFn>) -> Self {
        Self {
            funcs,
            locals: HashMap::new(),
        }
    }

    pub fn infer_type_expr(&mut self, ex: &Expression) -> Result<TypeSet, String> {
        match ex {
            Expression::LiteralInt(_, ts) => Ok(*ts),
            Expression::LiteralFloat(_, ts) => Ok(*ts),
            Expression::Variable(name) => Ok(self
                .locals
                .get(*name)
                .copied()
                .unwrap_or(TypeSet::default())),
            Expression::FnInvoke(name, _) => {
                Ok(self.funcs.get(*name).map_or(TypeSet::ALL, |f| f.ret_ty))
            }
            Expression::Add(lhs, rhs)
            | Expression::Sub(lhs, rhs)
            | Expression::Mul(lhs, rhs)
            | Expression::Div(lhs, rhs) => {
                let lhs_ty = self.infer_type_expr(lhs)?;
                let rhs_ty = self.infer_type_expr(rhs)?;
                Ok(lhs_ty & rhs_ty)
            }
            Expression::Lt(_, _) | Expression::Gt(_, _) => {
                // Relation operators always return i32 (boolean)
                Ok(Type::I32.into())
            }
            Expression::Conditional(_, t_branch, f_branch) => {
                let t_ty = t_branch
                    .last()
                    .map_or(Ok(TypeSet::default()), |stmt| self.forward_type_stmt(stmt))?;
                let f_ty = f_branch.as_ref().map_or(Ok(TypeSet::default()), |stmts| {
                    stmts
                        .last()
                        .map_or(Ok(TypeSet::default()), |stmt| self.forward_type_stmt(stmt))
                })?;
                Ok(t_ty & f_ty)
            }
            _ => Ok(Type::Void.into()),
        }
    }

    fn propagate_type_expr(&mut self, ex: &mut Expression, ts: &TypeSet) {
        match ex {
            Expression::LiteralInt(_, target_ts) => *target_ts = dbg!(*ts),
            Expression::LiteralFloat(_, target_ts) => *target_ts = *ts,
            Expression::Variable(var) => {
                if let Some(var) = self.locals.get_mut(*var) {
                    *var = *ts;
                }
            }
            Expression::Add(lhs, rhs)
            | Expression::Sub(lhs, rhs)
            | Expression::Mul(lhs, rhs)
            | Expression::Div(lhs, rhs) => {
                self.propagate_type_expr(lhs, ts);
                self.propagate_type_expr(rhs, ts);
            }
            Expression::Lt(lhs, rhs)
            | Expression::Gt(lhs, rhs) => {
                println!("propagate_type_expr");
                self.propagate_type_expr(lhs, &Type::I32.into());
                self.propagate_type_expr(rhs, &Type::I32.into());
            }
            Expression::Conditional(cond, t_branch, f_branch) => {
                self.propagate_type_expr(cond, &Type::I32.into());
                if let Some(t_stmt) = t_branch
                    .last_mut() {
                    self.propagate_type_stmt(t_stmt, ts);
                }
                if let Some(f_stmt) = f_branch.as_mut().and_then(|stmts| stmts.last_mut()) {
                    self.propagate_type_stmt(f_stmt, ts);
                }
            }
            _ => {}
        }
    }

    fn propagate_type_stmt(&mut self, stmt: &mut Statement, ts: &TypeSet) {
        match stmt {
            Statement::VarDecl(name, decl_ty, ex) => {
                if let Some(&var_ts) = self.locals.get(*name) {
                    self.propagate_type_expr(ex, dbg!(&var_ts));
                    *decl_ty = var_ts;
                }
            }
            Statement::Expr(ex) => self.propagate_type_expr(ex, ts),
            Statement::Brace(stmts) => {
                if let Some(stmt) = stmts.last_mut() {
                    self.propagate_type_stmt(stmt, dbg!(ts));
                }
                for stmt in stmts.iter_mut().rev().skip(1) {
                    self.propagate_type_stmt(stmt, &TypeSet::default());
                }
            }
            Statement::FnDecl(fn_decl) => {
                for stmt in fn_decl.stmts.iter_mut().rev() {
                    self.propagate_type_stmt(stmt, &TypeSet::default());
                }
            }
            _ => {}
        }
    }

    fn forward_type_stmt(&mut self, stmt: &Statement) -> Result<TypeSet, String> {
        match stmt {
            Statement::Expr(ex) => self.infer_type_expr(dbg!(ex)),
            Statement::Brace(stmts) => {
                let mut last_ty = TypeSet::default();
                for stmt in stmts {
                    last_ty = self.forward_type_stmt(stmt)?;
                }
                Ok(last_ty)
            }
            _ => Ok(TypeSet::default()),
        }
    }

    pub fn infer_type_stmt(&mut self, stmt: &mut Statement) -> Result<(), String> {
        match stmt {
            Statement::VarDecl(name, ty, ex) => {
                let ex_ty = dbg!(self.infer_type_expr(ex)?);
                let inferred_ty = dbg!(ex_ty & TypeSet::from(*ty));
                if let Some(determined_ty) = inferred_ty.determine() {
                    // .ok_or_else(|| "Type could not be determined".to_string())?;
                    let determined_ts = TypeSet::from(determined_ty);
                    self.propagate_type_expr(ex, &determined_ts);
                }
                self.locals.insert(name.to_string(), inferred_ty);
                Ok(())
            }
            Statement::FnDecl(fn_decl) => {
                let mut inferer = TypeInferer::new(self.funcs);
                inferer.locals = fn_decl
                    .params
                    .iter()
                    .map(|param| (param.name.clone(), param.ty))
                    .collect();
                let mut last_ty = TypeSet::default();
                for stmt in &mut fn_decl.stmts {
                    last_ty = inferer.forward_type_stmt(stmt)?;
                    println!("stmt {stmt:?} ty {last_ty}");
                }
                dbg!(last_ty);
                if let Some(determined_ty) = (last_ty & fn_decl.ret_ty.into()).determine() {
                    let determined_ts = TypeSet::from(determined_ty);
                    dbg!(determined_ty);
                    fn_decl.ret_ty = determined_ts;
                    for stmt in fn_decl.stmts.iter_mut().rev() {
                        inferer.propagate_type_stmt(stmt, &determined_ts);
                    }
                } else {
                    println!(
                        "Function return type could not be determined: {}",
                        last_ty & fn_decl.ret_ty.into()
                    );
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}
