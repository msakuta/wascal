//! Implementation of type inference using TypeSet

use std::{collections::HashMap, io::Write};

use crate::{
    model::{FuncDef, TypeSet},
    parser::{format_params, format_stmt, Expression, Statement},
    wasm_file::{CompileError, CompileResult},
    FuncImport, FuncType, Type,
};

macro_rules! dprintln {
    ($fmt:literal) => {
        if DEBUG.get() {
            println!($fmt);
        }
    };
    ($fmt:literal, $($args:expr),*) => {
        if DEBUG.get() {
            println!($fmt, $($args),*);
        }
    };
}

thread_local! {
    static DEBUG: std::cell::Cell<bool> = std::cell::Cell::new(false);
}

pub(crate) fn set_infer_debug(b: bool) {
    DEBUG.set(b);
}

#[derive(Debug, PartialEq)]
struct TypeInferFn {
    pub(crate) params: Vec<Type>,
    pub(crate) ret_ty: TypeSet,
}

fn get_type_infer_fns(
    stmt: &Statement,
    funcs: &mut HashMap<String, TypeInferFn>,
) -> Result<(), String> {
    match stmt {
        Statement::FnDecl(fn_decl) => {
            let mut buf = vec![];
            format_stmt(stmt, 0, &mut buf).map_err(|e| e.to_string())?;
            let ast_str = String::from_utf8(buf).map_err(|e| e.to_string())?;
            println!("before infer: {}", ast_str);
            funcs.insert(
                fn_decl.name.to_string(),
                TypeInferFn {
                    params: fn_decl
                        .params
                        .iter()
                        .map(|p| p.ty.determine().ok_or_else(|| "Parameter requires a type"))
                        .collect::<Result<_, _>>()?,
                    ret_ty: fn_decl.ret_ty,
                },
            );
        }
        _ => {}
    }
    Ok(())
}

struct TypeInferer<'a> {
    ret_ty: TypeSet,
    funcs: &'a HashMap<String, TypeInferFn>,
    locals: HashMap<String, TypeSet>,
}

impl<'a> TypeInferer<'a> {
    pub fn new(ret_ty: TypeSet, funcs: &'a HashMap<String, TypeInferFn>) -> Self {
        Self {
            ret_ty,
            funcs,
            locals: HashMap::new(),
        }
    }

    pub fn forward_type_expr(&mut self, ex: &Expression) -> Result<TypeSet, String> {
        match ex {
            Expression::LiteralInt(_, ts) => Ok(*ts),
            Expression::LiteralFloat(_, ts) => Ok(*ts),
            Expression::StrLiteral(_) => Ok(Type::Str.into()),
            Expression::Variable(name) => Ok(self
                .locals
                .get(*name)
                .copied()
                .unwrap_or(TypeSet::default())),
            Expression::FnInvoke(name, _) => self
                .funcs
                .get(*name)
                .map(|f| f.ret_ty)
                .ok_or_else(|| format!("Function {name} not found")),
            Expression::Cast(_ex, ty) => Ok((*ty).into()),
            Expression::Neg(ex) => self.forward_type_expr(ex),
            Expression::Add(lhs, rhs)
            | Expression::Sub(lhs, rhs)
            | Expression::Mul(lhs, rhs)
            | Expression::Div(lhs, rhs) => {
                let lhs_ty = self.forward_type_expr(lhs)?;
                let rhs_ty = self.forward_type_expr(rhs)?;
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
        }
    }

    fn propagate_type_expr(&mut self, ex: &mut Expression, ts: &TypeSet) -> Result<(), String> {
        match ex {
            Expression::LiteralInt(_, target_ts) => *target_ts = *ts,
            Expression::LiteralFloat(_, target_ts) => *target_ts = *ts,
            Expression::StrLiteral(_) => {}
            Expression::Variable(name) => {
                if let Some(var) = self.locals.get_mut(*name) {
                    dprintln!("propagate variable {name}: {var} -> {ts}");
                    if (*var & *ts).is_none() {
                        return Err(format!("Type inference failed to find intersection of type of variable {name}: {var} & {ts}"));
                    }
                    *var = *var & *ts;
                }
            }
            Expression::FnInvoke(name, args) => {
                let func = self
                    .funcs
                    .get(*name)
                    .ok_or_else(|| format!("Function not found: {}", *name))?;
                for (arg, ts) in args.iter_mut().zip(func.params.iter()) {
                    self.propagate_type_expr(arg, &(*ts).into())?;
                }
            }
            Expression::Cast(_ex, _ty) => {
                // Cast will not propagate type constraints
            }
            Expression::Neg(ex) => {
                self.propagate_type_expr(ex, ts)?;
            }
            Expression::Add(lhs, rhs)
            | Expression::Sub(lhs, rhs)
            | Expression::Mul(lhs, rhs)
            | Expression::Div(lhs, rhs) => {
                self.propagate_type_expr(lhs, ts)?;
                self.propagate_type_expr(rhs, ts)?;
            }
            Expression::Lt(lhs, rhs) | Expression::Gt(lhs, rhs) => {
                let lhs_ty = self.forward_type_expr(lhs)?.determine();
                let rhs_ty = self.forward_type_expr(rhs)?.determine();
                dprintln!("propagate_type_expr comp({lhs_ty:?}, {rhs_ty:?})");
                match (lhs_ty, rhs_ty) {
                    (Some(lhs_ts), None) => self.propagate_type_expr(rhs, &lhs_ts.into())?,
                    (None, Some(rhs_ts)) => self.propagate_type_expr(lhs, &rhs_ts.into())?,
                    // (Some(lhs), Some(rhs)) => {}
                    _ => {}
                }
            }
            Expression::Conditional(cond, t_branch, f_branch) => {
                self.propagate_type_expr(cond, &Type::I32.into())?;
                if let Some(t_stmt) = t_branch.last_mut() {
                    self.propagate_type_stmt(t_stmt, ts)?;
                }
                if let Some(f_stmt) = f_branch.as_mut().and_then(|stmts| stmts.last_mut()) {
                    self.propagate_type_stmt(f_stmt, ts)?;
                }
            }
        }
        Ok(())
    }

    fn propagate_type_stmt(&mut self, stmt: &mut Statement, ts: &TypeSet) -> Result<(), String> {
        match stmt {
            Statement::VarDecl(name, decl_ty, ex) => {
                if let Some(&var_ts) = self.locals.get(*name) {
                    dprintln!("propagate vardecl {}: {} -> {}", name, *decl_ty, var_ts);
                    self.propagate_type_expr(ex, &var_ts)?;
                    *decl_ty = var_ts;
                }
            }
            Statement::VarAssign(name, ex) => {
                let Some(&var_ts) = self.locals.get(*name) else {
                    return Err(format!("Assigned-to variable not declared: {name}"));
                };
                self.propagate_type_expr(ex, &var_ts)?;
            }
            Statement::Expr(ex) => self.propagate_type_expr(ex, ts)?,
            Statement::Brace(stmts) => {
                if let Some(stmt) = stmts.last_mut() {
                    self.propagate_type_stmt(stmt, ts)?;
                }
                for stmt in stmts.iter_mut().rev().skip(1) {
                    self.propagate_type_stmt(stmt, &Type::Void.into())?;
                }
            }
            Statement::FnDecl(fn_decl) => {
                for stmt in fn_decl.stmts.iter_mut().rev() {
                    self.propagate_type_stmt(stmt, &TypeSet::default())?;
                }
            }
            Statement::For(for_stmt) => {
                for stmt in for_stmt.stmts.iter_mut().rev() {
                    self.propagate_type_stmt(stmt, &TypeSet::default())?;
                }
                let Some(&idx_ty) = self.locals.get(for_stmt.name) else {
                    return Err(format!("Could not find variable {}", for_stmt.name));
                };
                dprintln!("propagate For {}: {}", for_stmt.name, idx_ty);
                self.propagate_type_expr(&mut for_stmt.start, &idx_ty)?;
                self.propagate_type_expr(&mut for_stmt.end, &idx_ty)?;
            }
            Statement::Return(ex) => {
                if let Some(ex) = ex {
                    let ret_ty = self.ret_ty;
                    self.propagate_type_expr(ex, &ret_ty)?;
                }
            }
        }
        Ok(())
    }

    fn forward_type_stmt(&mut self, stmt: &Statement) -> Result<TypeSet, String> {
        match stmt {
            Statement::Expr(ex) => self.forward_type_expr(ex),
            Statement::VarDecl(name, decl_ty, ex) => {
                if self.locals.get(*name).is_some() {
                    return Err(format!("Redeclaration of a variable {name}"));
                }
                let ex_ty = self.forward_type_expr(ex)?;
                let ty;
                if decl_ty != &TypeSet::default() {
                    ty = *decl_ty;
                } else {
                    ty = ex_ty;
                }
                dprintln!("forward vardecl {name}: {ty}");
                self.locals.insert(name.to_string(), ty);
                Ok(Type::Void.into())
            }
            Statement::VarAssign(name, ex) => {
                let Some(&decl_ty) = self.locals.get(*name) else {
                    return Err(format!("Assigned-to variable not declared: {name}"));
                };
                let ex_ty = self.forward_type_expr(ex)?;
                let ty = decl_ty & ex_ty;
                dprintln!("forward varassign {name}: {ty}");
                self.locals.insert(name.to_string(), ty);
                Ok(Type::Void.into())
            }
            Statement::Brace(stmts) => {
                let mut last_ty = TypeSet::default();
                for stmt in stmts {
                    last_ty = self.forward_type_stmt(stmt)?;
                }
                Ok(last_ty)
            }
            Statement::For(for_stmt) => {
                let start_ty = self.forward_type_expr(&for_stmt.start)?;
                let end_ty = self.forward_type_expr(&for_stmt.end)?;
                let index_ty = start_ty & end_ty;
                dprintln!("forward for {}: {}", for_stmt.name, index_ty);
                self.locals.insert(for_stmt.name.to_string(), index_ty);
                for stmt in &for_stmt.stmts {
                    self.forward_type_stmt(stmt)?;
                }
                Ok(Type::Void.into())
            }
            Statement::Return(ex) => ex
                .as_ref()
                .map_or(Ok(TypeSet::default()), |ex| self.forward_type_expr(ex)),
            _ => Ok(TypeSet::default()),
        }
    }

    pub fn infer_type_stmt(&mut self, stmt: &mut Statement) -> Result<(), String> {
        match stmt {
            Statement::VarDecl(name, ty, ex) => {
                let ex_ty = self.forward_type_expr(ex)?;
                let inferred_ty = ex_ty & TypeSet::from(*ty);
                if let Some(determined_ty) = inferred_ty.determine() {
                    // .ok_or_else(|| "Type could not be determined".to_string())?;
                    let determined_ts = TypeSet::from(determined_ty);
                    self.propagate_type_expr(ex, &determined_ts)?;
                }
                dprintln!("infer {name}: {inferred_ty}");
                self.locals.insert(name.to_string(), inferred_ty);
                Ok(())
            }
            Statement::FnDecl(fn_decl) => {
                dprintln!("Start type inference on fn {}", fn_decl.name);
                let mut inferer = TypeInferer::new(fn_decl.ret_ty, self.funcs);
                inferer.locals = fn_decl
                    .params
                    .iter()
                    .map(|param| (param.name.clone(), param.ty))
                    .collect();
                inferer.dump_locals();
                let mut last_ty = TypeSet::default();
                for stmt in &mut fn_decl.stmts {
                    last_ty = inferer.forward_type_stmt(stmt)?;
                    let mut bytes = vec![];
                    if let Some(s) = format_stmt(stmt, 0, &mut bytes)
                        .ok()
                        .and_then(|_| String::from_utf8(bytes).ok())
                    {
                        dprintln!("stmt ty {last_ty}: {}", s);
                    } else {
                        dprintln!("stmt ty {last_ty}");
                    }
                }
                dprintln!("FnDecl last_ty: {}", last_ty);
                inferer.dump_locals();
                if let Some(determined_ty) = (last_ty & fn_decl.ret_ty.into()).determine() {
                    let determined_ts = TypeSet::from(determined_ty);
                    fn_decl.ret_ty = determined_ts;
                    dprintln!(
                        "Function {}'s return type is inferred to be {}",
                        fn_decl.name,
                        determined_ty
                    );
                    for stmt in fn_decl.stmts.iter_mut().rev() {
                        inferer.propagate_type_stmt(stmt, &determined_ts)?;
                    }
                } else {
                    dprintln!(
                        "Function {}'s return type could not be determined: {}",
                        fn_decl.name,
                        last_ty & fn_decl.ret_ty.into()
                    );
                    fn_decl.ret_ty = Type::Void.into();
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn dump_locals(&self) {
        if !DEBUG.get() {
            return;
        }
        dprintln!("Dump of local types:");
        for (name, ty) in &self.locals {
            println!("  {name}: {ty}");
        }
    }
}

/// Run type inference engine. Since it modifies the type declaration in the AST,
/// `stmts` needs to be a mutable reference, but it won't add or remove statements, so it doesn't
/// have to be a `&mut Vec<Statement>`.
pub fn run_type_infer(
    stmts: &mut [Statement],
    types: &mut Vec<FuncType>,
    imports: &[FuncImport],
    funcs: &[FuncDef],
    typeinf_f: Option<&mut dyn Write>,
) -> CompileResult<()> {
    let mut type_infer_funcs = HashMap::new();
    for import_fn in imports {
        let func_ty = &types[import_fn.ty];
        type_infer_funcs.insert(
            import_fn.name.clone(),
            TypeInferFn {
                params: func_ty.params.clone(),
                ret_ty: func_ty
                    .results
                    .get(0)
                    .map_or(TypeSet::default(), |v| (*v).into()),
            },
        );
    }
    for func in funcs {
        let func_ty = &types[func.ty];
        type_infer_funcs.insert(
            func.name.clone(),
            TypeInferFn {
                params: func_ty.params.clone(),
                ret_ty: func_ty
                    .results
                    .get(0)
                    .map_or(TypeSet::VOID, |f| (*f).into()),
            },
        );
    }
    for stmt in stmts.iter_mut() {
        get_type_infer_fns(stmt, &mut type_infer_funcs).map_err(|e| CompileError::Compile(e))?;
    }

    for (i, stmt) in stmts.iter_mut().enumerate() {
        dprintln!("type inferring toplevel statement {i}");
        let mut inferer = TypeInferer::new(TypeSet::default(), &type_infer_funcs);
        inferer
            .infer_type_stmt(stmt)
            .map_err(|e| CompileError::Compile(e))?;
    }

    if let Some(typeinf_f) = typeinf_f {
        let mut formatted = vec![];
        for stmt in stmts {
            format_stmt(stmt, 0, &mut formatted)?;
        }
        if let Ok(formatted) = String::from_utf8(formatted) {
            write!(typeinf_f, "{}", formatted)?;
        }
    }

    Ok(())
}
