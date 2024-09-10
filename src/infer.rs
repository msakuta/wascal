//! Implementation of type inference using TypeSet

use std::{collections::HashMap, io::Write};

use crate::{
    model::{FuncDef, StructDef, TypeSet},
    parser::{format_expr, format_stmt, Expression, Statement},
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
                    ret_ty: fn_decl.ret_ty.clone(),
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
    structs: &'a HashMap<String, StructDef>,
}

impl<'a> TypeInferer<'a> {
    pub fn new(
        ret_ty: TypeSet,
        funcs: &'a HashMap<String, TypeInferFn>,
        structs: &'a HashMap<String, StructDef>,
    ) -> Self {
        Self {
            ret_ty,
            funcs,
            locals: HashMap::new(),
            structs,
        }
    }

    pub fn forward_type_expr(&mut self, ex: &Expression) -> Result<TypeSet, String> {
        match ex {
            Expression::LiteralInt(_, ts) => Ok(ts.clone()),
            Expression::LiteralFloat(_, ts) => Ok(ts.clone()),
            Expression::StrLiteral(_) => Ok(Type::Str.into()),
            Expression::StructLiteral(name, _) => {
                if self.structs.get(*name).is_none() {
                    return Err(format!("Struct {name} was not found"));
                }
                Ok(Type::Struct(name.to_string()).into())
            }
            Expression::Variable(name) => Ok(self
                .locals
                .get(*name)
                .cloned()
                .ok_or_else(|| format!("Variable {name} not found"))?),
            Expression::FnInvoke(name, _) => self
                .funcs
                .get(*name)
                .map(|f| f.ret_ty.clone())
                .ok_or_else(|| format!("Function {name} not found")),
            Expression::Cast(_ex, ty) => Ok(ty.clone().into()),
            Expression::FieldAccess(ex, fname) => {
                let prefix_ty = self
                    .forward_type_expr(ex)?
                    .determine()
                    .ok_or_else(|| "Struct could not be determined".to_string())?;
                if let Type::Struct(stname) = prefix_ty {
                    let Some(stdef) = self.structs.get(&stname) else {
                        return Err(format!("Struct {stname} not defined"));
                    };

                    let Some(stfield) = stdef.fields.iter().find(|field| &field.name == fname)
                    else {
                        return Err(format!("Struct field {fname} not found"));
                    };

                    Ok(stfield.ty.clone().into())
                } else {
                    Err("Field access operator (.) is applied to a non-struct".to_string())
                }
            }
            Expression::Neg(ex) => self.forward_type_expr(ex),
            Expression::Add(lhs, rhs)
            | Expression::Sub(lhs, rhs)
            | Expression::Mul(lhs, rhs)
            | Expression::Div(lhs, rhs) => {
                let lhs_ty = self.forward_type_expr(lhs)?;
                let rhs_ty = self.forward_type_expr(rhs)?;
                Ok(lhs_ty & rhs_ty)
            }
            Expression::Lt(_, _)
            | Expression::Gt(_, _)
            | Expression::And(_, _)
            | Expression::Or(_, _) => {
                // Relation and logical operators always return i32 (boolean)
                Ok(Type::I32.into())
            }
            Expression::Conditional(_, t_branch, f_branch) => {
                let stmts_to_string =
                    |stmts: &[Statement]| -> Result<String, Box<dyn std::error::Error>> {
                        let mut buf = vec![];
                        for stmt in stmts {
                            format_stmt(stmt, 0, &mut buf)?;
                            writeln!(&mut buf, "")?;
                        }
                        Ok(String::from_utf8(buf)?.replace("\n", " "))
                    };

                let t_ty = self.forward_type_stmts(t_branch)?;
                let f_ty = f_branch.as_ref().map_or(Ok(TypeSet::default()), |stmts| {
                    self.forward_type_stmts(stmts)
                })?;
                dprintln!(
                    "forward conditional: {} & {} = {} <: ({}) & ({})",
                    &t_ty,
                    &f_ty,
                    &t_ty & &f_ty,
                    stmts_to_string(t_branch).unwrap(),
                    f_branch
                        .as_ref()
                        .map(|f| stmts_to_string(f).unwrap())
                        .unwrap_or_else(|| "?".to_string())
                );
                Ok(t_ty & f_ty)
            }
        }
    }

    fn propagate_type_expr(&mut self, ex: &mut Expression, ts: &TypeSet) -> Result<(), String> {
        match ex {
            Expression::LiteralInt(_, target_ts) => *target_ts = ts.clone(),
            Expression::LiteralFloat(_, target_ts) => *target_ts = ts.clone(),
            Expression::StrLiteral(_) => {}
            Expression::StructLiteral(name, fields) => {
                if let Some(stdef) = self.structs.get(*name) {
                    for ((_, ex), stfield) in fields.iter_mut().zip(stdef.fields.iter()) {
                        self.propagate_type_expr(ex, &stfield.ty.clone().into())?;
                    }
                } else {
                    return Err(format!("Undefined struct {name}"));
                }
            }
            Expression::Variable(name) => {
                if let Some(var) = self.locals.get_mut(*name) {
                    dprintln!("propagate variable {name}: {var} -> {ts}");
                    let im_var: &TypeSet = var;
                    let intersection = im_var & ts;
                    if intersection.is_none() {
                        return Err(format!("Type inference failed to find intersection of type of variable {name}: {var} & {ts}"));
                    }
                    *var = intersection;
                }
            }
            Expression::FnInvoke(name, args) => {
                let func = self
                    .funcs
                    .get(*name)
                    .ok_or_else(|| format!("Function not found: {}", *name))?;
                for (arg, ts) in args.iter_mut().zip(func.params.iter()) {
                    self.propagate_type_expr(arg, &ts.clone().into())?;
                }
            }
            Expression::Cast(ex, _ty) => {
                // Cast will not propagate type constraints
                self.propagate_type_expr(ex, &TypeSet::all())?;
            }
            Expression::FieldAccess(_ex, _ty) => {
                // Field access will not propagate type constraints because struct types should be determinate
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
            Expression::And(lhs, rhs) | Expression::Or(lhs, rhs) => {
                self.propagate_type_expr(lhs, &Type::I32.into())?;
                self.propagate_type_expr(rhs, &Type::I32.into())?;
            }
            Expression::Conditional(cond, t_branch, f_branch) => {
                self.propagate_type_expr(cond, &Type::I32.into())?;
                for stmt in t_branch.iter_mut().rev() {
                    self.propagate_type_stmt(stmt, ts)?;
                }
                if let Some(f_stmts) = f_branch.as_mut() {
                    for stmt in f_stmts.iter_mut().rev() {
                        self.propagate_type_stmt(stmt, ts)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn propagate_type_stmt(&mut self, stmt: &mut Statement, ts: &TypeSet) -> Result<(), String> {
        match stmt {
            Statement::VarDecl(name, decl_ty, ex) => {
                if let Some(var_ts) = self.locals.get(*name) {
                    dprintln!("propagate vardecl {}: {} -> {}", name, *decl_ty, var_ts);
                    let var_ts = var_ts.clone();
                    self.propagate_type_expr(ex, &var_ts)?;
                    *decl_ty = var_ts;
                }
            }
            Statement::VarAssign(lhs, ex) => {
                let var_ts = self.forward_type_expr(lhs)?;
                self.propagate_type_expr(ex, &var_ts.clone())?;
            }
            Statement::Expr(ex, false) => self.propagate_type_expr(ex, ts)?,
            Statement::Expr(ex, true) => self.propagate_type_expr(ex, &TypeSet::void())?,
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
                let Some(idx_ty) = self.locals.get(for_stmt.name) else {
                    return Err(format!("Could not find variable {}", for_stmt.name));
                };
                dprintln!("propagate For {}: {}", for_stmt.name, idx_ty);
                let idx_ty = idx_ty.clone();
                self.propagate_type_expr(&mut for_stmt.start, &idx_ty)?;
                self.propagate_type_expr(&mut for_stmt.end, &idx_ty)?;
            }
            Statement::Return(ex) => {
                if let Some(ex) = ex {
                    self.propagate_type_expr(ex, &self.ret_ty.clone())?;
                } else {
                    println!("return without expression!!!!!!!");
                }
            }
            Statement::Struct(_) => {}
        }
        Ok(())
    }

    fn forward_lvalue(&mut self, ast: &Expression) -> Result<Option<(String, TypeSet)>, String> {
        match ast {
            Expression::LiteralInt(_, _) => Err("Literal int cannot be a lvalue".to_string()),
            Expression::LiteralFloat(_, _) => Err("Literal float cannot be a lvalue".to_string()),
            Expression::StrLiteral(_) => Err("Literal string cannot be a lvalue".to_string()),
            Expression::StructLiteral(_, _) => Err("Literal struct cannot be a lvalue".to_string()),
            Expression::Variable(name) => {
                let ts = self
                    .locals
                    .get(*name)
                    .ok_or_else(|| format!("Variable {name} not found"))?;
                Ok(Some((name.to_string(), ts.clone())))
            }
            Expression::FnInvoke(_, _) => {
                Err("Function return value cannot be a lvalue".to_string())
            }
            Expression::Cast(_, _) => Err("Cast expression cannot be a lvalue".to_string()),
            Expression::FieldAccess(_, _) => {
                // Struct fields have fixed types, so we do not need to propagate type inference.
                Ok(None)
            }
            Expression::Neg(_)
            | Expression::Add(_, _)
            | Expression::Sub(_, _)
            | Expression::Mul(_, _)
            | Expression::Div(_, _)
            | Expression::Lt(_, _)
            | Expression::Gt(_, _)
            | Expression::And(_, _)
            | Expression::Or(_, _)
            | Expression::Conditional(_, _, _) => {
                Err("Arithmetic expression cannot be a lvalue".to_string())
            }
        }
    }

    fn forward_type_stmts(&mut self, stmts: &[Statement]) -> Result<TypeSet, String> {
        let mut last = TypeSet::void();
        for stmt in stmts {
            last = self.forward_type_stmt(stmt)?;
        }
        Ok(last)
    }

    fn forward_type_stmt(&mut self, stmt: &Statement) -> Result<TypeSet, String> {
        match stmt {
            Statement::Expr(ex, false) => self.forward_type_expr(ex),
            Statement::Expr(ex, true) => {
                self.forward_type_expr(ex)?;
                Ok(TypeSet::void())
            }
            Statement::VarDecl(name, decl_ty, ex) => {
                if self.locals.get(*name).is_some() {
                    return Err(format!("Redeclaration of a variable {name}"));
                }
                let ex_ty = self.forward_type_expr(ex)?;
                let ty;
                if decl_ty != &TypeSet::all() {
                    ty = decl_ty.clone();
                } else {
                    ty = ex_ty;
                }
                dprintln!("forward vardecl {name}: {ty}");
                self.locals.insert(name.to_string(), ty);
                Ok(Type::Void.into())
            }
            Statement::VarAssign(lhs, ex) => {
                if let Some((name, decl_ty)) = self.forward_lvalue(lhs)? {
                    let ex_ty = self.forward_type_expr(ex)?;
                    let ty = decl_ty & ex_ty;
                    dprintln!("forward varassign {ty}");
                    self.locals.insert(name, ty);
                }
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
            Statement::Return(ex) => {
                // TODO: constrain the result type with return statements
                ex.as_ref()
                    .map_or(Ok(TypeSet::default()), |ex| self.forward_type_expr(ex))?;

                // As a statement, it should not yield any value.
                Ok(TypeSet::void())
            }
            _ => Ok(TypeSet::default()),
        }
    }

    pub fn infer_type_stmt(&mut self, stmt: &mut Statement) -> Result<(), String> {
        match stmt {
            Statement::VarDecl(name, ty, ex) => {
                let ex_ty = self.forward_type_expr(ex)?;
                let inferred_ty = ex_ty & TypeSet::from(ty.clone());
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
                let mut inferer =
                    TypeInferer::new(fn_decl.ret_ty.clone(), self.funcs, self.structs);
                inferer.locals = fn_decl
                    .params
                    .iter()
                    .map(|param| (param.name.clone(), param.ty.clone()))
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
                let intersection = &last_ty & &fn_decl.ret_ty.clone().into();
                if let Some(determined_ty) = intersection.determine() {
                    let determined_ts = TypeSet::from(determined_ty);
                    fn_decl.ret_ty = determined_ts;
                    dprintln!(
                        "Function {}'s return type is inferred to be {}",
                        fn_decl.name,
                        fn_decl.ret_ty
                    );
                    for stmt in fn_decl.stmts.iter_mut().rev() {
                        inferer.propagate_type_stmt(stmt, &fn_decl.ret_ty)?;
                    }
                } else if fn_decl.ret_ty == TypeSet::void() {
                    dprintln!("Function {} returns void; coercing", fn_decl.name);
                    for stmt in fn_decl.stmts.iter_mut().rev() {
                        inferer.propagate_type_stmt(stmt, &fn_decl.ret_ty)?;
                    }
                } else {
                    dprintln!(
                        "Function {}'s return type could not be determined: {}",
                        fn_decl.name,
                        last_ty & fn_decl.ret_ty.clone().into()
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
pub fn run_type_infer<'src>(
    stmts: &mut [Statement],
    types: &mut Vec<FuncType>,
    imports: &[FuncImport],
    funcs: &[FuncDef],
    structs: &HashMap<String, StructDef>,
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
                    .map_or_else(TypeSet::default, |v| v.clone().into()),
            },
        );
    }
    for func in funcs {
        type_infer_funcs.insert(
            func.name.clone(),
            TypeInferFn {
                params: func.locals[..func.args]
                    .iter()
                    .map(|arg| arg.ty.clone().determine())
                    .collect::<Option<_>>()
                    .ok_or_else(|| {
                        CompileError::Compile(
                            "Function parameter types could not be determine".to_string(),
                        )
                    })?,
                ret_ty: func.ret_ty.clone().into(),
            },
        );
    }
    for stmt in stmts.iter_mut() {
        get_type_infer_fns(stmt, &mut type_infer_funcs).map_err(|e| CompileError::Compile(e))?;
    }

    for (i, stmt) in stmts.iter_mut().enumerate() {
        dprintln!("type inferring toplevel statement {i}");
        let mut inferer = TypeInferer::new(TypeSet::default(), &type_infer_funcs, structs);
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
