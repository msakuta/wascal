use crate::model::{Type, TypeSet};

#[derive(Debug, PartialEq)]
pub enum Expression<'src> {
    LiteralInt(i64, TypeSet),
    LiteralFloat(f64, TypeSet),
    StrLiteral(String),
    Variable(&'src str),
    StructLiteral(&'src str, Vec<(&'src str, Expression<'src>)>),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    Cast(Box<Expression<'src>>, Type),
    FieldAccess(Box<Expression<'src>>, &'src str),
    Neg(Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Conditional(
        Box<Expression<'src>>,
        Vec<Statement<'src>>,
        Option<Vec<Statement<'src>>>,
    ),
}

#[derive(Debug, PartialEq)]
pub enum Statement<'src> {
    VarDecl(&'src str, TypeSet, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    Expr(Expression<'src>),
    FnDecl(FnDecl<'src>),
    For(For<'src>),
    Brace(Vec<Statement<'src>>),
    Return(Option<Expression<'src>>),
    Struct(StructDef<'src>),
}

#[derive(Debug, PartialEq)]
pub struct FnDecl<'src> {
    pub(crate) name: &'src str,
    pub(crate) params: Vec<VarDecl>,
    pub(crate) stmts: Vec<Statement<'src>>,
    pub(crate) ret_ty: TypeSet,
    pub(crate) public: bool,
}

/// Variable declaration with associated type.
/// Since WebAssembly is strict about type, we need to keep track of type for each slot
/// in function arguments and local variables.
#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub(crate) name: String,
    pub(crate) ty: TypeSet,
}

#[derive(Debug, PartialEq)]
pub struct For<'src> {
    pub(crate) name: &'src str,
    pub(crate) start: Expression<'src>,
    pub(crate) end: Expression<'src>,
    pub(crate) stmts: Vec<Statement<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField<'src> {
    pub(crate) name: &'src str,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef<'src> {
    pub(crate) name: &'src str,
    pub(crate) fields: Vec<StructField<'src>>,
}

type IResult<R, T> = Result<(R, T), String>;

fn num_literal(mut input: &str) -> Result<(&str, Expression), String> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '.' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(input.chars().next(), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        let slice = &start[..(input.as_ptr() as usize - start.as_ptr() as usize)];
        if slice.contains('.') {
            let num = slice.parse::<f64>().map_err(|s| s.to_string())?;
            Ok((
                input,
                Expression::LiteralFloat(num, TypeSet::f32() | TypeSet::f64()),
            ))
        } else {
            let num = slice.parse::<i64>().map_err(|s| s.to_string())?;
            Ok((
                input,
                Expression::LiteralInt(num, TypeSet::i32() | TypeSet::i64()),
            ))
        }
    } else {
        Err("Not a number".to_string())
    }
}

fn str_literal(i: &str) -> IResult<&str, Expression> {
    let (r0, _) = recognize("\"")(space(i))?;
    let mut r = r0;

    let mut escaped = false;
    let mut buf = String::new();
    loop {
        let Some(c) = peek_char(r) else {
            return Err("Unclosed string".to_string());
        };
        r = advance_char(r);
        if !escaped && c == '\"' {
            break;
        }
        if (c == '\\') ^ !escaped {
            buf.push(c);
        }
        escaped = c == '\\';
    }

    let val = &r0[..r.as_ptr() as usize - r0.as_ptr() as usize - 1];

    Ok((
        r,
        Expression::StrLiteral(val.to_string().replace("\\\\", "\\").replace("\\n", "\n")),
    ))
}

#[test]
fn test_uneg() {
    assert_eq!(
        num_literal("-2.5"),
        Ok((
            "",
            Expression::LiteralFloat(-2.5, TypeSet::f32() | TypeSet::f64())
        ))
    );
}

fn advance_char(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
    input.chars().next()
}

/// Matches zero or more spaces. Used for skipping potential spaces.
fn space(mut i: &str) -> &str {
    while peek_char(i).is_some_and(|c| c.is_whitespace()) {
        i = advance_char(i);
    }
    i
}

/// Matches one or more spaces. Used when a space is expected, e.g. after an identifier.
fn space1(mut i: &str) -> Result<&str, String> {
    let c = peek_char(i).ok_or_else(|| "Expected one or more spaces")?;
    if !c.is_whitespace() {
        return Err("Expected a whitespace".to_string());
    }
    i = advance_char(i);
    while peek_char(i).is_some_and(|c| c.is_whitespace()) {
        i = advance_char(i);
    }
    Ok(i)
}

fn identifier(mut input: &str) -> Result<(&str, &str), String> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z' | '_'))) {
        input = advance_char(input);
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
        ) {
            input = advance_char(input);
        }
        Ok((
            input,
            &start[..(input.as_ptr() as usize - start.as_ptr() as usize)],
        ))
    } else {
        Err("Invalid identifier".to_string())
    }
}

fn fn_call<'a>(name: &'a str, i: &'a str) -> IResult<&'a str, Expression<'a>> {
    let (mut r, _) = recognize("(")(space(i))?;

    let mut args = vec![];
    loop {
        let Ok((next_r, arg)) = expression(r) else {
            break;
        };
        args.push(arg);
        r = next_r;
        let Ok((next_r, _)) = recognize(",")(space(r)) else {
            break;
        };
        r = next_r;
    }

    let Ok((r, _)) = recognize(")")(r) else {
        return Err("FnInvoke is not closed".to_string());
    };
    return Ok((r, Expression::FnInvoke(name, args)));
}

#[test]
fn test_fn_call() {
    let source = "f_x(x, y)";
    let ast = expression(source);
    assert_eq!(
        ast,
        Ok((
            "",
            Expression::FnInvoke(
                "f_x",
                vec![Expression::Variable("x"), Expression::Variable("y")]
            )
        ))
    );
}

fn struct_literal<'a>(name: &'a str, i: &'a str) -> IResult<&'a str, Expression<'a>> {
    let (mut r, _) = recognize("{")(space(i))?;

    let mut fields = vec![];
    loop {
        let Ok((next_r, fname)) = identifier(space(r)) else {
            break;
        };
        let (next_r, _) = recognize(":")(space(next_r))?;
        let (next_r, initializer) = expression(next_r)?;
        fields.push((fname, initializer));
        r = next_r;
        let Ok((next_r, _)) = recognize(",")(space(r)) else {
            break;
        };
        r = next_r;
    }

    let Ok((r, _)) = recognize("}")(r) else {
        return Err("FnInvoke is not closed".to_string());
    };
    return Ok((r, Expression::StructLiteral(name, fields)));
}

fn postfix_as<'a>(i: &'a str) -> IResult<&'a str, Type> {
    let (r, _) = space1(i).and_then(|r| recognize("as")(r))?;
    let (r, ty) = identifier(space1(r)?)?;
    Ok((r, Type::try_from(ty)?))
}

fn postfix_dot<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
    let (r, _) = recognize(".")(space(i))?;
    dbg!(r);
    let (r, field) = identifier(space(r))?;
    Ok((r, field))
}

fn postfix_expr<'a>(expr: Expression<'a>, i: &'a str) -> IResult<&'a str, Expression<'a>> {
    if let Ok((r, p_as)) = postfix_as(i) {
        return Ok((r, Expression::Cast(Box::new(expr), p_as)));
    }

    if let Ok((r, p_dot)) = postfix_dot(i) {
        return Ok((r, Expression::FieldAccess(Box::new(expr), p_dot)));
    }

    Ok((i, expr))
}

fn factor(i: &str) -> Result<(&str, Expression), String> {
    let r = space(i);

    if let Ok((r, val)) = num_literal(r) {
        return Ok((r, val));
    }

    if let Ok((r, _)) = recognize("-")(r) {
        let (r, val) = factor(r)?;
        return Ok((r, Expression::Neg(Box::new(val))));
    }

    if let Ok((r, str)) = str_literal(r) {
        return Ok((r, str));
    }

    if let Ok((r, _)) = recognize("(")(r) {
        let (r, ex) = expression(r)?;
        let (r, _) = recognize(")")(r)?;
        return postfix_expr(ex, r);
    }

    let Ok((r, name)) = identifier(r) else {
        return Err("Factor is neither a numeric literal or an identifier".to_string());
    };

    if let Ok((r, ex)) = struct_literal(name, r) {
        return postfix_expr(ex, r);
    }

    if let Ok((r, ex)) = fn_call(name, r) {
        return postfix_expr(ex, r);
    }

    postfix_expr(Expression::Variable(name), r)
}

fn mul(i: &str) -> Result<(&str, Expression), String> {
    let (r, mut lhs) = factor(i)?;

    let mut r = space(r);

    while 1 <= r.len() && matches!(&r[..1], "*" | "/") {
        let op = &r[..1];
        let (next_r, rhs) = factor(&r[1..])?;
        r = space(next_r);
        lhs = if op == "*" {
            Expression::Mul(Box::new(lhs), Box::new(rhs))
        } else {
            Expression::Div(Box::new(lhs), Box::new(rhs))
        };
    }
    Ok((r, lhs))
}

fn add(i: &str) -> Result<(&str, Expression), String> {
    let (r, mut lhs) = mul(i)?;

    let mut r = space(r);

    while 1 <= r.len() && matches!(&r[..1], "+" | "-") {
        let op = &r[..1];
        let (next_r, rhs) = mul(&r[1..])?;
        r = space(next_r);
        lhs = if op == "+" {
            Expression::Add(Box::new(lhs), Box::new(rhs))
        } else {
            Expression::Sub(Box::new(lhs), Box::new(rhs))
        };
    }
    Ok((r, lhs))
}

fn _parse_params(i: &str) -> Result<(&str, Vec<&str>), String> {
    let mut ret = vec![];
    let mut r = i;

    loop {
        r = space(r);

        if &r[..2] == "=>" {
            r = &r[2..];
            break;
        }

        let Ok((next_r, name)) = identifier(r) else {
            return Err("Parameter list contains non-identifier".to_string());
        };

        r = next_r;

        ret.push(name);
    }

    Ok((r, ret))
}

fn recognize<'a>(s: &'a str) -> impl FnOnce(&'a str) -> Result<(&'a str, &'a str), String> {
    move |i| {
        if i.len() < s.len() || &i[..s.len()] != s {
            return Err(format!("Does not match {s}"));
        }

        Ok((&i[s.len()..], s))
    }
}

fn cmp_expr(i: &str) -> IResult<&str, Expression> {
    let (r, lhs) = add(i)?;

    let Ok((r, op)) = recognize("<")(space(r)).or_else(|_| recognize(">")(space(r))) else {
        return Ok((r, lhs));
    };

    let (r, rhs) = add(r)?;

    Ok((
        r,
        if op == "<" {
            Expression::Lt(Box::new(lhs), Box::new(rhs))
        } else {
            Expression::Gt(Box::new(lhs), Box::new(rhs))
        },
    ))
}

fn else_clause(i: &str) -> IResult<&str, Vec<Statement>> {
    let (r, _) = recognize("else")(space(i))?;

    if let Ok((r, next_cond)) = conditional(r) {
        return Ok((r, vec![Statement::Expr(next_cond)]));
    }

    let (r, _) = recognize("{")(space(r))?;
    let (r, stmts) = statements(r)?;
    let (r, _) = recognize("}")(space(r))?;
    Ok((r, stmts))
}

fn conditional(i: &str) -> IResult<&str, Expression> {
    let (r, _) = recognize("if")(space(i))?;

    let (r, ex) = cmp_expr(r)?;

    let (r, _) = recognize("{")(space(r))?;

    let (r, stmts) = statements(r)?;

    let (r, _) = recognize("}")(space(r))?;
    let (r, else_res) = if let Ok((r, res)) = else_clause(r) {
        (r, Some(res))
    } else {
        (r, None)
    };

    Ok((r, Expression::Conditional(Box::new(ex), stmts, else_res)))
}

fn expression(i: &str) -> IResult<&str, Expression> {
    if let Ok((r, res)) = conditional(i) {
        return Ok((r, res));
    }

    if let Ok((r, res)) = cmp_expr(i) {
        return Ok((r, res));
    }

    Err("Expression failed to parse".to_string())
}

fn for_stmt(i: &str) -> IResult<&str, Statement> {
    let (r, _) = recognize("for")(space(i))?;
    let (r, name) = identifier(space(r))?;
    let (r, _) = recognize("in")(space(r))?;
    let (r, start) = expression(space(r))?;
    let (r, _) = recognize("to")(space(r))?;
    let (r, end) = expression(space(r))?;
    let (r, _) = recognize("{")(space(r))?;
    let (r, stmts) = statements(space(r))?;
    let (r, _) = recognize("}")(space(r))?;
    Ok((
        r,
        Statement::For(For {
            name,
            start,
            end,
            stmts,
        }),
    ))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
    let (r, name) = identifier(space(i))?;
    let (r, _) = recognize("=")(space(r))?;
    let (r, ex) = expression(space(r))?;
    if let Ok((r, _)) = recognize(";")(space(r)) {
        return Ok((r, Statement::VarAssign(name, ex)));
    }
    Ok((r, Statement::VarAssign(name, ex)))
}

fn decl_ty(i: &str) -> IResult<&str, Type> {
    let (r, _) = recognize(":")(space(i))?;
    let (r, ty) = identifier(space(r))?;
    Ok((r, ty.try_into()?))
}

fn fn_param(i: &str) -> IResult<&str, VarDecl> {
    let (r, param_name) = identifier(space(i))?;
    let (r, ty) = if let Ok((r, ty)) = decl_ty(space(r)) {
        (r, ty.into())
    } else {
        (r, TypeSet::all())
    };
    Ok((
        r,
        VarDecl {
            name: param_name.to_string(),
            ty,
        },
    ))
}

fn fn_ret_ty(i: &str) -> IResult<&str, Type> {
    let (r, _) = recognize("->")(space(i))?;
    let (r, ty) = identifier(space(r))?;
    Ok((r, ty.try_into()?))
}

fn let_binding(i: &str) -> IResult<&str, Statement> {
    let public;
    let r = if let Ok((r, _)) = recognize("pub")(space(i)) {
        public = true;
        space1(r)?
    } else {
        public = false;
        space(i)
    };
    let (r, _) = recognize("let")(r)?;
    let (r, name) = identifier(space1(r)?)?;

    if let Ok((mut r, _)) = recognize("(")(space(r)) {
        let mut params = vec![];

        loop {
            let Ok((next_r, param)) = fn_param(r) else {
                break;
            };
            params.push(param);
            r = next_r;
            let Ok((next_r, _)) = recognize(",")(space(r)) else {
                break;
            };
            r = next_r;
        }

        let Ok((r, _)) = recognize(")")(space(r)) else {
            return Err(
                "Syntax error in func decl: closing parenthesis could not be found".to_string(),
            );
        };

        let (r, ret_ty) =
            fn_ret_ty(r).map_or((r, TypeSet::all()), |(r, ty)| (r, TypeSet::from(ty)));

        let Ok((r, _)) = recognize("=")(space(r)) else {
            return Err("Syntax error in func decl: = could not be found".to_string());
        };

        let (r, stmts) = statement(r)?;

        return Ok((
            r,
            Statement::FnDecl(FnDecl {
                name,
                params,
                stmts: vec![stmts],
                ret_ty,
                public,
            }),
        ));
    }

    let (r, ty) = decl_ty(r).map_or((r, TypeSet::all()), |(r, ty)| (r, ty.into()));

    let Ok((r, _)) = recognize("=")(space(r)) else {
        return Err("Syntax error in var decl".to_string());
    };
    let (r, ex) = expression(r)?;
    if let Ok((r, _)) = recognize(";")(space(r)) {
        return Ok((r, Statement::VarDecl(name, ty, ex)));
    }
    return Ok((r, Statement::VarDecl(name, ty, ex)));
}

/// The difference from [`fn_param`] is that the type is not optional.
fn struct_field(i: &str) -> IResult<&str, StructField> {
    let (r, field_name) = identifier(space(i))?;
    dbg!(field_name);
    let (r, ty) = decl_ty(space(r))?;
    Ok((
        r,
        StructField {
            name: field_name,
            ty,
        },
    ))
}

fn struct_def(i: &str) -> IResult<&str, Statement> {
    let (r, "struct") = identifier(space(i))? else {
        return Err("Ident `struct` expected".to_string());
    };

    let (r, name) = identifier(space(r))?;

    dbg!(name);

    let (mut r, _) = recognize("{")(space(r))?;

    let mut fields = vec![];
    while let Ok((next_r, field)) = struct_field(r) {
        fields.push(field);
        let Ok((next_r, _)) = recognize(",")(space(next_r)) else {
            break;
        };
        r = next_r;
    }

    let (r, _) = recognize("}")(space(r))?;

    Ok((r, Statement::Struct(StructDef { name, fields })))
}

fn statement(i: &str) -> Result<(&str, Statement), String> {
    let r = space(i);

    if let Ok((r, stmts)) = brace_statement(r) {
        return Ok((r, Statement::Brace(stmts)));
    }

    if let Ok((r, stmt)) = for_stmt(r) {
        return Ok((r, stmt));
    }

    if let Ok((r, "return")) = identifier(space(r)) {
        let (r, ex) = expression(r).map_or((r, None), |(r, ex)| (r, Some(ex)));
        let (r, _) = recognize(";")(space(r)).unwrap_or((r, ""));
        return Ok((r, Statement::Return(ex)));
    }

    if let Ok((r, stdef)) = struct_def(space(r)) {
        return Ok((r, stdef));
    }

    if let Ok(res) = let_binding(r) {
        return Ok(res);
    }

    if let Ok((r, stmt)) = var_assign(r) {
        return Ok((r, stmt));
    }

    let (r, res) = expression(r)?;

    if let Ok((r, _)) = recognize(";")(space(r)) {
        return Ok((r, Statement::Expr(res)));
    }
    return Ok((r, Statement::Expr(res)));
}

fn brace_statement(i: &str) -> IResult<&str, Vec<Statement>> {
    let (r, _) = recognize("{")(space(i))?;
    let (r, stmts) = statements(r)?;
    let (r, _) = recognize("}")(space(r))?;

    Ok((r, stmts))
}

fn statements(mut r: &str) -> Result<(&str, Vec<Statement>), String> {
    let mut stmts = vec![];
    while let Ok(res) = statement(r) {
        r = res.0;
        stmts.push(res.1);
    }

    Ok((r, stmts))
}

pub fn parse(i: &str) -> Result<Vec<Statement>, String> {
    let mut r = i;

    let mut stmts = vec![];
    while let Ok(res) = statement(r) {
        r = res.0;
        stmts.push(res.1);
    }

    let r = space(r);

    if !r.is_empty() {
        return Err(format!("Input terminated {r:?}"));
    }

    Ok(stmts)
}

pub fn format_expr(
    ex: &Expression,
    level: usize,
    f: &mut impl std::io::Write,
) -> std::io::Result<()> {
    match ex {
        Expression::LiteralFloat(num, ts) => write!(f, "{num}: {ts}"),
        Expression::LiteralInt(num, ts) => write!(f, "{num}: {ts}"),
        Expression::StrLiteral(s) => write!(f, "\"{s}\""), // TODO: escape
        Expression::Variable(name) => write!(f, "{name}"),
        Expression::StructLiteral(name, fields) => {
            let indent = "  ".repeat(level);
            writeln!(f, "{name} {{")?;
            for field in fields {
                write!(f, "{indent}  {}: ", field.0)?;
                format_expr(&field.1, level, f)?;
                writeln!(f, ",")?;
            }
            writeln!(f, "{indent}}}")
        }
        Expression::FnInvoke(fname, args) => {
            write!(f, "{fname}(")?;
            for (i, arg) in args.iter().enumerate() {
                format_expr(arg, level, f)?;
                if i != args.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")?;
            Ok(())
        }
        Expression::Cast(ex, ty) => {
            format_expr(ex, level, f)?;
            write!(f, " as {ty}")?;
            Ok(())
        }
        Expression::FieldAccess(ex, field) => {
            format_expr(ex, level, f)?;
            write!(f, ".{field}")?;
            Ok(())
        }
        Expression::Neg(ex) => {
            write!(f, "-")?;
            format_expr(ex, level, f)?;
            Ok(())
        }
        Expression::Add(lhs, rhs) => format_bin_op('+', lhs, rhs, level, f),
        Expression::Sub(lhs, rhs) => format_bin_op('-', lhs, rhs, level, f),
        Expression::Mul(lhs, rhs) => format_bin_op('*', lhs, rhs, level, f),
        Expression::Div(lhs, rhs) => format_bin_op('/', lhs, rhs, level, f),
        Expression::Lt(lhs, rhs) => format_bin_op('<', lhs, rhs, level, f),
        Expression::Gt(lhs, rhs) => format_bin_op('>', lhs, rhs, level, f),
        Expression::Conditional(cond, t_branch, f_branch) => {
            let indent = "  ".repeat(level);
            write!(f, "if ")?;
            format_expr(cond, level, f)?;
            writeln!(f, " {{")?;
            for stmt in t_branch {
                format_stmt(stmt, level + 1, f)?;
            }
            write!(f, "{indent}}}")?;
            if let Some(stmts) = f_branch {
                writeln!(f, " else {{")?;
                for stmt in stmts {
                    format_stmt(stmt, level + 1, f)?;
                }
                write!(f, "{indent}}}")?;
            }
            Ok(())
        }
    }
}

fn format_bin_op(
    op: char,
    lhs: &Expression,
    rhs: &Expression,
    level: usize,
    f: &mut impl std::io::Write,
) -> std::io::Result<()> {
    write!(f, "(")?;
    format_expr(lhs, level, f)?;
    write!(f, " {op} ")?;
    format_expr(rhs, level, f)?;
    write!(f, ")")?;
    Ok(())
}

pub fn format_stmt(
    stmt: &Statement,
    level: usize,
    f: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let indent = "  ".repeat(level);
    match stmt {
        Statement::VarDecl(name, ty, init) => {
            write!(f, "{indent}let {name}: {ty} = ")?;
            format_expr(init, level, f)?;
            writeln!(f, ";")
        }
        Statement::VarAssign(name, ex) => {
            write!(f, "{indent}{name} = ")?;
            format_expr(ex, level, f)?;
            writeln!(f, ";")
        }
        Statement::Expr(ex) => {
            write!(f, "{indent}")?;
            format_expr(ex, level, f)?;
            writeln!(f, ";")
        }
        Statement::Brace(stmts) => {
            writeln!(f, "{indent}{{")?;
            for stmt in stmts {
                format_stmt(stmt, level + 1, f)?;
            }
            writeln!(f, "}}")
        }
        Statement::FnDecl(func) => {
            let public = if func.public { "pub " } else { "" };
            write!(f, "{public}let {}(", func.name)?;
            format_params(&func.params, f)?;
            write!(f, ") -> {} =", func.ret_ty)?;
            for stmt in &func.stmts {
                format_stmt(stmt, level + 1, f)?;
            }
            writeln!(f, "")?;
            Ok(())
        }
        Statement::For(for_stmt) => {
            write!(f, "{indent}for {} in ", for_stmt.name)?;
            format_expr(&for_stmt.start, level, f)?;
            write!(f, " to ")?;
            format_expr(&for_stmt.end, level, f)?;
            writeln!(f, " {{")?;
            for stmt in &for_stmt.stmts {
                format_stmt(stmt, level + 1, f)?;
            }
            writeln!(f, "{indent}}}")?;
            Ok(())
        }
        Statement::Return(ex) => {
            write!(f, "{indent}return ")?;
            if let Some(ex) = ex {
                format_expr(ex, level, f)?;
            }
            writeln!(f, ";")
        }
        Statement::Struct(stdef) => {
            writeln!(f, "{indent}struct {} {{", stdef.name)?;
            for field in &stdef.fields {
                writeln!(f, "{indent}  {}: {},", field.name, field.ty)?;
            }
            writeln!(f, "{indent}}}")
        }
    }
}

pub fn format_params(params: &[VarDecl], f: &mut impl std::io::Write) -> std::io::Result<()> {
    for (i, param) in params.iter().enumerate() {
        write!(f, "{}: {}", param.name, param.ty)?;
        if i != params.len() - 1 {
            write!(f, ", ")?;
        }
    }
    Ok(())
}
