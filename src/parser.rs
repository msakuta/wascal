use crate::model::Type;

#[derive(Debug, PartialEq)]
pub enum Expression<'src> {
    LiteralI32(i32),
    LiteralF64(f64),
    Variable(&'src str),
    FnInvoke(&'src str, Vec<Expression<'src>>),
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
    VarDecl(&'src str, Type, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    Expr(Expression<'src>),
    FnDecl(FnDecl<'src>),
    For(For<'src>),
    Brace(Vec<Statement<'src>>),
    Return(Option<Expression<'src>>),
}

#[derive(Debug, PartialEq)]
pub struct FnDecl<'src> {
    pub(crate) name: &'src str,
    pub(crate) params: Vec<VarDecl>,
    pub(crate) stmts: Vec<Statement<'src>>,
    pub(crate) ret_ty: Type,
    pub(crate) public: bool,
}

/// Variable declaration with associated type.
/// Since WebAssembly is strict about type, we need to keep track of type for each slot
/// in function arguments and local variables.
#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub(crate) name: String,
    pub(crate) ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct For<'src> {
    pub(crate) name: &'src str,
    pub(crate) start: Expression<'src>,
    pub(crate) end: Expression<'src>,
    pub(crate) stmts: Vec<Statement<'src>>,
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
            Ok((input, Expression::LiteralF64(num)))
        } else {
            let num = slice.parse::<i32>().map_err(|s| s.to_string())?;
            Ok((input, Expression::LiteralI32(num)))
        }
    } else {
        Err("Not a number".to_string())
    }
}

#[test]
fn test_uneg() {
    assert!(matches!(
        num_literal("-2.5"),
        Ok(("", Expression::LiteralF64(-2.5)))
    ));
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

fn factor(i: &str) -> Result<(&str, Expression), String> {
    let r = space(i);
    if let Ok((r, val)) = num_literal(r) {
        return Ok((r, val));
    }

    if let Ok((r, _)) = recognize("(")(r) {
        let (r, ex) = expression(r)?;
        let (r, _) = recognize(")")(r)?;
        return Ok((r, ex));
    }

    let Ok((r, name)) = identifier(r) else {
        return Err("Factor is neither a numeric literal or an identifier".to_string());
    };

    if let Ok((r, ex)) = fn_call(name, r) {
        return Ok((r, ex));
    }

    return Ok((r, Expression::Variable(name)));
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
        (r, ty)
    } else {
        (r, Type::I32)
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
    dbg!(public, name);

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

        let (r, ret_ty) = fn_ret_ty(r).unwrap_or((r, Type::I32));

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

    let (r, ty) = decl_ty(r).unwrap_or((r, Type::I32));

    let Ok((r, _)) = recognize("=")(space(r)) else {
        return Err("Syntax error in var decl".to_string());
    };
    let (r, ex) = expression(r)?;
    if let Ok((r, _)) = recognize(";")(space(r)) {
        return Ok((r, Statement::VarDecl(name, ty, ex)));
    }
    return Ok((r, Statement::VarDecl(name, ty, ex)));
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
