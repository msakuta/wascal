#[derive(Debug)]
pub enum Expression<'src> {
    Literal(i32),
    Variable(&'src str),
    FnInvoke(&'src str, Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    Conditional(
        Box<Expression<'src>>,
        Vec<Statement<'src>>,
        Option<Vec<Statement<'src>>>,
    ),
}

#[derive(Debug)]
pub enum Statement<'src> {
    VarDecl(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    Expr(Expression<'src>),
    FnDecl(FnDecl<'src>),
    For(For<'src>),
    Brace(Vec<Statement<'src>>),
}

#[derive(Debug)]
pub struct FnDecl<'src> {
    pub(crate) name: &'src str,
    pub(crate) params: Vec<&'src str>,
    pub(crate) stmts: Vec<Statement<'src>>,
}

#[derive(Debug)]
pub struct For<'src> {
    pub(crate) name: &'src str,
    pub(crate) start: Expression<'src>,
    pub(crate) end: Expression<'src>,
    pub(crate) stmts: Vec<Statement<'src>>,
}

type IResult<R, T> = Result<(R, T), String>;

fn num_literal(mut input: &str) -> Result<(&str, Expression), String> {
    let start = input;
    if matches!(
        input.chars().next(),
        Some(_x @ ('-' | '+' | '.' | '0'..='9'))
    ) {
        while matches!(input.chars().next(), Some(_x @ ('.' | '0'..='9'))) {
            let mut chars = input.chars();
            chars.next();
            input = chars.as_str();
        }
        let num = start[..(start.len() - input.len())]
            .parse::<i32>()
            .map_err(|s| s.to_string())?;
        Ok((input, Expression::Literal(num)))
    } else {
        Err("Not a number".to_string())
    }
}

fn advance_char(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
    input.chars().next()
}

fn space(mut i: &str) -> &str {
    while peek_char(i).is_some_and(|c| c.is_whitespace()) {
        i = advance_char(i);
    }
    i
}

fn identifier(mut input: &str) -> Result<(&str, &str), String> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        input = advance_char(input);
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
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

fn factor(i: &str) -> Result<(&str, Expression), String> {
    let r = space(i);
    if let Ok((r, val)) = num_literal(r) {
        return Ok((r, val));
    }

    if let Ok((r, name)) = identifier(r) {
        let r = space(r);
        if 1 <= r.len() && &r[..1] == "(" {
            let r = space(&r[1..]);
            let (r, res) = expression(r)?;
            if r.len() < 1 || &r[..1] != ")" {
                return Err("FnInvoke is not closed".to_string());
            }
            return Ok((&r[1..], Expression::FnInvoke(name, Box::new(res))));
        }
        return Ok((r, Expression::Variable(name)));
    }

    Err("Factor is neither a numeric literal or an identifier".to_string())
}

fn mul(i: &str) -> Result<(&str, Expression), String> {
    let (r, lhs) = factor(i)?;

    let r = space(r);

    if 1 <= r.len() && matches!(&r[..1], "*" | "/") {
        let op = &r[..1];
        let (r, rhs) = mul(&r[1..])?;
        Ok((
            r,
            if op == "*" {
                Expression::Mul(Box::new(lhs), Box::new(rhs))
            } else {
                Expression::Div(Box::new(lhs), Box::new(rhs))
            },
        ))
    } else {
        Ok((r, lhs))
    }
}

fn add(i: &str) -> Result<(&str, Expression), String> {
    let (r, lhs) = mul(i)?;

    let r = space(r);

    if 1 <= r.len() && matches!(&r[..1], "+" | "-") {
        let op = &r[..1];
        let (r, rhs) = add(&r[1..])?;
        Ok((
            r,
            if op == "+" {
                Expression::Add(Box::new(lhs), Box::new(rhs))
            } else {
                Expression::Sub(Box::new(lhs), Box::new(rhs))
            },
        ))
    } else {
        Ok((r, lhs))
    }
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

    let Ok((r, _)) = recognize("<")(space(r)) else {
        return Ok((r, lhs));
    };

    let (r, rhs) = add(r)?;

    Ok((r, Expression::Lt(Box::new(lhs), Box::new(rhs))))
}

fn else_clause(i: &str) -> IResult<&str, Vec<Statement>> {
    let (r, _) = recognize("else")(space(i))?;
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

fn statement(i: &str) -> Result<(&str, Statement), String> {
    let r = space(i);

    if let Ok((r, stmts)) = brace_statement(r) {
        return Ok((r, Statement::Brace(stmts)));
    }

    if let Ok((r, stmt)) = for_stmt(r) {
        return Ok((r, stmt));
    }

    if let Ok((r, _)) = recognize("let")(r) {
        let (r, name) = identifier(space(r))?;

        if let Ok((mut r, _)) = recognize("(")(space(r)) {
            let mut params = vec![];

            loop {
                let Ok((next_r, param_name)) = identifier(space(r)) else {
                    break;
                };
                params.push(param_name);
                r = next_r;
            }

            let Ok((r, _)) = recognize(")")(space(r)) else {
                return Err(
                    "Syntax error in func decl: closing parenthesis could not be found".to_string(),
                );
            };

            let Ok((r, _)) = recognize("=")(space(r)) else {
                return Err("Syntax error in func decl: = could not be found".to_string());
            };

            let (r, stmts) = statements(r)?;

            return Ok((
                r,
                Statement::FnDecl(FnDecl {
                    name,
                    params,
                    stmts,
                }),
            ));
        }

        let Ok((r, _)) = recognize("=")(space(r)) else {
            return Err("Syntax error in var decl".to_string());
        };
        let (r, ex) = expression(r)?;
        if let Ok((r, _)) = recognize(";")(space(r)) {
            return Ok((r, Statement::VarDecl(name, ex)));
        }
        return Ok((r, Statement::VarDecl(name, ex)));
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

    Ok(stmts)
}
