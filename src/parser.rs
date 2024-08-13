#[derive(Debug)]
pub enum Expression<'src> {
    Literal(i32),
    Variable(&'src str),
    FnInvoke(&'src str, Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Conditional(
        Box<Expression<'src>>,
        Vec<Statement<'src>>,
        Option<Vec<Statement<'src>>>,
    ),
}

#[derive(Debug)]
pub enum Statement<'src> {
    VarDecl(&'src str, Expression<'src>),
    Expr(Expression<'src>),
    FnDecl(FnDecl<'src>),
}

#[derive(Debug)]
pub struct FnDecl<'src> {
    pub(crate) name: &'src str,
    pub(crate) params: Vec<&'src str>,
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
            let (r, res) = add(r)?;
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

fn parse_params(i: &str) -> Result<(&str, Vec<&str>), String> {
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

fn else_clause(i: &str) -> IResult<&str, Vec<Statement>> {
    let (r, _) = recognize("else")(space(i))?;
    let (r, _) = recognize("{")(space(r))?;
    let (r, stmts) = statements(r)?;
    let (r, _) = recognize("}")(space(r))?;
    Ok((r, stmts))
}

fn conditional(i: &str) -> IResult<&str, Expression> {
    let (r, _) = recognize("if")(space(i))?;

    let (r, ex) = add(r)?;

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

    if let Ok((r, res)) = add(i) {
        return Ok((r, res));
    }

    Err("Expression failed to parse".to_string())
}

fn statement(i: &str) -> Result<(&str, Statement), String> {
    let r = space(i);
    if 3 < r.len() && &r[..3] == "let" {
        let r = space(&r[3..]);
        let (r, name) = identifier(r)?;

        let r = space(r);

        if 1 <= r.len() && &r[..1] == "(" {
            let mut params = vec![];

            let mut r = &r[1..];

            loop {
                let Ok((next_r, param_name)) = identifier(space(r)) else {
                    break;
                };
                params.push(param_name);
                r = next_r;
            }

            let r = space(r);

            if r.len() < 1 || &r[..1] != ")" {
                return Err(
                    "Syntax error in func decl: closing parenthesis could not be found".to_string(),
                );
            }

            let r = space(&r[1..]);

            if r.len() < 1 || &r[..1] != "=" {
                return Err("Syntax error in func decl: = could not be found".to_string());
            }

            let r = &r[1..];
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

        if r.len() < 1 || &r[..1] != "=" {
            return Err("Syntax error in var decl".to_string());
        }
        let r = &r[1..];
        let (r, ex) = expression(r)?;
        if 1 <= r.len() && &r[..1] == ";" {
            return Ok((&r[1..], Statement::VarDecl(name, ex)));
        }
        return Ok((r, Statement::VarDecl(name, ex)));
    }

    let (r, res) = expression(r)?;

    let r = space(r);

    if 1 <= r.len() && &r[..1] == ";" {
        return Ok((&r[1..], Statement::Expr(res)));
    }
    return Ok((r, Statement::Expr(res)));
}

fn statements(r: &str) -> Result<(&str, Vec<Statement>), String> {
    let r = space(r);
    if 0 < r.len() && &r[..1] == "{" {
        let mut r = &r[1..];

        let mut stmts = vec![];
        while let Ok(res) = statement(r) {
            r = res.0;
            stmts.push(res.1);
        }

        let r = space(r);

        if r.len() < 1 || &r[..1] != "}" {
            return Err("Brace expression is not closed".to_string());
        }

        return Ok((&r[1..], stmts));
    }

    let (r, stmt) = statement(r)?;

    Ok((r, vec![stmt]))
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
