#[derive(Debug)]
pub enum Expression<'src> {
    Literal(i32),
    Variable(&'src str),
    FnInvoke(&'src str, Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
}

#[derive(Debug)]
pub enum Statement<'src> {
    VarDecl(&'src str, Expression<'src>),
    Expr(Expression<'src>),
}

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
            dbg!(&r);
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

    if 1 <= r.len() && &r[..1] == "*" {
        let (r, rhs) = mul(&r[1..])?;
        Ok((r, Expression::Mul(Box::new(lhs), Box::new(rhs))))
    } else {
        Ok((r, lhs))
    }
}

fn add(i: &str) -> Result<(&str, Expression), String> {
    let (r, lhs) = mul(i)?;

    let r = space(r);

    if 1 <= r.len() && &r[..1] == "+" {
        let (r, rhs) = add(&r[1..])?;
        Ok((r, Expression::Add(Box::new(lhs), Box::new(rhs))))
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

fn statement(i: &str) -> Result<(&str, Statement), String> {
    let r = space(i);
    if 3 < r.len() && &r[..3] == "let" {
        let r = dbg!(space(&r[3..]));
        let (r, name) = identifier(r)?;
        let r = space(r);
        if r.len() < 1 || &r[..1] != "=" {
            return Err("Syntax error in var decl".to_string());
        }
        let r = &r[1..];
        let (r, ex) = add(r)?;
        dbg!(&r);
        if 1 <= r.len() && &r[..1] == ";" {
            return Ok((&r[1..], Statement::VarDecl(name, ex)));
        }
        return Ok((r, Statement::VarDecl(name, ex)));
    }
    let (r, res) = add(r)?;

    let r = space(r);

    if 1 <= r.len() && &r[..1] == ";" {
        return Ok((&r[1..], Statement::Expr(res)));
    }
    return Ok((r, Statement::Expr(res)));
}

pub fn parse(i: &str) -> Result<(Vec<String>, Vec<Statement>), String> {
    let (mut r, params) = parse_params(i)?;
    let mut stmts = vec![];
    while let Ok(res) = statement(r) {
        r = res.0;
        stmts.push(res.1);
    }
    Ok((params.into_iter().map(|s| s.to_string()).collect(), stmts))
}
