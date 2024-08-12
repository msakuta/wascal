#[derive(Debug)]
pub enum Expression<'src> {
    Literal(i32),
    Variable(&'src str),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
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

fn identifier(mut input: &str) -> Result<(&str, Expression), String> {
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
            Expression::Variable(&start[..(start.len() - input.len())]),
        ))
    } else {
        Err("Invalid identifier".to_string())
    }
}

fn factor(i: &str) -> Result<(&str, Expression), String> {
    if let Ok((r, val)) = num_literal(i) {
        return Ok((r, val));
    }
    identifier(i)
}

fn mul(i: &str) -> Result<(&str, Expression), String> {
    let (r, lhs) = factor(i)?;

    if 1 <= r.len() && &r[..1] == "*" {
        let (r, rhs) = mul(&r[1..])?;
        Ok((r, Expression::Mul(Box::new(lhs), Box::new(rhs))))
    } else {
        Ok((r, lhs))
    }
}

fn add(i: &str) -> Result<(&str, Expression), String> {
    let (r, lhs) = mul(i)?;

    if 1 <= r.len() && &r[..1] == "+" {
        let (r, rhs) = add(&r[1..])?;
        Ok((r, Expression::Add(Box::new(lhs), Box::new(rhs))))
    } else {
        Ok((r, lhs))
    }
}

pub fn parse(i: &str) -> Result<Expression, String> {
    let (_, res) = add(i)?;
    Ok(res)
}
