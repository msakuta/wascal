pub enum Expression {
    Literal(i32),
    Add(Box<Expression>, Box<Expression>),
    // Mul(Box<Expression>, Box<Expression>),
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

pub fn add(i: &str) -> Result<(&str, Expression), String> {
    let (r, lhs) = num_literal(i)?;

    if 1 <= r.len() && &r[..1] == "+" {
        let (r, rhs) = add(&r[1..])?;
        Ok((r, Expression::Add(Box::new(lhs), Box::new(rhs))))
    } else {
        Ok((r, lhs))
    }
}
