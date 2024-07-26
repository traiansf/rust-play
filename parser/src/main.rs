use std::rc::Rc;
use std::io::stdin;

#[derive(Debug)]
#[derive(PartialEq)]
struct Parsed<'a, T> {
    result: T,
    rest: &'a str,
}

impl<'a, T> Parsed<'a, T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> Parsed<'a, U> {
        match self {
            Parsed { result, rest } => Parsed { result: op(result), rest }
        }

    }
}

#[derive(Debug)]
#[derive(PartialEq)]
enum ParserError {
    EmptyString,
    UnexpectedWhitespace,
    ExpectingAplha,
    ParseIntError,
    InputNotConsumed,
    UnexpectedChar(char),
    UnbalancedParentheses,
}

trait Parser<'a> {
    fn from_str(input: &'a str) -> Result<Parsed<'a, Self>, ParserError> where Self: Sized;
    fn from_whole_str(input: &'a str) -> Result<Self, ParserError> where Self: Sized {
        let Parsed { result, rest } = Self::from_str(input)?;
        if rest.is_empty() {
            Ok(result)
        } else {
            Err(ParserError::InputNotConsumed)
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
#[derive(PartialEq)]
struct IdStr(String);

impl<'a> Parser<'a> for IdStr {
    fn from_str(s: &'a str) -> Result<Parsed<'a, Self>, ParserError> where Self: Sized {
        let fst = match s.chars().next() {
            Some(c) => c,
            None => return Err(ParserError::EmptyString),
        };
        if fst.is_ascii_whitespace() {
            return Err(ParserError::UnexpectedWhitespace);
        }
        if !fst.is_alphabetic() {
            return Err(ParserError::ExpectingAplha);
        }
        for (idx, c) in s[1..].chars().enumerate() {
            if !c.is_ascii_alphanumeric() {
                return Ok(Parsed { result: IdStr(s[..idx+1].to_string()), rest: s[idx+1..].trim_start() }, );
            }
        }
        Ok(Parsed { result: IdStr(s.to_string()), rest: "" })
    }
}

impl<'a> Parser<'a> for i128 {
    fn from_str(s: &'a str) -> Result<Parsed<'a, Self>, ParserError> where Self: Sized {
        for (idx, c) in s.chars().enumerate() {
            if !c.is_ascii_digit() {
                return s[..idx].parse()
                    .map_err(|_| ParserError::ParseIntError)
                    .map(|n| Parsed { result: n, rest: s[idx..].trim_start() });
            }
        }
        s.parse()
            .map_err(|_| ParserError::ParseIntError)
            .map(|n| Parsed { result: n, rest: "" })
    }
}


#[derive(Debug)]
#[derive(PartialEq)]
enum AExp {
    Nu(i128),
    Id(String),
    PlusE(Rc<AExp>, Rc<AExp>),
    MinusE(Rc<AExp>, Rc<AExp>),
    TimesE(Rc<AExp>, Rc<AExp>),
    DivE(Rc<AExp>, Rc<AExp>),
}
impl AExp {
    fn mk_bin(t1: AExp, op: char, t2: AExp) -> AExp {
        match op {
            '+' => AExp::PlusE(Rc::new(t1), Rc::new(t2)),
            '-' => AExp::MinusE(Rc::new(t1), Rc::new(t2)),
            '*' => AExp::TimesE(Rc::new(t1), Rc::new(t2)),
            '/' => AExp::DivE(Rc::new(t1), Rc::new(t2)),
            _ => todo!()
        }
    }
}
impl<'a> Parser<'a> for AExp {
    fn from_str(s: &'a str) -> Result<Parsed<'a, Self>, ParserError> where Self: Sized {
        let Parsed { result, mut rest } = Term::from_str(s)?;
        let mut result = result.unbox();
        while let Some(op) = rest.chars().next() {
            if !(op == '+' || op == '-') {
                return Ok(Parsed { result: result, rest});
            }
            let Parsed { result: Term(t2), rest: r} = Term::from_str(rest[1..].trim_start())?;
            result = AExp::mk_bin(result, op, t2);
            rest = r;
        }
        Ok(Parsed { result, rest: "" })
    }
}

#[repr(transparent)]
#[derive(Debug)]
#[derive(PartialEq)]
struct Term(AExp);
impl Term {
    fn unbox(self) -> AExp {
        let Term(a) = self;
        a
    }
}
impl<'a> Parser<'a> for Term {
    fn from_str(s: &'a str) -> Result<Parsed<'a, Self>, ParserError> where Self: Sized {
        let Parsed { result, mut rest } = Factor::from_str(s)?;
        let mut result = result.unbox();
        while let Some(op) = rest.chars().next() {
            if !(op == '*' || op == '/') {
                return Ok(Parsed { result: Term(result), rest});
            }
            let Parsed { result: Factor(t2), rest: r} = Factor::from_str(rest[1..].trim_start())?;
            result = AExp::mk_bin(result, op, t2);
            rest = r;
        }
        Ok(Parsed { result: Term(result), rest: "" })
    }
}

#[repr(transparent)]
#[derive(Debug)]
#[derive(PartialEq)]
struct Factor(AExp);
impl Factor {
    fn unbox(self) -> AExp {
        let Factor(a) = self;
        a
    }
}

impl<'a> Parser<'a> for Factor {
    fn from_str(s: &'a str) -> Result<Parsed<'a, Self>, ParserError> where Self: Sized {
        let fst = match s.chars().next() {
            Some(c) => c,
            None => return Err(ParserError::EmptyString),
        };
        match fst {
            '(' => {
                let Parsed{ result, rest } = AExp::from_str(&s[1..].trim_start())?;
                match rest.chars().next() {
                    Some(')') => Ok(Parsed { result: Factor(result), rest: rest[1..].trim_start() }),
                    _ => Err(ParserError::UnbalancedParentheses)

                }
            }
            '0'..='9' =>
                i128::from_str(s)
                    .map(|p| p.map(|n| Factor(AExp::Nu(n)))),
            'a'..='z' | 'A'..='Z' =>
                IdStr::from_str(s)
                    .map(|p| p.map(|IdStr(id)| Factor(AExp::Id(id)))),
            _ => Err(ParserError::UnexpectedChar(fst))
        }
    }
}


fn main() {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    println!("{:?}", AExp::from_whole_str(buf.trim()));
}

#[cfg(test)]
mod tests {
    use crate::{IdStr, Parsed, Parser, ParserError};


    #[test]
    fn parse_id() {
        assert_eq!(IdStr::from_str("x"), Ok(Parsed { result:  IdStr("x".to_string()), rest: "" }));
        assert_eq!(IdStr::from_str("x -y"), Ok(Parsed { result:  IdStr("x".to_string()), rest: "-y" }));
        assert_eq!(IdStr::from_str(" x -y"), Err(ParserError::UnexpectedWhitespace));
        assert_eq!(IdStr::from_str(""), Err(ParserError::EmptyString));
        assert_eq!(IdStr::from_str("-x"), Err(ParserError::ExpectingAplha));
    }

    #[test]
    fn parse_num() {
        assert_eq!(i128::from_str("10x"), Ok(Parsed { result: 10, rest: "x" }));
        assert_eq!(i128::from_str("10 -3"), Ok(Parsed { result: 10, rest: "-3" }));
        assert_eq!(i128::from_str(" 1 -y"), Err(ParserError::ParseIntError));
        assert_eq!(i128::from_str(""), Err(ParserError::ParseIntError));
        assert_eq!(i128::from_str("-1"), Err(ParserError::ParseIntError));
    }
}