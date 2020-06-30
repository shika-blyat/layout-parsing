use std::convert::TryFrom;

pub use crate::errors::err::Error;
use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, PartialEq, Clone)]
pub struct Spanned<T> {
    pub span: Range<usize>,
    pub column: usize,
    pub elem: T,
}

impl<T> From<Spanned<T>> for Spanned<Box<T>> {
    fn from(b: Spanned<T>) -> Spanned<Box<T>> {
        Spanned {
            elem: Box::new(b.elem),
            span: b.span,
            column: b.column,
        }
    }
}

pub type SpannedTok<'a> = Spanned<Token<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Op(&'a str),

    // I don't want to handle literal overflow and everything during lexing, so I just store them as strings
    Num(&'a str),

    Str(&'a str),

    Ident(&'a str),

    Bool(bool),

    Unit,

    Placeholder,

    If,

    Then,

    Let,

    Else,

    Equal,

    Delimiter(Delimiter),

    // Contains a token just after a newline
    Newline(Box<Spanned<Token<'a>>>),
    EOF,
}

impl<'a> TryFrom<&'a str> for Token<'a> {
    type Error = ();
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Ok(match value {
            "=" => Token::Equal,
            "if" => Token::If,
            "let" => Token::Let,
            "then" => Token::Then,
            "else" => Token::Else,
            "True" => Token::Bool(true),
            "False" => Token::Bool(false),
            "(" | ")" => {
                Token::Delimiter(Delimiter::try_from(value.chars().next().unwrap()).unwrap())
            }
            "+" | "-" | "*" | "/" | "!" | "==" | "!=" | "&&" | "||" => Token::Op(value),
            _ => return Err(()),
        })
    }
}
#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Delimiter {
    LParen,
    RParen,
}
impl TryFrom<char> for Delimiter {
    type Error = ();
    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            '(' => Delimiter::LParen,
            ')' => Delimiter::RParen,
            _ => return Err(()),
        })
    }
}
