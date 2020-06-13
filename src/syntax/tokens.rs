use std::{convert::TryFrom, iter::Peekable, ops::Range};

pub use crate::{
    errors::err::{Error, SpannedErr},
    source_pos::Spanned,
};

pub type SpannedTok<'a> = Spanned<Token<'a>>;

#[derive(Debug, Copy, Clone)]
pub enum Token<'a> {
    Op(&'a str),

    // I don't want to handle literal overflow and everything during parsing, that'd be ugly imo, so i'll just store them as strings
    Num(&'a str),

    Str(&'a str),

    Ident(&'a str),

    Bool(bool),

    If,

    Then,

    Else,

    Equal,

    Newline,

    Delimiter(Delimiter),

    Indent(usize),
}

impl<'a> TryFrom<&'a str> for Token<'a> {
    type Error = ();
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Ok(match value {
            "=" => Token::Equal,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "True" => Token::Bool(true),
            "False" => Token::Bool(false),
            "(" | ")" | "{" | "}" => {
                Token::Delimiter(Delimiter::try_from(value.chars().next().unwrap()).unwrap())
            }
            "+" | "-" | "*" | "/" | "!" | "==" | "!=" | "&&" | "||" => Token::Op(value),
            _ => return Err(()),
        })
    }
}
#[derive(Debug, Copy, Clone)]
pub enum Delimiter {
    LParen,
    RParen,
    LBrace,
    RBrace,
}
impl TryFrom<char> for Delimiter {
    type Error = ();
    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            '(' => Delimiter::LParen,
            ')' => Delimiter::RParen,
            '{' => Delimiter::LBrace,
            '}' => Delimiter::RBrace,
            _ => return Err(()),
        })
    }
}
