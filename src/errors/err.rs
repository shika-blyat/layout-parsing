use crate::{
    source_pos::{Span, Spanned},
    syntax::tokens::Token,
};

pub type SpannedErr<'a> = Spanned<Error<'a>>;
#[derive(Debug)]
pub struct ErrorInfo<'a> {
    pub expected: Option<Expected>,
    pub found: Option<Found>,
    pub error: Error<'a>,
    pub span: Span,
}
#[derive(Debug)]
pub enum Error<'a> {
    UnexpectedChar(char),
    UnclosedStringLiteral,
    LayoutError(&'static str),
    UnexpectedTok(Token<'a>),
    UnexpectedEOF,
}

#[derive(Debug)]
pub enum Expected {
    Named(&'static str),
}
#[derive(Debug)]
pub enum Found {
    Named(&'static str),
    Char(char),
}