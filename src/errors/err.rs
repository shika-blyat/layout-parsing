// TODO Integrate codespan for errors pretty printing, and write the necessary Display implementations

use crate::syntax::tokens::{Span, Spanned, Token};
#[allow(unused)]
pub type SpannedErr<'a> = Spanned<Error<'a>>;
pub type ParseResult<'a, T> = Result<Spanned<T>, ErrorInfo<'a>>;
#[derive(Debug, PartialEq)]
pub struct ErrorInfo<'a> {
    pub expected: Option<Expected>,
    pub error: Error<'a>,
    pub span: Span,
}
#[derive(Debug, PartialEq)]
pub enum Error<'a> {
    UnexpectedChar(char),
    UnclosedStringLiteral,
    LayoutError(&'static str),
    UnexpectedTok(Token<'a>),
    UnexpectedEOF,
}

#[derive(Debug, PartialEq)]
pub enum Expected {
    Named(&'static str),
}
