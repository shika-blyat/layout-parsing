use crate::source_pos::Span;
#[derive(Debug)]
pub struct SyntaxErr {
    pub span: Span,
    pub reason: SyntaxErrReason,
    pub expected: Option<&'static str>,
}
#[derive(Debug)]
pub enum SyntaxErrReason {
    UnexpectedChar(char),
    UnclosedStringLiteral,
}
