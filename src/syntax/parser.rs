use std::{iter::Peekable, ops::Range};

use crate::{
    errors::err::{Error, ErrorInfo, Expected},
    syntax::{
        ast::*,
        tokens::{Delimiter, SpannedTok, Token},
    },
};
macro_rules! token {
    ($name: ident, $p: pat, $expected: literal) => {
        pub fn $name(&mut self) -> Result<SpannedTok<'a>, ErrorInfo<'a>> {
            if let Some(Spanned { elem: $p, .. }) = self.peek() {
                return Ok(self.next().unwrap());
            }
            let (span, error) = self.next_or_eof();
            Err(ErrorInfo {
                expected: Some(Expected::Named($expected)),
                found: None,
                span,
                error,
            })
        }
    };
}

pub struct Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    tokens: Peekable<I>,
    last_newline: usize,
}
#[allow(unused)]
impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            last_newline: 0,
        }
    }
    pub fn block(&mut self, enclosing_ctx: usize) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        let (start, block_ctx, first_statement) = self
            .statement(enclosing_ctx)
            .map(|v| (v.span.start, v.span.start - self.last_newline, v))
            .or_else(|_| {
                if let Some(SpannedTok { span, .. }) = self.indentation() {
                    if span.end > enclosing_ctx {
                        return Ok((span.end, span.end, self.statement(enclosing_ctx)?));
                    }
                }
                todo!();
            })?;
        let mut instructions = vec![first_statement];
        loop {
            // doesn't match
            match self.peek() {
                Some(SpannedTok {
                    elem: Token::Indent(_, _),
                    span,
                }) if span.end == block_ctx => {
                    instructions.push(self.statement(enclosing_ctx)?);
                }
                Some(SpannedTok { span, .. }) if span.start == block_ctx => {
                    instructions.push(self.statement(enclosing_ctx)?);
                }
                _ => break,
            }
        }
        let end = instructions.last().unwrap().span.end;
        Ok(Spanned {
            span: start..end,
            elem: Expr::Block { instructions },
        })
    }
    fn expr(&mut self, enclosing_ctx: usize) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        self.identifier()
            .or_else(|_| self.num())
            .or_else(|_| self.bool())
            .map(|Spanned { span, elem }| match elem {
                Token::Ident(i) => Spanned {
                    elem: Expr::Ident(Spanned {
                        span: span.clone(),
                        elem: i,
                    }),
                    span,
                },
                Token::Num(n) => Spanned {
                    elem: Expr::Literal(Spanned {
                        span: span.clone(),
                        elem: Literal::Num(n),
                    }),
                    span,
                },
                Token::Bool(b) => Spanned {
                    elem: Expr::Literal(Spanned {
                        span: span.clone(),
                        elem: Literal::Bool(b),
                    }),
                    span,
                },
                _ => unreachable!(),
            })
    }
    fn statement(&mut self, enclosing_ctx: usize) -> Result<Spanned<Statement<'a>>, ErrorInfo<'a>> {
        self.expr(enclosing_ctx)
            .map(|Spanned { span, elem }| Spanned {
                span,
                elem: Statement::StmtExpr(elem),
            })
    }

    token!(
        lparen,
        Token::Delimiter(Delimiter::LParen),
        "left parenthesis"
    );
    token!(
        rparen,
        Token::Delimiter(Delimiter::RParen),
        "right parenthesis"
    );
    token!(lbrace, Token::Delimiter(Delimiter::LBrace), "left brace");
    token!(rbrace, Token::Delimiter(Delimiter::RBrace), "right brace");

    token!(if_, Token::If, "if token");
    token!(then, Token::Then, "then token");
    token!(else_, Token::Else, "else token");
    token!(num, Token::Num(_), "number");
    token!(identifier, Token::Ident(_), "identifier");
    token!(bool, Token::Bool(_), "boolean");

    fn indentation(&mut self) -> Option<SpannedTok<'a>> {
        if let Some(Spanned {
            elem: Token::Indent(_, _),
            span,
        }) = self.peek()
        {
            self.last_newline = span.start;
            return self.next();
        }
        None
    }

    fn next_or_eof(&mut self) -> (Range<usize>, Error<'a>) {
        match self.peek() {
            Some(Spanned {
                span,
                elem: Token::EOF,
            }) => (span.clone(), Error::UnexpectedEOF),
            Some(Spanned { span, elem }) => (span.clone(), Error::UnexpectedTok(elem.clone())),
            _ => unreachable!(),
        }
    }

    fn next(&mut self) -> Option<SpannedTok<'a>> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&SpannedTok<'a>> {
        self.tokens.peek()
    }
}
