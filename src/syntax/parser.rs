// TODO operations parsing
// TODO function call parsing
// TODO func decl and var decl parsing
// TODO Write a small code formatter

use std::{iter::Peekable, ops::Range};

use crate::{
    errors::err::{Error, ErrorInfo, Expected, SpannedErr},
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
    last_indent_peeked: Option<SpannedTok<'a>>,
    last_newline: usize,
}
impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            last_indent_peeked: None,
            last_newline: 0,
        }
    }
    fn skip_while_indent(&mut self) {
        while let Some(Spanned {
            elem: Token::Ident(_),
            ..
        }) = self.indentation()
        {}
    }
    pub fn if_then_else(&mut self) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        let start = self.if_()?.span.start;
        let if_context = start - self.last_newline;
        let condition = self.expr()?;
        let changed_line = match self.then().ok().or_else(|| self.indentation()) {
            Some(Spanned { span, elem }) => match elem {
                Token::Indent(n) => {
                    if if_context != n {
                        return Err(ErrorInfo {
                            expected: Some(Expected::Named("a then token")),
                            error: Error::LayoutError(
                                "The then block is expected to be as indented as the if",
                            ),
                            span,
                            found: None,
                        });
                    }
                    self.then()?;
                    true
                }
                Token::Then => false,
                _ => unreachable!(),
            },
            None => return Err(self.unexpected_tok_or_eof(Some(Expected::Named("then block")))),
        };
        let then_branch = self.expr()?;
        let else_branch = match self.else_() {
            Ok(Spanned {
                span,
                elem: Token::Else,
            }) => {
                if changed_line && span.start != if_context {
                    return Err(ErrorInfo {
                        expected: None,
                        span,
                        error: Error::LayoutError(
                            "the then block is expected to be as indented as the opening if",
                        ),
                        found: None,
                    });
                }
                Some(self.expr()?)
            }
            Err(_) => match self.peek_indent() {
                Some(SpannedTok {
                    elem: Token::Indent(n),
                    ..
                }) if *n == if_context => match self.peek() {
                    Some(Spanned {
                        elem: Token::Else,
                        span,
                    }) => {
                        self.next();
                        self.next();
                        Some(self.expr()?)
                    }
                    _ => None,
                },
                _ => None,
            },
            _ => unreachable!(),
        };
        Ok(Spanned {
            elem: Expr::IfThenElse {
                condition: condition.into(),
                then_branch: then_branch.into(),
                else_branch: else_branch.map(|e| e.into()),
            },
            span: start..0,
        })
    }
    fn expr(&mut self) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        self.block().or_else(|_| self.expr_())
    }
    fn expr_(&mut self) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
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
            .or_else(|_| self.if_then_else())
            .or_else(|_| Err(self.unexpected_tok_or_eof(Some(Expected::Named("an expression")))))
    }
    fn block(&mut self) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        let last_newline = self.last_newline;
        let (start, fst_instruction) = match self.indentation() {
            Some(Spanned {
                elem: Token::Indent(n),
                ..
            }) => (n, self.statement()?),
            _ => self.statement().map(|Spanned { elem, span }| {
                println!("a: {:#?} {:#?}", span, elem);
                (span.start - last_newline, Spanned { elem, span })
            })?,
        };
        let mut instructions = vec![fst_instruction];
        while let Some(n) = self.is_more_or_equally_indented_than(start) {
            // FIXME: allow blocks like this:
            // if True
            // then 2
            //      4
            instructions.push(self.statement()?);
        }
        let span = 0..0;
        Ok(Spanned {
            elem: Expr::Block { instructions },
            span: start..0,
        })
    }
    fn statement(&mut self) -> Result<Spanned<Statement<'a>>, ErrorInfo<'a>> {
        self.expr_().map(|Spanned { span, elem }| Spanned {
            span,
            elem: Statement::StmtExpr(elem),
        })
    }
    // FIXME This method only exists because there's some cases where i need to be able to peek an indent token and the following token too
    // There's probably a cleaner workaround than that, but for the moment, i don't know any, and it work as it is.
    fn peek_indent<'b>(&'b mut self) -> &'b Option<SpannedTok<'a>> {
        if self.last_indent_peeked.is_some() {
            panic!("Internal error occured, oopsie")
        }
        self.last_indent_peeked = self.next();
        &self.last_indent_peeked
    }
    fn is_more_or_equally_indented_than(&mut self, n2: usize) -> Option<usize> {
        if let Some(Spanned {
            elem: Token::Indent(n),
            ..
        }) = self.last_indent_peeked
        {
            if n >= n2 {
                self.last_indent_peeked
                    .take()
                    .map(|Spanned { elem, .. }| match elem {
                        Token::Indent(n) => n - self.last_newline,
                        _ => unreachable!(),
                    });
            }
        } else if let Some(Spanned {
            elem: Token::Indent(n),
            ..
        }) = self.peek()
        {
            if *n == n2 {
                if let Some(Spanned {
                    elem: Token::Indent(n),
                    span,
                }) = self.next()
                {
                    self.last_newline = span.start;
                    return Some(n);
                }
            }
        }
        None
    }
    fn indentation(&mut self) -> Option<SpannedTok<'a>> {
        if self.last_indent_peeked.is_some() {
            return self.last_indent_peeked.take();
        }
        if let Some(Spanned {
            elem: Token::Indent(n),
            span,
        }) = self.peek()
        {
            self.last_newline = span.start;
            return self.next();
        }
        None
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
    fn next_or_eof(&mut self) -> (Range<usize>, Error<'a>) {
        match self.peek() {
            Some(Spanned { span, elem }) => (span.clone(), Error::UnexpectedTok(*elem)),
            _ => (std::usize::MAX..std::usize::MAX, Error::UnexpectedEOF),
        }
    }
    fn unexpected_tok_or_eof(&mut self, expected: Option<Expected>) -> ErrorInfo<'a> {
        let (span, error) = match self.peek() {
            Some(Spanned { span, elem }) => (span.clone(), Error::UnexpectedTok(*elem)),
            _ => (std::usize::MAX..std::usize::MAX, Error::UnexpectedEOF),
        };
        ErrorInfo {
            span,
            error,
            expected,
            found: None,
        }
    }
    fn next(&mut self) -> Option<SpannedTok<'a>> {
        if self.last_indent_peeked.is_none() {
            self.tokens.next()
        } else {
            self.last_indent_peeked.take()
        }
    }
    fn peek(&mut self) -> Option<&SpannedTok<'a>> {
        self.tokens.peek()
    }
}
