// TODO operations parsing
// TODO function call parsing
// TODO func decl and var decl parsing
// TODO Write a small code formatter
// TODO Write my own result type to propagate only some kind of error (or write a macro ? A macro sounds better)
// FIXME Raise a decent error in cases like this:
/*
```
if True
then 2
     4
```
*/

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

    fn if_then_else(&mut self, enclosing_ctx: usize) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        let if_ = self.if_()?;
        let start = if_.span.start;
        if start <= enclosing_ctx {
            return Err(ErrorInfo {
                expected: Some(Expected::Named("a then token")),
                error: Error::LayoutError("The then block is expected to be as indented as the if"),
                span: if_.span,
                found: None,
            });
        }
        let if_context = start - self.last_newline;
        let condition = self.block(if_context)?;
        let is_on_another_line = match self.then().ok().or_else(|| self.indentation()) {
            Some(Spanned { span, elem }) => match elem {
                Token::Indent(n, _) => {
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
        let then_branch = self.block(if_context)?;
        let mut end = then_branch.span.end;
        let else_branch = match self.else_() {
            Ok(Spanned {
                span,
                elem: Token::Else,
            }) => {
                if is_on_another_line && span.start != if_context {
                    return Err(ErrorInfo {
                        expected: None,
                        span,
                        error: Error::LayoutError(
                            "the then block is expected to be as indented as the opening if",
                        ),
                        found: None,
                    });
                }
                let expr = self.block(if_context)?;
                end = expr.span.end;
                Some(expr)
            }
            Err(ErrorInfo { span, .. }) if !is_on_another_line => {
                return Err(ErrorInfo {
                    expected: None,
                    span,
                    error: Error::LayoutError(
                        "the `then` token cannot be on the line of the `if` token if the else is on another line",
                    ),
                    found: None,
                });
            }
            Err(_) => match self.peek() {
                Some(SpannedTok {
                    elem:
                        Token::Indent(
                            n,
                            box Spanned {
                                elem: Token::Else, ..
                            },
                        ),
                    ..
                }) if *n == if_context => {
                    self.next(); // the indentation token
                    self.next(); // the following else token
                    let expr = self.block(if_context)?;
                    end = expr.span.end;
                    Some(expr)
                }
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
            span: start..end,
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
            .or_else(|_| self.if_then_else(enclosing_ctx))
    }

    pub fn block(&mut self, enclosing_ctx: usize) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        let first_newline = self.last_newline;
        let (start, fst_instruction, span) = match self.indentation() {
            Some(Spanned {
                elem: Token::Indent(n, _),
                span,
            }) => (n, self.statement(enclosing_ctx)?, span),
            _ => self
                .statement(enclosing_ctx)
                .map(|Spanned { elem, span }| {
                    println!("{} {}", span.start, first_newline);
                    (
                        span.start - first_newline,
                        Spanned {
                            elem,
                            span: span.clone(),
                        },
                        span,
                    )
                })?,
        };
        if start <= enclosing_ctx {
            return Err(ErrorInfo {
                expected: Some(Expected::Named("a statement")),
                error: Error::LayoutError(
                    "The block is not supposed to be more indented than the enclosing context",
                ),
                span,
                found: None,
            });
        }
        println!("{:#?} {}", self.peek(), start);
        let mut instructions = vec![fst_instruction];
        while self.is_as_much_indented_than(start) {
            println!("a");
            instructions.push(self.statement(enclosing_ctx)?);
        }
        let span = first_newline
            ..instructions
                .last()
                .map(|Spanned { span, .. }| span.end)
                .unwrap();
        Ok(Spanned {
            elem: Expr::Block { instructions },
            span,
        })
    }

    fn statement(&mut self, enclosing_ctx: usize) -> Result<Spanned<Statement<'a>>, ErrorInfo<'a>> {
        self.expr(enclosing_ctx)
            .map(|Spanned { span, elem }| Spanned {
                span,
                elem: Statement::StmtExpr(elem),
            })
    }

    fn is_as_much_indented_than(&mut self, n2: usize) -> bool {
        if let Some(Spanned {
            elem: Token::Indent(n, _),
            span,
        }) = self.peek()
        {
            if *n == n2 {
                self.last_newline = self.next().unwrap().span.start;
                return true;
            }
        }
        false
    }

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
            Some(Spanned {
                span,
                elem: Token::EOF,
            }) => (span.clone(), Error::UnexpectedEOF),
            Some(Spanned { span, elem }) => (span.clone(), Error::UnexpectedTok(elem.clone())),
            _ => unreachable!(),
        }
    }

    fn unexpected_tok_or_eof(&mut self, expected: Option<Expected>) -> ErrorInfo<'a> {
        let (span, error) = self.next_or_eof();
        ErrorInfo {
            span,
            error,
            expected,
            found: None,
        }
    }

    pub fn skip_while_indent(&mut self) {
        while let Some(Spanned {
            elem: Token::Indent(_, _),
            ..
        }) = self.indentation()
        {}
    }

    fn next(&mut self) -> Option<SpannedTok<'a>> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&SpannedTok<'a>> {
        self.tokens.peek()
    }
}
