use std::{iter::Peekable, ops::Range};

use crate::{
    errors::err::{Error, ErrorInfo, Expected},
    syntax::{
        ast::*,
        tokens::{Delimiter, Spanned, SpannedTok, Token},
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
    pub fn if_then_else(
        &mut self,
        enclosing_ctx: usize,
    ) -> Result<ExprSpan<Expr<'a>>, ErrorInfo<'a>> {
        let if_ctx = self.if_()?.span.start - self.last_newline;
        let condition = self.block(enclosing_ctx)?;
        match self.then() {
            Ok(_) => (),
            Err(_) => match self.indentation() {
                Some(Spanned {
                    span,
                    elem:
                        Token::Indent(
                            n,
                            box Spanned {
                                elem: Token::Then, ..
                            },
                        ),
                }) if n == if_ctx => {
                    self.next();
                }
                _ => todo!("error"),
            },
        };
        let then_arm = self.block(enclosing_ctx)?;
        let else_arm = match self.else_() {
            Ok(_) => Some(self.block(enclosing_ctx)?),
            _ => match self.indentation() {
                Some(Spanned {
                    span,
                    elem:
                        Token::Indent(
                            n,
                            box Spanned {
                                elem: Token::Else, ..
                            },
                        ),
                }) if n == if_ctx => {
                    self.indentation();
                    self.next();
                    Some(self.block(enclosing_ctx)?)
                }
                _ => None,
            },
        };
        let end = match &else_arm {
            Some(v) => v.span.end,
            None => then_arm.span.end,
        };
        Ok(ExprSpan {
            elem: Expr::IfThenElse {
                condition: condition.into(),
                then_arm: then_arm.into(),
                else_arm: else_arm.map(|v| v.into()),
            },
            column: if_ctx,
            span: if_ctx..end,
        })
    }
    pub fn block(&mut self, enclosing_ctx: usize) -> Result<ExprSpan<Expr<'a>>, ErrorInfo<'a>> {
        let (start, block_ctx, first_statement) = self
            .statement(enclosing_ctx)
            .map(|v| (v.span.start, v.column, v))
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
            match self.peek() {
                Some(SpannedTok {
                    elem: Token::Indent(n, _),
                    span,
                }) if *n == block_ctx => {
                    self.indentation();
                    instructions.push(self.statement(enclosing_ctx)?);
                }
                _ => break,
            }
        }
        let end = instructions.last().unwrap().span.end;
        Ok(ExprSpan {
            span: start..end,
            column: block_ctx,
            elem: Expr::Block { instructions },
        })
    }
    fn expr(&mut self, enclosing_ctx: usize) -> Result<ExprSpan<Expr<'a>>, ErrorInfo<'a>> {
        self.identifier()
            .or_else(|_| self.num())
            .or_else(|_| self.bool())
            .map(|Spanned { span, elem }| match elem {
                Token::Ident(i) => ExprSpan {
                    elem: Expr::Ident(ExprSpan {
                        span: span.clone(),
                        elem: i,
                        column: span.start - self.last_newline,
                    }),
                    column: span.start - self.last_newline,
                    span,
                },
                Token::Num(n) => ExprSpan {
                    elem: Expr::Literal(ExprSpan {
                        span: span.clone(),
                        column: span.start - self.last_newline,
                        elem: Literal::Num(n),
                    }),
                    column: span.start - self.last_newline,
                    span,
                },
                Token::Bool(b) => ExprSpan {
                    elem: Expr::Literal(ExprSpan {
                        span: span.clone(),
                        column: span.start - self.last_newline,
                        elem: Literal::Bool(b),
                    }),
                    column: span.start - self.last_newline,
                    span,
                },
                _ => unreachable!(),
            })
            .or_else(|_| self.if_then_else(enclosing_ctx))
    }
    fn statement(
        &mut self,
        enclosing_ctx: usize,
    ) -> Result<ExprSpan<Statement<'a>>, ErrorInfo<'a>> {
        self.expr(enclosing_ctx)
            .map(|ExprSpan { span, elem, column }| ExprSpan {
                span,
                column,
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

mod test {
    use crate::syntax::{ast::Expr::*, *};
    use pretty_assertions::{assert_eq, assert_ne};
    #[test]
    fn inline_if() {
        let code2 = "
    if True
    then 4
         2
    else if True then 2 
         else 4
    15
";
        let lexer = Lexer::new(code2);
        let mut parser = Parser::new(lexer.tokenize().unwrap().into_iter());
        let result = parser.block(0);
        assert_eq!(
            result,
            Ok(ExprSpan {
                span: 5..82,
                column: 5,
                elem: Block {
                    instructions: vec![
                        ExprSpan {
                            span: 5..75,
                            column: 5,
                            elem: Statement::StmtExpr(Expr::IfThenElse {
                                condition: ExprSpan {
                                    span: 8..12,
                                    column: 8,
                                    elem: Box::new(Block {
                                        instructions: vec![{
                                            ExprSpan {
                                                span: 8..12,
                                                column: 8,
                                                elem: Statement::StmtExpr(Literal(ExprSpan {
                                                    span: 8..12,
                                                    column: 8,
                                                    elem: ast::Literal::Bool(true),
                                                })),
                                            }
                                        }]
                                    })
                                },
                                then_arm: ExprSpan {
                                    span: 22..34,
                                    column: 10,
                                    elem: Box::new(Block {
                                        instructions: vec![
                                            ExprSpan {
                                                span: 22..23,
                                                column: 10,
                                                elem: Statement::StmtExpr(Literal(ExprSpan {
                                                    span: 22..23,
                                                    column: 10,
                                                    elem: ast::Literal::Num("4"),
                                                }))
                                            },
                                            ExprSpan {
                                                span: 33..34,
                                                column: 10,
                                                elem: Statement::StmtExpr(Expr::Literal(
                                                    ExprSpan {
                                                        span: 33..34,
                                                        column: 10,
                                                        elem: ast::Literal::Num("2"),
                                                    }
                                                ))
                                            }
                                        ]
                                    })
                                },
                                else_arm: Some(ExprSpan {
                                    span: 10..75,
                                    column: 10,
                                    elem: Box::new(Block {
                                        instructions: vec![ExprSpan {
                                            span: 10..75,
                                            column: 10,
                                            elem: Statement::StmtExpr(Expr::IfThenElse {
                                                condition: ExprSpan {
                                                    span: 47..51,
                                                    column: 13,
                                                    elem: Box::new(Block {
                                                        instructions: vec![ExprSpan {
                                                            span: 47..51,
                                                            column: 13,
                                                            elem: Statement::StmtExpr(
                                                                Expr::Literal(ExprSpan {
                                                                    span: 47..51,
                                                                    column: 13,
                                                                    elem: ast::Literal::Bool(true)
                                                                })
                                                            )
                                                        }]
                                                    })
                                                },
                                                then_arm: ExprSpan {
                                                    span: 57..58,
                                                    column: 23,
                                                    elem: Box::new(Block {
                                                        instructions: vec![ExprSpan {
                                                            span: 57..58,
                                                            column: 23,
                                                            elem: Statement::StmtExpr(
                                                                Expr::Literal(ExprSpan {
                                                                    span: 57..58,
                                                                    column: 23,
                                                                    elem: ast::Literal::Num("2"),
                                                                })
                                                            )
                                                        }]
                                                    })
                                                },
                                                else_arm: Some(ExprSpan {
                                                    span: 74..75,
                                                    column: 15,
                                                    elem: Box::new(Block {
                                                        instructions: vec![ExprSpan {
                                                            span: 74..75,
                                                            column: 15,
                                                            elem: Statement::StmtExpr(
                                                                Expr::Literal(ExprSpan {
                                                                    span: 74..75,
                                                                    column: 15,
                                                                    elem: ast::Literal::Num("4"),
                                                                })
                                                            )
                                                        }]
                                                    })
                                                })
                                            })
                                        }]
                                    })
                                })
                            })
                        },
                        ExprSpan {
                            span: 80..82,
                            column: 5,
                            elem: Statement::StmtExpr(Expr::Literal(ExprSpan {
                                span: 80..82,
                                column: 5,
                                elem: ast::Literal::Num("15"),
                            }))
                        }
                    ]
                }
            })
        );
    }
}
