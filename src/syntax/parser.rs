use std::{iter::Peekable, ops::Range};

use crate::{
    errors::err::{Error, ErrorInfo, Expected, ParseResult},
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
    pub fn if_then_else(&mut self) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        let if_ctx = self.if_()?.span.start - self.last_newline;
        let condition = self.block(if_ctx)?;
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
                    ..
                }) if n == if_ctx => {
                    self.next();
                }
                _ => {
                    let (span, error) = self.next_or_eof();
                    return Err(ErrorInfo {
                        error,
                        span,
                        expected: Some(Expected::Named("A then block")),
                    });
                }
            },
        };
        let then_arm = self.block(if_ctx)?;
        let else_arm = match self.else_() {
            Ok(_) => Some(self.block(if_ctx)?),
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
                    ..
                }) if n == if_ctx => {
                    self.indentation();
                    self.next();
                    Some(self.block(if_ctx)?)
                }
                _ => None,
            },
        };
        let end = match &else_arm {
            Some(v) => v.span.end,
            None => then_arm.span.end,
        };
        Ok(Spanned {
            elem: Expr::IfThenElse {
                condition: condition.into(),
                then_arm: then_arm.into(),
                else_arm: else_arm.map(|v| v.into()),
            },
            column: if_ctx,
            span: if_ctx..end,
        })
    }
    pub fn block(&mut self, enclosing_ctx: usize) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        let (start, block_ctx, first_statement) = self
            .statement()
            .map(|v| (v.span.start, v.column, v))
            .or_else(|_| {
                if let Some(SpannedTok {
                    elem: Token::Indent(n, _),
                    ..
                }) = self.indentation()
                {
                    if n > enclosing_ctx {
                        return Ok((n, n, self.statement()?));
                    }
                }
                let (span, error) = self.next_or_eof();
                return Err(ErrorInfo {
                    expected: Some(Expected::Named("An expression")),
                    error,
                    span,
                });
            })?;
        let mut instructions = vec![first_statement];
        loop {
            match self.peek() {
                Some(SpannedTok {
                    elem: Token::Indent(n, _),
                    span,
                    ..
                }) if *n == block_ctx => {
                    self.next();
                    instructions.push(self.statement()?);
                }
                _ => break,
            }
        }
        let end = instructions.last().unwrap().span.end;
        Ok(Spanned {
            span: start..end,
            column: block_ctx,
            elem: Expr::Block { instructions },
        })
    }
    fn expr(&mut self) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        self.num()
            .or_else(|_| self.bool())
            .map(|Spanned { span, elem, .. }| match elem {
                Token::Num(n) => Spanned {
                    elem: Expr::Literal(Literal::Num(n)),
                    column: span.start - self.last_newline,
                    span,
                },
                Token::Bool(b) => Spanned {
                    elem: Expr::Literal(Literal::Bool(b)),
                    column: span.start - self.last_newline,
                    span,
                },
                _ => unreachable!(),
            })
            .or_else(|_| self.if_then_else())
            .or_else(|_| self.function_call())
    }

    fn statement(&mut self) -> Result<Spanned<Statement<'a>>, ErrorInfo<'a>> {
        self.expr().map(|Spanned { span, elem, column }| Spanned {
            span,
            column,
            elem: Statement::StmtExpr(elem),
        })
    }
    fn function_call(&mut self) -> ParseResult<'a, Expr<'a>> {
        let function = self.ident()?;
        let fun_ctx = function.column;
        let mut params = vec![];
        let mut sub_ctx = None;
        loop {
            match self.expr() {
                Ok(v) => params.push(v),
                _ => match self.peek() {
                    Some(Spanned {
                        elem: Token::Indent(n, _),
                        span,
                        ..
                    }) if *n > fun_ctx => match sub_ctx {
                        Some(level) if level == *n => {
                            self.next();
                            params.push(self.expr()?);
                        }
                        Some(_) => {
                            return Err(ErrorInfo {
                                expected: Some(Expected::Named("A function argument")),
                                error: Error::LayoutError("All the argument placed in a function call sub context are supposed to be on the same column") ,
                                span: fun_ctx..span.end,
                            });
                        }
                        None => {
                            sub_ctx = Some(*n);
                            self.next();
                            params.push(self.expr()?);
                        }
                    },
                    _ => break,
                },
            }
        }
        if params.is_empty() {
            Ok(function)
        } else {
            Ok(Spanned {
                column: fun_ctx,
                span: function.span.start..params.last().unwrap().span.end,
                elem: Expr::Call(function.into(), params),
            })
        }
    }
    fn ident(&mut self) -> ParseResult<'a, Expr<'a>> {
        self.identifier()
            .map(|Spanned { span, elem, .. }| match elem {
                Token::Ident(i) => Spanned {
                    elem: Expr::Ident(Spanned {
                        span: span.clone(),
                        column: span.start - self.last_newline,
                        elem: i,
                    }),
                    column: span.start - self.last_newline,
                    span,
                },
                _ => unreachable!(),
            })
    }
    fn indentation(&mut self) -> Option<SpannedTok<'a>> {
        if let Some(Spanned {
            elem: Token::Indent(_, _),
            span,
            ..
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
                ..
            }) => (span.clone(), Error::UnexpectedEOF),
            Some(Spanned { span, elem, .. }) => (span.clone(), Error::UnexpectedTok(elem.clone())),
            _ => unreachable!(),
        }
    }
    fn next(&mut self) -> Option<SpannedTok<'a>> {
        let v = self.tokens.next();
        if let Some(Spanned {
            elem: Token::Indent(_, _),
            span,
            ..
        }) = &v
        {
            self.last_newline = span.start;
        }
        v
    }

    fn peek(&mut self) -> Option<&SpannedTok<'a>> {
        self.tokens.peek()
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
}

// SHUNTING YARD

enum SYOp {
    Unary(UnOp),
    Binary(BinOp),
}
impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    /*#[allow(unused)]
    fn shunting_yard(&mut self) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        match self.sy_atom(){
        }
    }*/
    fn sy_atom(&mut self) -> Result<Spanned<Expr<'a>>, ErrorInfo<'a>> {
        self.num()
            .or_else(|_| self.bool())
            .map(|Spanned { span, elem, .. }| match elem {
                Token::Num(n) => Spanned {
                    elem: Expr::Literal(Literal::Num(n)),
                    column: span.start - self.last_newline,
                    span,
                },
                Token::Bool(b) => Spanned {
                    elem: Expr::Literal(Literal::Bool(b)),
                    column: span.start - self.last_newline,
                    span,
                },
                _ => unreachable!(),
            })
            .or_else(|_| self.if_then_else())
            .or_else(|_| self.function_call())
    }
}

// TESTS

#[allow(unused)]
mod test {
    use crate::syntax::*;
    use pretty_assertions::assert_eq;
    #[test]
    fn inline_if() {
        let code = "
    if True then 2 else if True then 3
    ";
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer.tokenize().unwrap().into_iter());
        let result = parser.block(0).unwrap();
        assert_eq!(
            result,
            Spanned {
                span: 5..39,
                column: 5,
                elem: Expr::Block {
                    instructions: vec![Spanned {
                        span: 5..39,
                        column: 5,
                        elem: Statement::StmtExpr(Expr::IfThenElse {
                            condition: Spanned {
                                span: 8..12,
                                column: 8,
                                elem: Box::new(Expr::Block {
                                    instructions: vec![Spanned {
                                        span: 8..12,
                                        column: 8,
                                        elem: Statement::StmtExpr(Expr::Literal(
                                            ast::Literal::Bool(true)
                                        ))
                                    }]
                                })
                            },
                            then_arm: Spanned {
                                span: 18..19,
                                column: 18,
                                elem: Box::new(Expr::Block {
                                    instructions: vec![Spanned {
                                        span: 18..19,
                                        column: 18,
                                        elem: Statement::StmtExpr(Expr::Literal(
                                            ast::Literal::Num("2")
                                        ))
                                    }]
                                })
                            },
                            else_arm: Some(Spanned {
                                span: 25..39,
                                column: 25,
                                elem: Box::new(Expr::Block {
                                    instructions: vec![Spanned {
                                        span: 25..39,
                                        column: 25,
                                        elem: Statement::StmtExpr(Expr::IfThenElse {
                                            condition: Spanned {
                                                span: 28..32,
                                                column: 28,
                                                elem: Box::new(Expr::Block {
                                                    instructions: vec![Spanned {
                                                        span: 28..32,
                                                        column: 28,
                                                        elem: Statement::StmtExpr(Expr::Literal(
                                                            ast::Literal::Bool(true)
                                                        ))
                                                    }]
                                                })
                                            },
                                            then_arm: Spanned {
                                                span: 38..39,
                                                column: 38,
                                                elem: Box::new(Expr::Block {
                                                    instructions: vec![Spanned {
                                                        span: 38..39,
                                                        column: 38,
                                                        elem: Statement::StmtExpr(Expr::Literal(
                                                            ast::Literal::Num("3",),
                                                        ),),
                                                    },],
                                                }),
                                            },
                                            else_arm: None
                                        })
                                    }]
                                })
                            }),
                        })
                    }]
                }
            }
        );
    }
    #[test]
    fn nested_if() {
        let code = "
    if True
    then 4
         2
    else if True then 2 
         else 4
    15
";
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer.tokenize().unwrap().into_iter());
        let result = parser.block(0).unwrap();
        assert_eq!(
            result,
            Spanned {
                span: 5..82,
                column: 5,
                elem: Expr::Block {
                    instructions: vec![
                        Spanned {
                            span: 5..75,
                            column: 5,
                            elem: Statement::StmtExpr(Expr::IfThenElse {
                                condition: Spanned {
                                    span: 8..12,
                                    column: 8,
                                    elem: Box::new(Expr::Block {
                                        instructions: vec![{
                                            Spanned {
                                                span: 8..12,
                                                column: 8,
                                                elem: Statement::StmtExpr(Expr::Literal(
                                                    ast::Literal::Bool(true),
                                                )),
                                            }
                                        }]
                                    })
                                },
                                then_arm: Spanned {
                                    span: 22..34,
                                    column: 10,
                                    elem: Box::new(Expr::Block {
                                        instructions: vec![
                                            Spanned {
                                                span: 22..23,
                                                column: 10,
                                                elem: Statement::StmtExpr(Expr::Literal(
                                                    ast::Literal::Num("4"),
                                                ))
                                            },
                                            Spanned {
                                                span: 33..34,
                                                column: 10,
                                                elem: Statement::StmtExpr(Expr::Literal(
                                                    ast::Literal::Num("2"),
                                                ))
                                            }
                                        ]
                                    })
                                },
                                else_arm: Some(Spanned {
                                    span: 10..75,
                                    column: 10,
                                    elem: Box::new(Expr::Block {
                                        instructions: vec![Spanned {
                                            span: 10..75,
                                            column: 10,
                                            elem: Statement::StmtExpr(Expr::IfThenElse {
                                                condition: Spanned {
                                                    span: 47..51,
                                                    column: 13,
                                                    elem: Box::new(Expr::Block {
                                                        instructions: vec![Spanned {
                                                            span: 47..51,
                                                            column: 13,
                                                            elem: Statement::StmtExpr(
                                                                Expr::Literal(ast::Literal::Bool(
                                                                    true
                                                                ))
                                                            )
                                                        }]
                                                    })
                                                },
                                                then_arm: Spanned {
                                                    span: 57..58,
                                                    column: 23,
                                                    elem: Box::new(Expr::Block {
                                                        instructions: vec![Spanned {
                                                            span: 57..58,
                                                            column: 23,
                                                            elem: Statement::StmtExpr(
                                                                Expr::Literal(ast::Literal::Num(
                                                                    "2"
                                                                ),)
                                                            )
                                                        }]
                                                    })
                                                },
                                                else_arm: Some(Spanned {
                                                    span: 74..75,
                                                    column: 15,
                                                    elem: Box::new(Expr::Block {
                                                        instructions: vec![Spanned {
                                                            span: 74..75,
                                                            column: 15,
                                                            elem: Statement::StmtExpr(
                                                                Expr::Literal(ast::Literal::Num(
                                                                    "4"
                                                                ),)
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
                        Spanned {
                            span: 80..82,
                            column: 5,
                            elem: Statement::StmtExpr(Expr::Literal(ast::Literal::Num("15"),))
                        }
                    ]
                }
            }
        );
    }
}
