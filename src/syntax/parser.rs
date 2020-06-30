use crate::{
    errors::err::{Error, ErrorInfo, Expected, SpannedResult},
    syntax::{
        ast::*,
        tokens::{Delimiter, Spanned, SpannedTok, Token},
    },
};
use std::{iter::Peekable, ops::Range};

macro_rules! token {
    ($name: ident, $p: pat, $expected: literal) => {
        pub fn $name(&mut self) -> SpannedResult<'a, Token<'a>> {
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
    pub(super) tokens: Peekable<I>,
}

#[allow(unused)]
impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }
    pub fn module(&mut self) -> Result<Module<'a>, ErrorInfo<'a>> {
        let items = std::iter::from_fn(|| self.item(0).ok()).collect();
        Ok(Module { items })
    }
    fn item(&mut self, ctx: usize) -> SpannedResult<'a, Item<'a>> {
        self.function_decl(ctx)
            .map(|Spanned { elem, span, column }| Spanned {
                span,
                column,
                elem: Item::Function(elem),
            })
    }
    pub fn function_decl(&mut self, ctx: usize) -> SpannedResult<'a, FunctionDecl<'a>> {
        let name = self.ident_string()?;
        let arguments = std::iter::from_fn(|| self.pattern().ok()).collect();
        self.equal()?;
        let body = self.block(ctx)?;
        Ok(Spanned {
            span: name.span.start..body.span.end,
            column: name.column,
            elem: FunctionDecl {
                name,
                arguments,
                body,
            },
        })
    }
    fn pattern(&mut self) -> SpannedResult<'a, Pattern<'a>> {
        self.ident_string()
            .map(|Spanned { elem, span, column }| Spanned {
                elem: Pattern::Named(elem),
                span,
                column,
            })
    }
    pub(super) fn block(&mut self, enclosing_ctx: usize) -> SpannedResult<'a, Expr<'a>> {
        let (start, block_ctx, first_statement) = self
            .statement()
            .map(|v| (v.span.start, v.column, v))
            .or_else(|_| {
                if let Some(SpannedTok {
                    elem: Token::Newline(box Spanned { column, .. }),
                    span,
                    ..
                }) = self.newline()
                {
                    if column > enclosing_ctx {
                        return Ok((span.start, column, self.statement()?));
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
                    elem: Token::Newline(box Spanned { column, .. }),
                    span,
                    ..
                }) if *column == block_ctx => {
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

    fn expr(&mut self) -> SpannedResult<'a, Expr<'a>> {
        self.shunting_yard()
    }

    pub(super) fn if_then_else(&mut self) -> SpannedResult<'a, Expr<'a>> {
        let if_ctx = self.if_()?.column;
        let condition = self.block(if_ctx)?;
        match self.then() {
            Ok(_) => (),
            Err(_) => match self.newline() {
                Some(Spanned {
                    span,
                    elem:
                        Token::Newline(box Spanned {
                            elem: Token::Then,
                            column,
                            ..
                        }),
                    ..
                }) if column == if_ctx => {
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
            _ => match self.newline() {
                Some(Spanned {
                    span,
                    elem:
                        Token::Newline(box Spanned {
                            elem: Token::Else,
                            column,
                            ..
                        }),
                    ..
                }) if column == if_ctx => {
                    self.newline();
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

    pub(super) fn function_call(&mut self) -> SpannedResult<'a, Expr<'a>> {
        let function = self.ident()?;
        let fun_ctx = function.column;
        let mut params = vec![];
        let mut sub_ctx = None;
        loop {
            match self.argument() {
                Ok(v) => params.push(v),
                _ => match self.peek() {
                    Some(Spanned {
                        elem: Token::Newline(box Spanned { column, .. }),
                        span,
                        ..
                    }) if *column > fun_ctx => match sub_ctx {
                        Some(level) if level == *column => {
                            self.next();
                            params.push(self.argument()?);
                        }
                        Some(_) => {
                            return Err(ErrorInfo {
                                expected: Some(Expected::Named("A function argument")),
                                error: Error::LayoutError("All the argument placed in a function call sub context are supposed to be on the same column") ,
                                span: fun_ctx..span.end,
                            });
                        }
                        None => {
                            sub_ctx = Some(*column);
                            self.next();
                            params.push(self.argument()?);
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
    fn argument(&mut self) -> SpannedResult<'a, Expr<'a>> {
        self.literal().or_else(|_| self.parenthesized())
    }
    fn parenthesized(&mut self) -> SpannedResult<'a, Expr<'a>> {
        self.lparen()?;
        let expr = self.expr()?;
        self.rparen()?;
        Ok(expr)
    }
    fn ident(&mut self) -> SpannedResult<'a, Expr<'a>> {
        self.ident_token()
            .map(|Spanned { span, elem, column }| match elem {
                Token::Ident(i) => Spanned {
                    elem: Expr::Ident(Spanned {
                        span: span.clone(),
                        column: column,
                        elem: i,
                    }),
                    column: column,
                    span,
                },
                _ => unreachable!(),
            })
    }

    fn statement(&mut self) -> SpannedResult<'a, Statement<'a>> {
        self.expr()
            .map(|Spanned { span, elem, column }| Spanned {
                span,
                column,
                elem: Statement::StmtExpr(elem),
            })
            .or_else(|_| self.let_decl())
    }

    fn let_decl(&mut self) -> SpannedResult<'a, Statement<'a>> {
        let let_ = self.let_()?;
        let name = self.ident_string()?;
        self.equal()?;
        let value = self.expr()?;
        let end = value.span.end;
        Ok(Spanned {
            span: let_.span.start..end,
            column: let_.column,
            elem: Statement::Assignment(name, value),
        })
    }
    pub(super) fn literal(&mut self) -> SpannedResult<'a, Expr<'a>> {
        self.num()
            .or_else(|_| self.ident_token())
            .or_else(|_| self.bool())
            .map(|Spanned { span, elem, column }| match elem {
                Token::Num(n) => Spanned {
                    elem: Expr::Literal(Literal::Num(n)),
                    column,
                    span,
                },
                Token::Ident(s) => Spanned {
                    elem: Expr::Ident(Spanned {
                        span: span.clone(),
                        column,
                        elem: s,
                    }),
                    column,
                    span,
                },
                Token::Bool(b) => Spanned {
                    elem: Expr::Literal(Literal::Bool(b)),
                    column,
                    span,
                },
                _ => unreachable!(),
            })
    }

    fn ident_string(&mut self) -> SpannedResult<'a, Ident<'a>> {
        self.ident_token()
            .map(|Spanned { elem, span, column }| Spanned {
                span,
                column,
                elem: match elem {
                    Token::Ident(s) => s,
                    _ => unreachable!(),
                },
            })
    }

    fn newline(&mut self) -> Option<SpannedTok<'a>> {
        if let Some(Spanned {
            elem: Token::Newline(_),
            span,
            ..
        }) = self.peek()
        {
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
        v
    }

    pub(super) fn peek(&mut self) -> Option<&SpannedTok<'a>> {
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
    token!(equal, Token::Equal, "equal token");
    token!(if_, Token::If, "if token");
    token!(then, Token::Then, "then token");
    token!(else_, Token::Else, "else token");
    token!(let_, Token::Let, "let token");
    token!(num, Token::Num(_), "number");
    token!(ident_token, Token::Ident(_), "identifier");
    token!(bool, Token::Bool(_), "boolean");
    token!(operator, Token::Op(_), "operator");
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
                span: 4..39,
                column: 4,
                elem: Expr::Block {
                    instructions: vec![Spanned {
                        span: 4..39,
                        column: 4,
                        elem: Statement::StmtExpr(Expr::IfThenElse {
                            condition: Spanned {
                                span: 8..12,
                                column: 7,
                                elem: Box::new(Expr::Block {
                                    instructions: vec![Spanned {
                                        span: 8..12,
                                        column: 7,
                                        elem: Statement::StmtExpr(Expr::Literal(
                                            ast::Literal::Bool(true)
                                        ))
                                    }]
                                })
                            },
                            then_arm: Spanned {
                                span: 18..19,
                                column: 17,
                                elem: Box::new(Expr::Block {
                                    instructions: vec![Spanned {
                                        span: 18..19,
                                        column: 17,
                                        elem: Statement::StmtExpr(Expr::Literal(
                                            ast::Literal::Num("2")
                                        ))
                                    }]
                                })
                            },
                            else_arm: Some(Spanned {
                                span: 24..39,
                                column: 24,
                                elem: Box::new(Expr::Block {
                                    instructions: vec![Spanned {
                                        span: 24..39,
                                        column: 24,
                                        elem: Statement::StmtExpr(Expr::IfThenElse {
                                            condition: Spanned {
                                                span: 28..32,
                                                column: 27,
                                                elem: Box::new(Expr::Block {
                                                    instructions: vec![Spanned {
                                                        span: 28..32,
                                                        column: 27,
                                                        elem: Statement::StmtExpr(Expr::Literal(
                                                            ast::Literal::Bool(true)
                                                        ))
                                                    }]
                                                })
                                            },
                                            then_arm: Spanned {
                                                span: 38..39,
                                                column: 37,
                                                elem: Box::new(Expr::Block {
                                                    instructions: vec![Spanned {
                                                        span: 38..39,
                                                        column: 37,
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
                span: 4..82,
                column: 4,
                elem: Expr::Block {
                    instructions: vec![
                        Spanned {
                            span: 4..75,
                            column: 4,
                            elem: Statement::StmtExpr(Expr::IfThenElse {
                                condition: Spanned {
                                    span: 8..12,
                                    column: 7,
                                    elem: Box::new(Expr::Block {
                                        instructions: vec![{
                                            Spanned {
                                                span: 8..12,
                                                column: 7,
                                                elem: Statement::StmtExpr(Expr::Literal(
                                                    ast::Literal::Bool(true),
                                                )),
                                            }
                                        }]
                                    })
                                },
                                then_arm: Spanned {
                                    span: 22..34,
                                    column: 9,
                                    elem: Box::new(Expr::Block {
                                        instructions: vec![
                                            Spanned {
                                                span: 22..23,
                                                column: 9,
                                                elem: Statement::StmtExpr(Expr::Literal(
                                                    ast::Literal::Num("4"),
                                                ))
                                            },
                                            Spanned {
                                                span: 33..34,
                                                column: 9,
                                                elem: Statement::StmtExpr(Expr::Literal(
                                                    ast::Literal::Num("2"),
                                                ))
                                            }
                                        ]
                                    })
                                },
                                else_arm: Some(Spanned {
                                    span: 9..75,
                                    column: 9,
                                    elem: Box::new(Expr::Block {
                                        instructions: vec![Spanned {
                                            span: 9..75,
                                            column: 9,
                                            elem: Statement::StmtExpr(Expr::IfThenElse {
                                                condition: Spanned {
                                                    span: 47..51,
                                                    column: 12,
                                                    elem: Box::new(Expr::Block {
                                                        instructions: vec![Spanned {
                                                            span: 47..51,
                                                            column: 12,
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
                                                    column: 22,
                                                    elem: Box::new(Expr::Block {
                                                        instructions: vec![Spanned {
                                                            span: 57..58,
                                                            column: 22,
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
                                                    column: 14,
                                                    elem: Box::new(Expr::Block {
                                                        instructions: vec![Spanned {
                                                            span: 74..75,
                                                            column: 14,
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
                            column: 4,
                            elem: Statement::StmtExpr(Expr::Literal(ast::Literal::Num("15"),))
                        }
                    ]
                }
            }
        );
    }
}
