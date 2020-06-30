use crate::{
    errors::err::*,
    syntax::{
        ast::Expr,
        operators::{Fixity, Operator, BINARY_OPERATOR_TABLE, UNARY_OPERATOR_TABLE},
        parser::Parser,
        tokens::{Delimiter, Spanned, SpannedTok, Token},
    },
};
use std::convert::TryInto;
impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    // Some kind of revisited shunting yard
    // There's a fairly high probability I forgot something and the parser will break with no apparent reason
    pub(super) fn shunting_yard(&mut self) -> SpannedResult<'a, Expr<'a>> {
        let mut op_stack: Vec<Spanned<Operator<'_>>> = vec![];
        let mut ast_stack = vec![];
        self.push_atom(&mut ast_stack, &mut op_stack)?;
        'outer: loop {
            match self.bin_op_or_peek_rparen() {
                Some(op) if op.elem.sym == ")" => loop {
                    match op_stack.last() {
                        Some(last_op) if last_op.elem.sym == "(" => {
                            let _ = self.rparen();
                            op_stack.pop().unwrap();
                            break;
                        }
                        Some(last_op) if last_op.elem.is_infix() || last_op.elem.is_prefix() => {
                            let op = op_stack.pop().unwrap();
                            Self::push_op(&mut ast_stack, op);
                        }
                        _ => break 'outer,
                    }
                },
                Some(op) => {
                    loop {
                        match op_stack.last() {
                            Some(last_op)
                                if (last_op.elem.is_infix()
                                    && last_op.elem.has_bigger_prec(&op.elem))
                                    || last_op.elem.is_prefix() =>
                            {
                                let op = op_stack.pop().unwrap();
                                Self::push_op(&mut ast_stack, op);
                            }
                            _ => break,
                        }
                    }
                    op_stack.push(op);
                    self.push_atom(&mut ast_stack, &mut op_stack)?;
                }
                _ => break,
            }
        }
        for op in op_stack.into_iter().rev() {
            if op.elem.is_infix() || op.elem.is_prefix() {
                Self::push_op(&mut ast_stack, op);
            }
        }
        return Ok(ast_stack.into_iter().next().unwrap());
    }

    fn push_op(ast: &mut Vec<Spanned<Expr<'a>>>, op: Spanned<Operator<'a>>) {
        let right = ast.pop().unwrap();
        match op.elem.fixity {
            Fixity::Infix(_) => {
                let left = ast.pop().unwrap();
                ast.push(Spanned {
                    span: left.span.start..right.span.end,
                    column: left.column,
                    elem: Expr::Binary(op.elem.try_into().unwrap(), left.into(), right.into()),
                })
            }
            Fixity::Prefix => ast.push(Spanned {
                span: op.span.start..right.span.end,
                column: op.column,
                elem: Expr::Unary(op.elem.try_into().unwrap(), right.into()),
            }),
            Fixity::None => unreachable!(),
        }
    }

    fn bin_op_or_peek_rparen(&mut self) -> Option<Spanned<Operator<'a>>> {
        self.operator()
            .ok()
            .or_else(|| {
                if let rparen
                @
                Some(Spanned {
                    elem: Token::Delimiter(Delimiter::RParen),
                    ..
                }) = self.peek()
                {
                    return rparen.map(|v| v.clone());
                }
                None
            })
            .map(|v| match v {
                Spanned { elem, span, column } => Some(Spanned {
                    span,
                    elem: match elem {
                        Token::Op(s) => *BINARY_OPERATOR_TABLE.get(s)?,
                        Token::Delimiter(Delimiter::RParen) => Operator {
                            prec: 0,
                            fixity: Fixity::None,
                            sym: ")",
                        },
                        _ => unreachable!(),
                    },
                    column,
                }),
            })?
    }

    fn un_op(&mut self) -> Option<Spanned<Operator<'a>>> {
        let v = self.operator().ok()?;
        if let Spanned {
            span,
            elem: Token::Op(s),
            column,
        } = v
        {
            return Some(Spanned {
                span,
                elem: *UNARY_OPERATOR_TABLE.get(s)?,
                column,
            });
        }
        unreachable!()
    }

    fn push_atom(
        &mut self,
        ast_stack: &mut Vec<Spanned<Expr<'a>>>,
        op_stack: &mut Vec<Spanned<Operator<'a>>>,
    ) -> Result<(), ErrorInfo<'a>> {
        op_stack.extend(std::iter::from_fn(|| self.un_op()));
        if let Ok(Spanned { span, column, .. }) = self.lparen() {
            op_stack.push(Spanned {
                span,
                column,
                elem: Operator {
                    prec: 0,
                    fixity: Fixity::None,
                    sym: "(",
                },
            });
            self.push_atom(ast_stack, op_stack)?;
            return Ok(());
        }
        ast_stack.push(self.atom()?);
        Ok(())
    }

    fn atom(&mut self) -> SpannedResult<'a, Expr<'a>> {
        self.function_call()
            .or_else(|_| self.if_then_else())
            .or_else(|_| self.literal())
    }
}

// TESTS
#[allow(unused)]
mod test {
    use crate::syntax::*;
    use pretty_assertions::assert_eq;
    #[test]
    fn complex_operation() {
        let code = "    (1 + 2) * +-(if True then 4 else 3) - 1";
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer.tokenize().unwrap().into_iter());
        let result = parser.block(0).unwrap();
        assert_eq!(
            result,
            Spanned {
                span: 5..43,
                column: 5,
                elem: Expr::Block {
                    instructions: vec![Spanned {
                        span: 5..43,
                        column: 5,
                        elem: Statement::StmtExpr(Expr::Binary(
                            BinOp::Sub,
                            Spanned {
                                span: 5..38,
                                column: 5,
                                elem: Box::new(Expr::Binary(
                                    BinOp::Mul,
                                    Spanned {
                                        span: 5..10,
                                        column: 5,
                                        elem: Box::new(Expr::Binary(
                                            BinOp::Add,
                                            Spanned {
                                                elem: Box::new(Expr::Literal(Literal::Num("1"))),
                                                span: 5..6,
                                                column: 5
                                            },
                                            Spanned {
                                                elem: Box::new(Expr::Literal(Literal::Num("2"))),
                                                span: 9..10,
                                                column: 9
                                            }
                                        )),
                                    },
                                    Spanned {
                                        span: 14..38,
                                        column: 14,
                                        elem: Box::new(Expr::Unary(
                                            UnOp::Pos,
                                            Spanned {
                                                span: 15..38,
                                                column: 15,
                                                elem: Box::new(Expr::Unary(
                                                    UnOp::Neg,
                                                    Spanned {
                                                        span: 17..38,
                                                        column: 17,
                                                        elem: Box::new(Expr::IfThenElse {
                                                            condition: Spanned {
                                                                span: 20..24,
                                                                column: 20,
                                                                elem: Box::new(Expr::Block {
                                                                    instructions: vec![Spanned {
                                                                        span: 20..24,
                                                                        column: 20,
                                                                        elem: Statement::StmtExpr(
                                                                            Expr::Literal(
                                                                                Literal::Bool(true)
                                                                            )
                                                                        ),
                                                                    }]
                                                                })
                                                            },
                                                            then_arm: Spanned {
                                                                span: 30..31,
                                                                column: 30,
                                                                elem: Box::new(Expr::Block {
                                                                    instructions: vec![Spanned {
                                                                        span: 30..31,
                                                                        column: 30,
                                                                        elem: Statement::StmtExpr(
                                                                            Expr::Literal(
                                                                                Literal::Num("4")
                                                                            )
                                                                        ),
                                                                    }]
                                                                })
                                                            },
                                                            else_arm: Some(Spanned {
                                                                span: 37..38,
                                                                column: 37,
                                                                elem: Box::new(Expr::Block {
                                                                    instructions: vec![Spanned {
                                                                        span: 37..38,
                                                                        column: 37,
                                                                        elem: Statement::StmtExpr(
                                                                            Expr::Literal(
                                                                                Literal::Num("3")
                                                                            )
                                                                        ),
                                                                    }]
                                                                })
                                                            })
                                                        }),
                                                    }
                                                ))
                                            }
                                        ))
                                    }
                                )),
                            },
                            Spanned {
                                elem: Box::new(Expr::Literal(Literal::Num("1"))),
                                span: 42..43,
                                column: 42,
                            }
                        ))
                    }]
                }
            }
        )
    }
}
