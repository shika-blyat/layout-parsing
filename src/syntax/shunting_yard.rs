use crate::{
    errors::err::*,
    syntax::{
        ast::{Expr, Literal},
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
    pub(super) fn shunting_yard(&mut self) -> SpannedResult<'a, Expr<'a>> {
        let mut op_stack: Vec<Spanned<Operator<'_>>> = vec![];
        let mut ast_stack = vec![];
        self.push_atom(&mut ast_stack, &mut op_stack)?;
        loop {
            match self.bin_op_or_rparen() {
                Some(op) if op.elem.sym == ")" => loop {
                    match op_stack.last() {
                        Some(last_op) if last_op.elem.sym == "(" => {
                            op_stack.pop().unwrap();
                            break;
                        }
                        Some(last_op) if last_op.elem.is_infix() || last_op.elem.is_prefix() => {
                            let op = op_stack.pop().unwrap();
                            Self::push_op(&mut ast_stack, op);
                        }
                        _ => todo!("raise an error"),
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

    fn bin_op_or_rparen(&mut self) -> Option<Spanned<Operator<'a>>> {
        self.operator()
            .or_else(|_| self.rparen())
            .ok()
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
            })
        }
        ast_stack.push(self.atom()?);
        Ok(())
    }

    fn atom(&mut self) -> SpannedResult<'a, Expr<'a>> {
        self.num()
            .or_else(|_| self.bool())
            .map(|Spanned { span, elem, column }| match elem {
                Token::Num(n) => Spanned {
                    elem: Expr::Literal(Literal::Num(n)),
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
            .or_else(|_| self.if_then_else())
            .or_else(|_| self.function_call())
    }
}
