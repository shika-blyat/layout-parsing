use crate::{
    errors::err::*,
    syntax::{
        ast::*,
        parser::Parser,
        tokens::{Delimiter, Spanned, SpannedTok, Token},
    },
};
use phf::phf_map;
use std::convert::{TryFrom, TryInto};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    #[allow(unused)]
    pub(super) fn shunting_yard(&mut self) -> SpannedResult<'a, Expr<'a>> {
        let mut op_stack: Vec<Spanned<Operator<'_>>> = vec![];
        let mut ast_stack = vec![];
        self.push_atom(&mut ast_stack, &mut op_stack)?;
        loop {
            match self.bin_op_or_rparen() {
                Ok(op) if op.elem.sym == ")" => {
                    loop {
                        match op_stack.last() {
                            Some(last_op) if last_op.elem.sym == "(" => {
                                op_stack.pop().unwrap();
                                break;
                            }
                            Some(last_op) if last_op.elem.is_infix() => {
                                let op = op_stack.pop().unwrap();
                                Self::push_infix(&mut ast_stack, op);
                            }
                            Some(last_op) if last_op.elem.is_prefix() => {
                                let op = op_stack.pop().unwrap();
                                Self::push_prefix(&mut ast_stack, op);
                            }
                            _ => todo!("raise an error"),
                        }
                    }
                    continue;
                }
                Ok(op) => {
                    loop {
                        match op_stack.last() {
                            Some(last_op)
                                if last_op.elem.is_infix()
                                    && last_op.elem.has_bigger_prec(&op.elem) =>
                            {
                                let op = op_stack.pop().unwrap();
                                Self::push_infix(&mut ast_stack, op);
                            }
                            Some(last_op) if last_op.elem.is_prefix() => {
                                let op = op_stack.pop().unwrap();
                                Self::push_prefix(&mut ast_stack, op);
                            }
                            _ => break,
                        }
                    }
                    op_stack.push(op)
                }
                _ => break,
            }
            self.push_atom(&mut ast_stack, &mut op_stack)?;
        }
        for op in op_stack.into_iter().rev() {
            if op.elem.is_infix() {
                Self::push_infix(&mut ast_stack, op);
            } else if op.elem.is_prefix() {
                Self::push_prefix(&mut ast_stack, op);
            }
        }
        return Ok(ast_stack.into_iter().next().unwrap());
    }
    fn push_infix(ast: &mut Vec<Spanned<Expr<'a>>>, op: Spanned<Operator<'a>>) {
        let right = ast.pop().unwrap();
        let left = ast.pop().unwrap();
        ast.push(Spanned {
            span: left.span.start..right.span.end,
            column: left.column,
            elem: Expr::Binary(op.elem.try_into().unwrap(), left.into(), right.into()),
        })
    }

    fn push_prefix(ast: &mut Vec<Spanned<Expr<'a>>>, op: Spanned<Operator<'a>>) {
        let right = ast.pop().unwrap();
        ast.push(Spanned {
            span: op.span.start..right.span.end,
            column: op.column,
            elem: Expr::Unary(op.elem.try_into().unwrap(), right.into()),
        })
    }
    fn bin_op_or_rparen(&mut self) -> SpannedResult<'a, Operator<'a>> {
        self.operator()
            .or_else(|_| self.rparen())
            .map(|v| match v {
                Spanned {
                    elem: Token::Op(s),
                    span,
                    column,
                } if BINARY_OPERATOR_TABLE.contains_key(s) => Ok(Spanned {
                    span,
                    elem: BINARY_OPERATOR_TABLE[s],
                    column,
                }),
                Spanned {
                    elem: Token::Delimiter(Delimiter::RParen),
                    span,
                    column,
                } => Ok(Spanned {
                    span,
                    elem: Operator {
                        prec: 0,
                        fixity: Fixity::None,
                        sym: ")",
                    },
                    column,
                }),
                SpannedTok { span, elem, .. } => Err(ErrorInfo {
                    span,
                    expected: Some(Expected::Named("a binary operator")),
                    error: Error::UnexpectedTok(elem),
                }),
            })?
    }
    fn un_op(&mut self) -> SpannedResult<'a, Operator<'a>> {
        self.operator().map(|v| match v {
            Spanned {
                span,
                elem: Token::Op(s),
                column,
            } if UNARY_OPERATOR_TABLE.contains_key(s) => Ok(Spanned {
                span,
                elem: UNARY_OPERATOR_TABLE[s],
                column,
            }),
            SpannedTok { span, elem, .. } => Err(ErrorInfo {
                span,
                expected: Some(Expected::Named("an unary operator")),
                error: Error::UnexpectedTok(elem),
            }),
        })?
    }
    fn push_atom(
        &mut self,
        ast_stack: &mut Vec<Spanned<Expr<'a>>>,
        op_stack: &mut Vec<Spanned<Operator<'a>>>,
    ) -> Result<(), ErrorInfo<'a>> {
        while let Ok(op) = self.un_op() {
            op_stack.push(op);
        }
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Fixity {
    None,
    Prefix,
    Infix(Assoc),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Assoc {
    Right,
    Left,
}
impl Assoc {
    pub fn is_left(&self) -> bool {
        self == &Assoc::Left
    }
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Operator<'a> {
    pub sym: &'a str,
    pub fixity: Fixity,
    pub prec: u8,
}
impl<'a> Operator<'a> {
    pub fn has_bigger_prec(&self, op: &Operator<'_>) -> bool {
        self.prec > op.prec || (self.prec == op.prec && op.is_left_assoc())
    }
    pub fn is_infix(&self) -> bool {
        self.fixity != Fixity::Prefix
    }
    pub fn is_prefix(&self) -> bool {
        self.fixity == Fixity::Prefix
    }
    pub fn into_prefix(self) -> Option<Self> {
        match self.fixity {
            Fixity::Prefix => Some(self),
            Fixity::Infix(_) => Some(Self {
                fixity: Fixity::Prefix,
                prec: match self.sym {
                    "+" | "-" => 25,
                    _ => return None,
                },
                ..self
            }),
            Fixity::None => None,
        }
    }
    pub fn is_left_assoc(&self) -> bool {
        match self.fixity {
            Fixity::Infix(assoc) => assoc.is_left(),
            _ => false,
        }
    }
}
impl<'a> TryFrom<Operator<'a>> for BinOp {
    type Error = ();
    fn try_from(op: Operator<'a>) -> Result<BinOp, ()> {
        Ok(match op.sym {
            "+" => match op.fixity {
                Fixity::Prefix | Fixity::None => return Err(()),
                Fixity::Infix(_) => BinOp::Add,
            },
            "-" => match op.fixity {
                Fixity::Prefix | Fixity::None => return Err(()),
                Fixity::Infix(_) => BinOp::Sub,
            },
            "*" => BinOp::Mul,
            "/" => BinOp::Div,
            "&&" => BinOp::And,
            "||" => BinOp::Or,
            ">=" => BinOp::GTE,
            ">" => BinOp::GT,
            "<" => BinOp::LT,
            "<=" => BinOp::LTE,
            "==" => BinOp::EqEq,
            "!=" => BinOp::NotEq,
            _ => unreachable!(),
        })
    }
}
impl<'a> TryFrom<Operator<'a>> for UnOp {
    type Error = ();
    fn try_from(op: Operator<'a>) -> Result<UnOp, ()> {
        Ok(match op.sym {
            "+" => match op.fixity {
                Fixity::Prefix => UnOp::Pos,
                Fixity::Infix(_) | Fixity::None => return Err(()),
            },
            "-" => match op.fixity {
                Fixity::Prefix => UnOp::Neg,
                Fixity::Infix(_) | Fixity::None => return Err(()),
            },
            "!" => UnOp::Neg,
            _ => return Err(()),
        })
    }
}

const BINARY_OPERATOR_TABLE: phf::Map<&'static str, Operator> = phf_map! {
    "+"  => Operator {
        prec: 10,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "+",
    },
    "-" => Operator {
        prec: 10,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "-",
    },
    "/" => Operator {
        prec: 20,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "/",
    },
    "*" => Operator {
        prec: 20,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "*",
    },
    "==" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "==",
    },
    "!=" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "!=",
    },
    ">" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: ">",
    },
    "<" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "<",
    },
    ">=" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: ">=",
    },
    "<=" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "<=",
    },
    "&&" => Operator {
        prec: 7,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "&&",
    },
    "||" => Operator {
        prec: 6,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "||",
    },
};

const UNARY_OPERATOR_TABLE: phf::Map<&'static str, Operator> = phf_map! {
    "+" => Operator {
        prec: 25,
        fixity: Fixity::Prefix,
        sym: "+",
    },
    "-" => Operator {
        prec: 25,
        fixity: Fixity::Prefix,
        sym: "-",
    },
    "!" => Operator {
        prec: 30,
        fixity: Fixity::Prefix,
        sym: "!",
    },
};
