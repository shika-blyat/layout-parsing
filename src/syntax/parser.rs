use std::iter::Peekable;

use super::{
    ast::*,
    tokens::{Delimiter, SpannedTok, Token},
};

macro_rules! token {
    ($name: ident, $p: pat) => {
        pub fn $name(&mut self) -> Option<SpannedTok<'a>> {
            if let Some(Spanned { elem: $p, .. }) = self.peek() {
                return self.next();
            }
            None
        }
    };
}

pub struct Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    tokens: Peekable<I>,
    context_stack: Vec<usize>,
    last_newline: usize,
}
impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            context_stack: vec![],
            last_newline: 0,
        }
    }
    pub fn skip_while_indent(&mut self) {
        while let Some(Spanned {
            elem: Token::Ident(_),
            ..
        }) = self.indentation()
        {}
    }
    pub fn if_then_else(&mut self) -> Option<Spanned<Expr<'a>>> {
        let start = self.if_()?.span.start;
        let if_context = start - self.last_newline;
        let condition = self.expr(None).expect("Expected a condition");
        let changed_line = match self.then().or_else(|| self.indentation()) {
            Some(Spanned { span, elem }) => match elem {
                Token::Indent(n) => {
                    if if_context != n {
                        panic!("the then block is expected to be as indented as much as the if")
                    }
                    self.then().expect("expected a then block");
                    true
                }
                Token::Then => false,
                _ => unreachable!(),
            },
            None => panic!("Expected a then block after an if"),
        };
        let then_branch = self
            .expr(None)
            .expect("Expected an expression after the then block");
        let else_branch = match self.else_().or_else(|| self.indentation()) {
            Some(Spanned { span, elem }) => match elem {
                Token::Indent(n) => {
                    println!("{} {} {:#?}", if_context, n, then_branch);
                    if if_context != n {
                        panic!("the else block is expected to be as indented as much as the if")
                    }
                    self.else_().expect("expected a then block");
                    Some(
                        self.expr(None)
                            .expect("Expected an expression after the else block"),
                    )
                }
                Token::Else => {
                    if changed_line && span.start != if_context {
                        panic!("the then block is expected to be as indented as the if")
                    }
                    Some(
                        self.expr(None)
                            .expect("Expected an expression after the else block"),
                    )
                }
                _ => unreachable!(),
            },
            None => None,
        };
        Some(Spanned {
            elem: Expr::IfThenElse {
                condition: condition.into(),
                then_branch: then_branch.into(),
                else_branch: else_branch.map(|e| e.into()),
            },
            span: start..0,
        })
    }
    fn expr(&mut self, start: Option<usize>) -> Option<Spanned<Expr<'a>>> {
        self.block(start).or_else(|| self.expr_())
    }
    fn expr_(&mut self) -> Option<Spanned<Expr<'a>>> {
        self.identifier()
            .or_else(|| self.num())
            .or_else(|| self.bool())
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
            .or_else(|| self.if_then_else())
    }
    fn block(&mut self, mut start: Option<usize>) -> Option<Spanned<Expr<'a>>> {
        let mut instructions = match self.indentation() {
            Some(Spanned {
                elem: Token::Indent(n),
                ..
            }) => {
                start = Some(n);
                vec![self.statement().expect("Expected a statement")]
            }
            _ => match self.statement() {
                Some(Spanned { elem, span }) => {
                    if start.is_none() {
                        println!("{:#?}", elem);
                        println!("{} {}", span.start, self.last_newline);
                        start = Some(span.start - self.last_newline);
                    }

                    vec![Spanned {
                        elem,
                        span: span.clone(),
                    }]
                }
                None => panic!("expected an expression"),
            },
        };
        let start = start.unwrap();
        while let Some(n) = self.indent_lvl(|&n| n >= start) {
            if n > start {
                todo!("need to handle this")
            }
            instructions.push(self.statement().expect("expected a statement"))
        }
        let span = 0..0;
        Some(Spanned {
            elem: Expr::Block { instructions },
            span: start..0,
        })
    }
    pub fn statement(&mut self) -> Option<Spanned<Statement<'a>>> {
        self.expr_().map(|Spanned { span, elem }| Spanned {
            span,
            elem: Statement::StmtExpr(elem),
        })
    }
    pub fn indent_lvl(&mut self, predicate: impl Fn(&usize) -> bool) -> Option<usize> {
        if let Some(Spanned {
            elem: Token::Indent(n),
            ..
        }) = self.peek()
        {
            if predicate(n) {
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
    pub fn indentation(&mut self) -> Option<SpannedTok<'a>> {
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
    token!(lparen, Token::Delimiter(Delimiter::LParen));
    token!(rparen, Token::Delimiter(Delimiter::RParen));
    token!(lbrace, Token::Delimiter(Delimiter::LBrace));
    token!(rbrace, Token::Delimiter(Delimiter::RBrace));
    token!(if_, Token::If);
    token!(then, Token::Then);
    token!(else_, Token::Else);
    token!(num, Token::Num(_));
    token!(identifier, Token::Ident(_));
    token!(bool, Token::Bool(_));
    fn next(&mut self) -> Option<SpannedTok<'a>> {
        self.tokens.next()
    }
    fn peek(&mut self) -> Option<&SpannedTok<'a>> {
        self.tokens.peek()
    }
}
