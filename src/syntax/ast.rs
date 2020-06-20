// TODO Add modules
use std::ops::Range;

pub type BoxSpanned<T> = ExprSpan<Box<T>>;
pub type Ident<'a> = &'a str;

#[derive(Debug, PartialEq, Clone)]
pub struct ExprSpan<T> {
    pub span: Range<usize>,
    pub column: usize,
    pub elem: T,
}

impl<T> From<ExprSpan<T>> for ExprSpan<Box<T>> {
    fn from(b: ExprSpan<T>) -> ExprSpan<Box<T>> {
        ExprSpan {
            elem: Box::new(b.elem),
            span: b.span,
            column: b.column,
        }
    }
}

#[allow(unused)]
#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Ident(ExprSpan<Ident<'a>>),
    Unary(UnOp, BoxSpanned<Expr<'a>>),
    Binary(BinOp, BoxSpanned<Expr<'a>>, BoxSpanned<Expr<'a>>),
    Lambda(Ident<'a>, BoxSpanned<Expr<'a>>),
    Call(BoxSpanned<Expr<'a>>, Vec<ExprSpan<Expr<'a>>>),
    IfThenElse {
        condition: BoxSpanned<Expr<'a>>,
        then_arm: BoxSpanned<Expr<'a>>,
        else_arm: Option<BoxSpanned<Expr<'a>>>,
    },
    Block {
        instructions: Vec<ExprSpan<Statement<'a>>>,
    },
}

#[allow(unused)]
#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Return(Expr<'a>),
    Continue,
    Break(Expr<'a>),
    StmtExpr(Expr<'a>),
}

#[allow(unused)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    LT,
    LTE,
    GT,
    GTE,
}
#[allow(unused)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}

#[allow(unused)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Literal<'a> {
    Num(&'a str),
    Bool(bool),
    Unit,
}
