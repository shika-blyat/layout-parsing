// TODO Add modules
pub use crate::source_pos::Spanned;

pub type BoxSpanned<T> = Spanned<Box<T>>;
pub type Ident<'a> = &'a str;

#[allow(unused)]
#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Literal(Spanned<Literal<'a>>),
    Ident(Spanned<Ident<'a>>),
    Unary(UnOp, BoxSpanned<Expr<'a>>),
    Binary(BinOp, BoxSpanned<Expr<'a>>, BoxSpanned<Expr<'a>>),
    Lambda(Ident<'a>, BoxSpanned<Expr<'a>>),
    Call(BoxSpanned<Expr<'a>>, Vec<Spanned<Expr<'a>>>),
    IfThenElse {
        condition: BoxSpanned<Expr<'a>>,
        then_branch: BoxSpanned<Expr<'a>>,
        else_branch: Option<BoxSpanned<Expr<'a>>>,
    },
    Block {
        instructions: Vec<Spanned<Statement<'a>>>,
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
