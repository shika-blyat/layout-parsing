// TODO Add modules
use super::tokens::Spanned;

pub type BoxSpanned<T> = Spanned<Box<T>>;
pub type Ident<'a> = &'a str;

#[derive(Debug, PartialEq, Clone)]
pub struct Module<'a> {
    pub items: Vec<Spanned<Item<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item<'a> {
    Function(FunctionDecl<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDecl<'a> {
    pub name: Spanned<Ident<'a>>,
    pub arguments: Vec<Spanned<Pattern<'a>>>,
    pub body: Spanned<Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<'a> {
    Named(Ident<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Ident(Spanned<Ident<'a>>),
    Unary(UnOp, BoxSpanned<Expr<'a>>),
    Binary(BinOp, BoxSpanned<Expr<'a>>, BoxSpanned<Expr<'a>>),
    #[allow(unused)]
    Lambda(Ident<'a>, BoxSpanned<Expr<'a>>),
    Call(BoxSpanned<Expr<'a>>, Vec<Spanned<Expr<'a>>>),
    IfThenElse {
        condition: BoxSpanned<Expr<'a>>,
        then_arm: BoxSpanned<Expr<'a>>,
        else_arm: Option<BoxSpanned<Expr<'a>>>,
    },
    Block {
        instructions: Vec<Spanned<Statement<'a>>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    #[allow(unused)]
    Return(Expr<'a>),
    Assignment(Spanned<Ident<'a>>, Spanned<Expr<'a>>),
    StmtExpr(Expr<'a>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    NotEq,
    EqEq,
    LT,
    LTE,
    GT,
    GTE,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOp {
    Pos,
    Neg,
    #[allow(unused)]
    Not,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Literal<'a> {
    Str(&'a str),
    Num(&'a str),
    Bool(bool),
    #[allow(unused)]
    Unit,
}
