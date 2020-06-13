#[allow(unused)]
use crate::{
    errors::err::*,
    syntax::{
        ast::Expr,
        parser::Parser,
        tokens::{SpannedTok, Token},
    },
};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = SpannedTok<'a>>,
{
    #[allow(unused)]
    fn shunting_yard(&mut self) -> Result<Expr<'a>, ErrorInfo<'a>> {
        unimplemented!()
    }
}
