// #![feature(or_patterns)]
#![feature(box_patterns)]
mod errors;
mod syntax;
use syntax::{Lexer, Parser};

fn main() {
    let _code1 = "
    foo = 2
          if True
          then 4
          else
            if True
            then 2
            else 4
    a = 2
    ";
    let code2 = "
    if True then 2 else if True then 3 else 4
";
    let lexer = Lexer::new(code2);
    let tokens = lexer.tokenize();
    println!("{:#?}", tokens);
    let mut parser = Parser::new(tokens.unwrap().into_iter());
    println!("{:#?}", parser.block(0));
}
