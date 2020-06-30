// #![feature(or_patterns)]
#![feature(box_patterns)]
mod errors;
mod syntax;
use syntax::{Lexer, Parser};

fn main() {
  let code1 = "
foo a b = 
          let a = foo a (1 + 2)
                    (bar 18)
                    (baz 15 12)
          if True
          then 4
          else
            if True
            then 2
            else 4
foo a b = 2
    ";
  let lexer = Lexer::new(code1);
  let tokens = lexer.tokenize();
  println!("{:#?}", tokens);
  let mut parser = Parser::new(tokens.unwrap().into_iter());
  println!("{:#?}", parser.function_decl(0));
}
