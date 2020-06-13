//#[feature(or_patterns)]
mod errors;
mod source_pos;
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
  if True
  then False
  else
    2
    4";
    let lexer = Lexer::new(code2);
    let tokens = lexer.tokenize();
    println!("{:#?}", tokens);
    let mut parser = Parser::new(tokens.unwrap().into_iter());
    parser.skip_while_indent();
    println!("{:#?}", parser.block());
}
