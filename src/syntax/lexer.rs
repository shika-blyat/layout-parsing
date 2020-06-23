use std::{convert::TryFrom, iter::Peekable, str::Chars};

use crate::{
    errors::err::*,
    syntax::tokens::{Spanned, SpannedTok, Token},
};

macro_rules! many {
    ($name: ident, $predicate: expr, $token: path) => {
        pub fn $name(&mut self, start: usize) -> SpannedTok<'a> {
            let mut length = 1;
            while let Some(_) = self.next_if($predicate) {
                length += 1
            }
            let span = start..start + length;
            let s = &self.source[span.clone()];
            return Spanned {
                elem: Token::try_from(s).unwrap_or($token(s)),
                span,
                column: start - self.last_newline,
            };
        }
    };
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    byte_pos: usize,
    last_newline: usize,
}
impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            byte_pos: 0,
            last_newline: 0,
        }
    }
    pub fn tokenize(mut self) -> Result<Vec<SpannedTok<'a>>, ErrorInfo<'a>> {
        let mut tokens = vec![];
        let mut last_indent = None;
        while let Some(char) = self.next() {
            match char {
                c if c.is_ascii_digit() => tokens.push(self.num(self.byte_pos - 1)),
                c if c.is_alphabetic() => tokens.push(self.ident(self.byte_pos - 1)),
                '"' => tokens.push(self.string(self.byte_pos)?),
                '(' | ')' | '{' | '}' | '+' | '-' | '*' | '/' => tokens.push(SpannedTok {
                    column: self.byte_pos - self.last_newline - 1,
                    span: self.byte_pos - 1..self.byte_pos,
                    elem: Token::try_from(&self.source[self.byte_pos - 1..self.byte_pos]).unwrap(),
                }),
                '=' | '!' => match self.peek() {
                    Some('=') => {
                        self.next();
                        tokens.push(SpannedTok {
                            column: self.byte_pos - self.last_newline - 2,
                            elem: Token::Op(&self.source[self.byte_pos - 1..self.byte_pos + 1]),
                            span: self.byte_pos - 1..self.byte_pos + 1,
                        })
                    }
                    _ => tokens.push(SpannedTok {
                        column: self.byte_pos - self.last_newline - 1,
                        elem: Token::Op(&self.source[self.byte_pos - 1..self.byte_pos]),
                        span: self.byte_pos - 1..self.byte_pos,
                    }),
                },
                '&' | '|' => match self.peek() {
                    Some(c) if c == &char => {
                        let c = self.next().unwrap();
                        tokens.push(SpannedTok {
                            column: self.byte_pos - 2 - self.last_newline,
                            elem: Token::Op(
                                &self.source[self.byte_pos..self.byte_pos + c.len_utf8()],
                            ),
                            span: self.byte_pos - 1..self.byte_pos + 1,
                        })
                    }
                    _ => tokens.push(SpannedTok {
                        column: self.byte_pos - self.last_newline - 1,
                        elem: Token::Op(&self.source[self.byte_pos - 1..self.byte_pos]),
                        span: self.byte_pos - 1..self.byte_pos,
                    }),
                },
                '\n' => {
                    self.last_newline = self.byte_pos;
                    if tokens.is_empty() {
                        continue;
                    }
                    let mut counter = 1;
                    while let Some(' ') = self.peek() {
                        self.next();
                        counter += 1;
                    }
                    if let Some(c) = self.peek() {
                        if *c == '\n' {
                            continue;
                        }
                        last_indent = Some((counter, self.byte_pos - counter..self.byte_pos));
                    }
                    continue;
                }
                ' ' | '\t' | '\r' => (),
                _ => {
                    return Err(ErrorInfo {
                        span: self.byte_pos - 1..self.byte_pos,
                        error: Error::UnexpectedChar(char),
                        expected: None,
                    })
                }
            }
            if let Some((size, ref span)) = last_indent {
                let last_tok = tokens.pop().unwrap();
                tokens.push(SpannedTok {
                    column: self.byte_pos - self.last_newline - 1,
                    elem: Token::Indent(size, Box::new(last_tok.clone())),
                    span: span.clone(),
                });
                tokens.push(last_tok);
                last_indent = None;
            }
        }
        tokens.push(SpannedTok {
            column: self.byte_pos - self.last_newline,
            span: self.byte_pos - 1..self.byte_pos, //codespan will take care of this, and even if the range goes out of the string, it will just display the range we're targeting
            //as the final element of the string
            elem: Token::EOF,
        });
        Ok(tokens)
    }
    pub fn string(&mut self, start: usize) -> Result<SpannedTok<'a>, ErrorInfo<'a>> {
        let mut length = 0;
        loop {
            match self.next() {
                Some('"') => break,
                Some(c) => length += c.len_utf8(),
                None => {
                    return Err(ErrorInfo {
                        span: start..start + length,
                        error: Error::UnclosedStringLiteral,
                        expected: Some(Expected::Named("an enclosing quote")),
                    })
                }
            }
        }
        self.next();
        let span = start - 1..start + length;
        return Ok(Spanned {
            column: self.byte_pos - length - self.last_newline,
            elem: Token::Str(&self.source[span.clone()]),
            span,
        });
    }

    many!(num, |c| c.is_ascii_digit(), Token::Num);
    many!(ident, |c| c.is_alphanumeric(), Token::Ident);

    pub fn next_if(&mut self, predicate: impl Fn(char) -> bool) -> Option<char> {
        if predicate(*self.peek()?) {
            return self.next();
        }
        None
    }
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }
}
impl Iterator for Lexer<'_> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.byte_pos += c.len_utf8();
            return Some(c);
        }
        None
    }
}
