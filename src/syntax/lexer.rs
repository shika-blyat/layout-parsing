use std::{convert::TryFrom, iter::Peekable, str::CharIndices};

use crate::{
    errors::err::*,
    syntax::tokens::{Spanned, SpannedTok, Token},
};

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
}
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
            };
        }
    };
}
impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
        }
    }
    pub fn tokenize(mut self) -> Result<Vec<SpannedTok<'a>>, ErrorInfo<'a>> {
        let mut tokens = vec![];
        let mut last_indent = None;
        while let Some((pos, char)) = self.next() {
            match char {
                c if c.is_ascii_digit() => tokens.push(self.num(pos)),
                c if c.is_alphabetic() => tokens.push(self.ident(pos)),
                '"' => tokens.push(self.string(pos)?),
                '(' | ')' | '{' | '}' | '+' | '-' | '*' | '/' => tokens.push(SpannedTok {
                    span: pos..pos + 1,
                    elem: Token::try_from(&self.source[pos..pos + 2]).unwrap(),
                }),
                '=' | '!' => match self.peek() {
                    Some((_, '=')) => {
                        self.next();
                        tokens.push(SpannedTok {
                            elem: Token::Op(&self.source[pos..pos + 2]),
                            span: pos..pos + 2,
                        })
                    }
                    _ => tokens.push(SpannedTok {
                        elem: Token::Op(&self.source[pos..pos + 1]),
                        span: pos..pos + 1,
                    }),
                },
                '&' | '|' => match self.peek() {
                    Some((_, c)) if c == &char => {
                        self.next();
                        tokens.push(SpannedTok {
                            elem: Token::Op(&self.source[pos..pos + 2]),
                            span: pos..pos + 2,
                        })
                    }
                    _ => tokens.push(SpannedTok {
                        elem: Token::Op(&self.source[pos..pos + 1]),
                        span: pos..pos + 1,
                    }),
                },
                '\n' => {
                    if tokens.is_empty() {
                        continue;
                    }
                    let mut counter = 1;
                    while let Some((_, ' ')) = self.peek() {
                        self.next();
                        counter += 1;
                    }
                    if let Some((pos, c)) = self.peek() {
                        if *c == '\n' {
                            continue;
                        }
                        last_indent = Some((counter, pos - counter..*pos));
                    }
                    continue;
                }
                ' ' | '\t' | '\r' => (),
                c => {
                    return Err(ErrorInfo {
                        span: pos..pos + 1,
                        error: Error::UnexpectedChar(char),
                        expected: None,
                        found: Some(Found::Char(c)),
                    })
                }
            }
            if let Some((size, ref span)) = last_indent {
                let last_tok = tokens.pop().unwrap();
                tokens.push(SpannedTok {
                    elem: Token::Indent(size, Box::new(last_tok.clone())),
                    span: span.clone(),
                });
                tokens.push(last_tok);
                last_indent = None;
            }
        }
        tokens.push(SpannedTok {
            span: std::usize::MAX..std::usize::MAX, //codespan will take care of this, and even if the range goes out of the string, it will just display the range we're targeting
            //as the final element of the string
            elem: Token::EOF,
        });
        Ok(tokens)
    }
    pub fn string(&mut self, start: usize) -> Result<SpannedTok<'a>, ErrorInfo<'a>> {
        let mut length = 0;
        loop {
            match self.next() {
                Some((_, '"')) => break,
                Some(_) => length += 1,
                None => {
                    return Err(ErrorInfo {
                        span: start..start + length,
                        error: Error::UnclosedStringLiteral,
                        expected: Some(Expected::Named("an enclosing quote")),
                        found: Some(Found::Named("an EOF")),
                    })
                }
            }
        }
        self.next();
        let span = start - 1..start + length;
        return Ok(Spanned {
            elem: Token::Str(&self.source[span.clone()]),
            span,
        });
    }

    many!(num, |c| c.is_ascii_digit(), Token::Num);
    many!(ident, |c| c.is_alphanumeric(), Token::Ident);

    pub fn next_if(&mut self, predicate: impl Fn(char) -> bool) -> Option<(usize, char)> {
        if predicate(self.peek()?.1) {
            return self.next();
        }
        None
    }
    fn peek(&mut self) -> Option<&(usize, char)> {
        self.chars.peek()
    }
}
impl Iterator for Lexer<'_> {
    type Item = (usize, char);
    fn next(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }
}
