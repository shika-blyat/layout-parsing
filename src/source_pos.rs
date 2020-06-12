use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub elem: T,
    pub span: Span,
}

impl<T> From<Spanned<T>> for Spanned<Box<T>> {
    fn from(b: Spanned<T>) -> Spanned<Box<T>> {
        Spanned {
            elem: Box::new(b.elem),
            span: b.span,
        }
    }
}
