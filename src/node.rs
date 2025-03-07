use crate::html::Element;

#[derive(Debug, PartialEq)]
pub(crate) struct Node {
    pub start: usize,
    pub end: usize,
    pub value: Expr,
}

impl Node {
    pub fn new(start: usize, end: usize, value: Expr) -> Self {
        Self { start, end, value }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
    Declaration,
    HtmlElement(Element),
    Import,
    RustCode(String),
    Text(String),
}
