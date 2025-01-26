use crate::html::Tag;

pub(crate) struct Node {
    pub start: usize,
    pub end: usize,
    pub value: Expr,
}

pub(crate) enum Expr {
    Declaration,
    HtmlTag(Tag),
    Import,
    RustCode(String),
    Text(String),
}
