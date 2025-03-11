use winnow::PResult;

use winnow::{
    combinator::{alt, opt, separated},
    prelude::*,
    token::any,
};

use crate::node::{Expr, Node};
use crate::token::{Token, TokenType};

// #[derive(Debug, Clone)]
// pub struct TokenError {
//     // pub position: usize,
//     pub expected: String,
//     pub found: Option<Token>,
// }

// Implement ParseError for your custom error type
// impl ParserError<&[Token]> for TokenError {
//     fn from_error_kind(input: &mut &[Token], kind: ErrorKind) -> Self {
//         TokenError {
//             // position: input.start,
//             expected: format!("{:?}", kind),
//             found: input.first().cloned(),
//         }
//     }
//
//     fn append(self, _input: &&[Token], _checkpoint: &Checkpoint, _kind: ErrorKind) -> Self {
//         self
//     }
// }

// Implement FromExternalError for context support
// impl<'a> winnow::error::FromExternalError<&[Token], &'a str> for TokenError {
//     fn from_external_error(input: &&[Token], _kind: ErrorKind, e: &'a str) -> Self {
//         TokenError {
//             // position: input.start,
//             expected: e.to_string(),
//             found: input.first().cloned(),
//         }
//     }
// }

// Type alias for your results
// pub type PResult<O> = PResult<O, TokenError>;

fn expect_token<'a>(
    token_type: TokenType,
    value: Option<&'a str>,
) -> impl FnMut(&mut &[Token]) -> PResult<Token> + 'a {
    move |i: &mut &[Token]| {
        any.verify(|t: &Token| {
            t.token_type == token_type && (value.is_none() || t.value == value.unwrap())
        })
        .parse_next(i)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Element {
    pub tag: Tag,
    pub children: Option<Vec<Node>>,
}

impl Element {
    fn new(tag: Tag, children: Option<Vec<Node>>) -> Self {
        Self { tag, children }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Tag {
    pub name: String,
    pub attributes: Option<Vec<Attribute>>,
    pub self_closing: bool,
}

impl Tag {
    fn new(name: String, attributes: Option<Vec<Attribute>>, self_closing: bool) -> Self {
        Self {
            name,
            attributes,
            self_closing,
        }
    }

    fn is_self_closing(&self) -> bool {
        self.self_closing
    }

    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_attributes(&self) -> Option<&Vec<Attribute>> {
        self.attributes.as_ref()
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Attribute {
    pub key: String,
    pub value: Option<String>,
}

impl Attribute {
    fn new(key: String, value: Option<String>) -> Self {
        Self { key, value }
    }
}

// Helpers
fn whitespace(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::WhiteSpace, None)(i)
}

fn brace(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::Brace, None)(i)
}

fn operator(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::Operator, None)(i)
}

fn equals(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::Operator, Some("="))(i)
}

fn tag_open(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::Operator, Some("<"))(i)
}

fn tag_close(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::Operator, Some(">"))(i)
}

fn slash(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::Operator, Some("/"))(i)
}

fn word(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::Word, None)(i)
}

fn keyword(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::Keyword, None)(i)
}

fn string(i: &mut &[Token]) -> PResult<Token> {
    expect_token(TokenType::String, None)(i)
}

// Builders
fn attribute(i: &mut &[Token]) -> PResult<Attribute> {
    let key = alt((keyword, word)).parse_next(i)?.value;
    let mut value = None;
    if let Some(_) = opt(equals).parse_next(i)? {
        let string_token = string(i)?;
        value = Some(string_token.value);
    };

    Ok(Attribute { key, value })
}

fn attributes(i: &mut &[Token]) -> PResult<Vec<Attribute>> {
    separated(1.., attribute, whitespace).parse_next(i)
    // repeat(1.., preceded(whitespace, attribute)).parse_next(i)
}

fn opening_tag(i: &mut &[Token]) -> PResult<Tag> {
    println!("{:?}", i);
    tag_open(i)?;
    let name = word(i)?.value;
    // opt(whitespace).parse_next(i)?;
    let mut attrs = None;
    if let Some(_) = opt(whitespace).parse_next(i)? {
        attrs = opt(attributes).parse_next(i)?;
        opt(whitespace).parse_next(i)?;
    }
    println!("{:?}", attrs);
    let self_closing = opt(slash).parse_next(i)?.is_some();
    tag_close(i)?;
    // if !self_closing {
    //     let _ = tag_open(i)?;
    //     let _ = word(i)?;
    //     let _ = slash(i)?;
    //     let _ = tag_close(i)?;
    // }
    Ok(Tag {
        name,
        attributes: attrs,
        self_closing,
    })
}

fn closing_tag(i: &mut &[Token]) -> PResult<Tag> {
    let _open = tag_open(i)?;
    slash(i)?;
    let name = word(i)?.value;
    tag_close(i)?;
    Ok(Tag {
        name,
        attributes: None,
        self_closing: true,
    })
}

fn element(i: &mut &[Token]) -> PResult<Expr> {
    let opening_tag = opening_tag(i)?;
    let children = if !opening_tag.is_self_closing() {
        // TODO: Parse children
        // let children = Vec::new();
        // let children = repeat(0.., Node, whitespace).parse_next(i)?;
        // Some(children)

        None
    } else {
        None
    };
    if !opening_tag.is_self_closing() {
        closing_tag(i)?;
        // TODO: Check if opening and closing tags match
        // if opening_tag.get_name() != closing_tag.get_name() {
        //     return Err(winnow::error::StrError::new(
        //         "opening and closing tags do not match",
        //         ErrMode::Fatal,
        //         StrContext::Label("element"),
        //         StrContext::Value(StrContextValue::Token(closing_tag.get_name().to_string())),
        //     ));
        // }
    }
    Ok(Expr::HtmlElement(Element {
        tag: opening_tag,
        children,
    }))
}

#[cfg(test)]
mod tests {
    use crate::token::parse_all;

    use super::*;

    #[test]
    fn test_equals() {
        let input = vec![Token::new(0, 1, TokenType::Operator, "=".to_string())];
        let expected = Token::new(0, 1, TokenType::Operator, "=".to_string());
        let actual = equals(&mut input.as_slice()).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_word() {
        let input = vec![Token::new(0, 4, TokenType::Word, "class".to_string())];
        let expected = Token::new(0, 4, TokenType::Word, "class".to_string());
        let actual = word(&mut input.as_slice()).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_string() {
        let input = vec![Token::new(
            0,
            8,
            TokenType::String,
            r#""p-1 m-2""#.to_string(),
        )];
        let expected = Token::new(0, 8, TokenType::String, r#""p-1 m-2""#.to_string());
        let actual = string(&mut input.as_slice()).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_attribute() {
        let tests = vec![
            (
                r#"checked="true""#,
                Attribute::new("checked".to_string(), Some(r#""true""#.to_string())),
            ),
            (
                r#"class="p-1""#,
                Attribute::new("class".to_string(), Some(r#""p-1""#.to_string())),
            ),
            (
                r#"class="p-1 m-2""#,
                Attribute::new("class".to_string(), Some(r#""p-1 m-2""#.to_string())),
            ),
            ("checked", Attribute::new("checked".to_string(), None)),
            ("selected", Attribute::new("selected".to_string(), None)),
        ];
        for (input, expected) in tests {
            let mut input = input;
            let tokens = parse_all(&mut input).unwrap();
            let actual = attribute(&mut tokens.as_slice()).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_attributes() {
        let tests = vec![
            (
                r#"jake="true" checked"#,
                vec![
                    Attribute::new("jake".to_string(), Some(r#""true""#.to_string())),
                    Attribute::new("checked".to_string(), None),
                ],
            ),
            (
                r#"jake="true" checked"#,
                vec![
                    Attribute::new("jake".to_string(), Some(r#""true""#.to_string())),
                    Attribute::new("checked".to_string(), None),
                ],
            ),
            (
                r#"checked="true" class="p-1 m-2""#,
                vec![
                    Attribute::new("checked".to_string(), Some(r#""true""#.to_string())),
                    Attribute::new("class".to_string(), Some(r#""p-1 m-2""#.to_string())),
                ],
            ),
            (
                r#"class="p-1""#,
                vec![Attribute::new(
                    "class".to_string(),
                    Some(r#""p-1""#.to_string()),
                )],
            ),
        ];
        for (input, expected) in tests {
            let mut input = input;
            let tokens = parse_all(&mut input).unwrap();
            let actual = attributes(&mut tokens.as_slice()).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_open_tag() {
        let tests = vec![
            (r#"<div>"#, Tag::new("div".to_string(), None, false)),
            (
                r#"<div class="bg-red">"#,
                Tag::new(
                    "div".to_string(),
                    Some(vec![Attribute::new(
                        "class".to_string(),
                        Some(r#""bg-red""#.to_string()),
                    )]),
                    false,
                ),
            ),
            (
                r#"<div class="p-1 m-2">"#,
                Tag::new(
                    "div".to_string(),
                    Some(vec![Attribute::new(
                        "class".to_string(),
                        Some(r#""p-1 m-2""#.to_string()),
                    )]),
                    false,
                ),
            ),
            (
                r#"<input type="checkbox" checked/>"#,
                Tag::new(
                    "input".to_string(),
                    Some(vec![
                        Attribute::new("type".to_string(), Some(r#""checkbox""#.to_string())),
                        Attribute::new("checked".to_string(), None),
                    ]),
                    true,
                ),
            ),
            (r#"<input />"#, Tag::new("input".to_string(), None, true)),
        ];
        for (input, expected) in tests {
            let mut input = input;
            let tokens = parse_all(&mut input).unwrap();
            let actual = opening_tag(&mut tokens.as_slice()).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_element() {
        let tests = vec![
            (
                r#"<div></div>"#,
                Expr::HtmlElement(Element::new(Tag::new("div".to_string(), None, false), None)),
            ),
            (
                r#"<div class="p-1 m-2"></div>"#,
                Expr::HtmlElement(Element::new(
                    Tag::new(
                        "div".to_string(),
                        Some(vec![Attribute::new(
                            "class".to_string(),
                            Some(r#""p-1 m-2""#.to_string()),
                        )]),
                        false,
                    ),
                    None,
                )),
            ),
            (
                r#"<input />"#,
                Expr::HtmlElement(Element::new(
                    Tag::new("input".to_string(), None, true),
                    None,
                )),
            ),
            (
                r#"<input type="checkbox" checked/>"#,
                Expr::HtmlElement(Element::new(
                    Tag::new(
                        "input".to_string(),
                        Some(vec![
                            Attribute::new("type".to_string(), Some(r#""checkbox""#.to_string())),
                            Attribute::new("checked".to_string(), None),
                        ]),
                        true,
                    ),
                    None,
                )),
            ),
        ];
        for (input, expected) in tests {
            let mut input = input;
            let tokens = parse_all(&mut input).unwrap();
            let actual = element(&mut tokens.as_slice()).unwrap();
            assert_eq!(expected, actual);
        }
    }
}
