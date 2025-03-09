use winnow::error::{ErrorKind, ParseError, ParserError};
use winnow::PResult;

use winnow::{
    combinator::{
        alt, delimited, opt, peek, preceded, repeat, separated, separated_pair, terminated,
    },
    dispatch,
    error::{ErrMode, StrContext, StrContextValue},
    prelude::*,
    stream::Stream,
    token::{any, one_of, take_till},
};

use crate::node::{Expr, Node};
use crate::token::{parse_all, Checkpoint, Token, TokenStream, TokenType};

#[derive(Debug, Clone)]
pub struct TokenError {
    pub position: usize,
    pub expected: String,
    pub found: Option<Token>,
}

// Implement ParseError for your custom error type
impl ParserError<TokenStream> for TokenError {
    fn from_error_kind(input: &TokenStream, kind: ErrorKind) -> Self {
        TokenError {
            position: input.start,
            expected: format!("{:?}", kind),
            found: input.tokens.first().cloned(),
        }
    }

    fn append(self, _input: &TokenStream, _checkpoint: &Checkpoint, _kind: ErrorKind) -> Self {
        self
    }
}

// Implement FromExternalError for context support
impl<'a> winnow::error::FromExternalError<TokenStream, &'a str> for TokenError {
    fn from_external_error(input: &TokenStream, _kind: ErrorKind, e: &'a str) -> Self {
        TokenError {
            position: input.start,
            expected: e.to_string(),
            found: input.tokens.first().cloned(),
        }
    }
}

// Type alias for your results
pub type TokenResult<O> = PResult<O, TokenError>;

fn expect_token<'a>(
    token_type: TokenType,
    value: Option<&'a str>,
    error_msg: &'static str,
) -> impl FnMut(&mut TokenStream) -> TokenResult<Token> + 'a {
    move |i: &mut TokenStream| {
        let current_pos = i.start;
        let current_token = i.tokens.first().cloned();

        match any
            .verify(|t: &Token| {
                t.token_type == token_type && (value.is_none() || t.value == value.unwrap())
            })
            .parse_next(i)
        {
            Ok(token) => Ok(token),
            Err(ErrMode::Backtrack(_)) => Err(ErrMode::Backtrack(TokenError {
                position: current_pos,
                expected: error_msg.to_string(),
                found: current_token,
            })),
            Err(other) => Err(other),
        }
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
fn whitespace(i: &mut TokenStream) -> TokenResult<Token> {
    // any.verify(|t: &Token| t.token_type == TokenType::WhiteSpace)
    //     .context(winnow::error::StrContext::Label("expected whitespace"))
    //     .parse_next(i)
    expect_token(TokenType::WhiteSpace, None, "expected whitespace")(i)
}

fn brace(i: &mut TokenStream) -> TokenResult<Token> {
    // any.verify(|t: &Token| t.token_type == TokenType::Brace)
    //     .context(winnow::error::StrContext::Label("expected brace"))
    //     .parse_next(i)
    expect_token(TokenType::Brace, None, "expected brace")(i)
}

fn operator(i: &mut TokenStream) -> TokenResult<Token> {
    // any.verify(|t: &Token| t.token_type == TokenType::Operator)
    //     .context(winnow::error::StrContext::Label("expected operator"))
    //     .parse_next(i)
    expect_token(TokenType::Operator, None, "expected operator")(i)
}

fn equals(i: &mut TokenStream) -> TokenResult<Token> {
    // any.verify(|t: &Token| t.token_type == TokenType::Operator && t.value == "=")
    //     .context(winnow::error::StrContext::Label("expected equals"))
    //     .parse_next(i)
    expect_token(TokenType::Operator, Some("="), "expected equals")(i)
}

fn tag_open(i: &mut TokenStream) -> TokenResult<Token> {
    // let current_position = i.start;
    // let current_token = i.tokens.first().cloned();
    // any.verify(|t: &Token| t.token_type == TokenType::Operator && t.value == "<")
    //     .context(winnow::error::StrContext::Label("expected tag open"))
    //     .parse_next(i)
    expect_token(TokenType::Operator, Some("<"), "expected tag open")(i)
}

fn tag_close(i: &mut TokenStream) -> TokenResult<Token> {
    // let current_position = i.start;
    // let current_token = i.tokens.first().cloned();
    // match any
    //     .verify(|t: &Token| t.token_type == TokenType::Operator && t.value == ">")
    //     .parse_next(i)
    // {
    //     Ok(token) => Ok(token),
    //     Err(ErrMode::Backtrack(_)) => Err(ErrMode::Backtrack(TokenError {
    //         position: current_position,
    //         expected: "expected tag close '>'".to_string(),
    //         found: current_token,
    //     })),
    //     Err(other) => Err(other),
    // }
    expect_token(TokenType::Operator, Some(">"), "expected tag close '>'")(i)
}

fn slash(i: &mut TokenStream) -> TokenResult<Token> {
    // any.verify(|t: &Token| t.token_type == TokenType::Operator && t.value == "/")
    //     .context(winnow::error::StrContext::Label("expected slash"))
    //     .parse_next(i)
    expect_token(TokenType::Operator, Some("/"), "expected slash")(i)
}

fn word(i: &mut TokenStream) -> TokenResult<Token> {
    // any.verify(|t: &Token| t.token_type == TokenType::Word)
    //     .context(winnow::error::StrContext::Label("expected word"))
    //     .parse_next(i)
    expect_token(TokenType::Word, None, "expected word")(i)
}

fn string(i: &mut TokenStream) -> TokenResult<Token> {
    // any.verify(|t: &Token| t.token_type == TokenType::String)
    //     .context(winnow::error::StrContext::Label("expected string"))
    //     .parse_next(i)
    expect_token(TokenType::String, None, "expected string")(i)
}

// Builders
fn attribute(i: &mut TokenStream) -> TokenResult<Attribute> {
    let key = word(i)?.value;
    let mut value = None;
    if let Some(_) = opt(equals).parse_next(i)? {
        let string_token = string(i)?;
        value = Some(string_token.value);
    };

    Ok(Attribute { key, value })
}

fn attributes(i: &mut TokenStream) -> TokenResult<Vec<Attribute>> {
    separated(1.., attribute, whitespace).parse_next(i)
    // repeat(1.., preceded(whitespace, attribute)).parse_next(i)
}

fn opening_tag(i: &mut TokenStream) -> TokenResult<Tag> {
    tag_open(i)?;
    println!("after tag_open:\n\tStart: {}", i.start);
    let name = word(i)?.value;
    println!("after word:\n\tStart: {}", i.start);
    // opt(whitespace).parse_next(i)?;
    let mut attrs = None;
    if let Some(_) = opt(whitespace).parse_next(i)? {
        println!("after whitespace:\n\tStart: {}", i.start);
        attrs = opt(attributes).parse_next(i)?;
        println!("attrs: {:?}", attrs);
        println!("after attributes:\n\tStart: {}", i.start);
        opt(whitespace).parse_next(i)?;
        println!("after whitespace:\n\tStart: {}", i.start);
    }
    let self_closing = opt(slash).parse_next(i)?.is_some();
    println!("after slash:\n\tStart: {}", i.start);
    tag_close(i)?;
    println!("after tag_close:\n\tStart: {}", i.start);
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

fn closing_tag(i: &mut TokenStream) -> TokenResult<Tag> {
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

fn element(i: &mut TokenStream) -> TokenResult<Node> {
    let start = i.start;
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
    Ok(Node {
        start,
        end: i.end,
        value: Expr::HtmlElement(Element {
            tag: opening_tag,
            children,
        }),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_equals() {
        let mut input = TokenStream {
            tokens: vec![Token::new(0, 1, TokenType::Operator, "=".to_string())],
            start: 0,
            end: 1,
        };
        let expected = Token::new(0, 1, TokenType::Operator, "=".to_string());
        let actual = equals(&mut input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_word() {
        let mut input = TokenStream {
            tokens: vec![Token::new(0, 4, TokenType::Word, "class".to_string())],
            start: 0,
            end: 4,
        };
        let expected = Token::new(0, 4, TokenType::Word, "class".to_string());
        let actual = word(&mut input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_string() {
        let mut input = TokenStream {
            tokens: vec![Token::new(
                0,
                8,
                TokenType::String,
                r#""p-1 m-2""#.to_string(),
            )],
            start: 0,
            end: 4,
        };
        let expected = Token::new(0, 8, TokenType::String, r#""p-1 m-2""#.to_string());
        let actual = string(&mut input).unwrap();
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
            let mut stream = TokenStream::new(tokens);
            let actual = attribute(&mut stream).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_attributes() {
        let tests = vec![
            // (
            //     r#"jake="true" checked"#,
            //     vec![
            //         Attribute::new("jake".to_string(), Some(r#""true""#.to_string())),
            //         Attribute::new("checked".to_string(), None),
            //     ],
            // ),
            (
                r#"jake="true" checked"#,
                vec![
                    Attribute::new("jake".to_string(), Some(r#""true""#.to_string())),
                    Attribute::new("checked".to_string(), None),
                ],
            ),
            // (
            //     r#"jake="true" class="p-1 m-2""#,
            //     vec![
            //         Attribute::new("checked".to_string(), Some(r#""true""#.to_string())),
            //         Attribute::new("class".to_string(), Some(r#""p-1 m-2""#.to_string())),
            //     ],
            // ),
            (
                r#"class="p-1""#,
                vec![Attribute::new(
                    "class".to_string(),
                    Some(r#""p-1""#.to_string()),
                )],
            ),
            // (
            //     r#"class="p-1 m-2""#,
            //     Attribute::new("class".to_string(), Some(r#""p-1 m-2""#.to_string())),
            // ),
            // ("checked", Attribute::new("checked".to_string(), None)),
            // ("selected", Attribute::new("selected".to_string(), None)),
        ];
        for (input, expected) in tests {
            let mut input = input;
            let tokens = parse_all(&mut input).unwrap();
            let mut stream = TokenStream::new(tokens);
            let actual = attributes(&mut stream).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_element() {
        let tests = vec![
            (
                r#"<div></div>"#,
                Node::new(
                    0,
                    11,
                    Expr::HtmlElement(Element::new(Tag::new("div".to_string(), None, false), None)),
                ),
            ),
            (
                r#"<div class="p-1 m-2"></div>"#,
                Node::new(
                    0,
                    27,
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
            ),
            (
                r#"<input />"#,
                Node::new(
                    0,
                    9,
                    Expr::HtmlElement(Element::new(
                        Tag::new("input".to_string(), None, true),
                        None,
                    )),
                ),
            ),
            (
                r#"<input type="checkbox" />"#,
                Node::new(
                    0,
                    33,
                    Expr::HtmlElement(Element::new(
                        Tag::new(
                            "input".to_string(),
                            Some(vec![
                                Attribute::new(
                                    "type".to_string(),
                                    Some(r#""checkbox""#.to_string()),
                                ),
                                // Attribute::new("checked".to_string(), None),
                            ]),
                            true,
                        ),
                        None,
                    )),
                ),
            ),
        ];
        for (input, expected) in tests {
            let mut input = input;
            let tokens = parse_all(&mut input).unwrap();
            let mut stream = TokenStream::new(tokens);
            let actual = element(&mut stream).unwrap();
            assert_eq!(expected, actual);
        }
    }
}
