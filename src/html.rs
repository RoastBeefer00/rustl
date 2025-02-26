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

use crate::node::Node;
use crate::token::{parse_all, Token, TokenStream, TokenType};

pub(crate) struct Tag {
    pub name: String,
    pub attributes: Vec<Attribute>,
    pub children: Vec<Node>,
    pub tag_type: TagType,
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

pub(crate) enum TagType {
    Start,
    End,
    SelfClosing,
}

fn whitespace(i: &mut TokenStream) -> PResult<Token> {
    any.verify(|t: &Token| t.token_type == TokenType::WhiteSpace)
        .context(winnow::error::StrContext::Label("expected whitespace"))
        .parse_next(i)
}

fn attribute(i: &mut TokenStream) -> PResult<Attribute> {
    let key = word(i)?.value;
    let mut value = None;
    if let Some(_) = opt(equals).parse_next(i)? {
        value = Some(string(i)?.value);
    };
    // equals(i)?;
    // let value = string(i)?.value;

    Ok(Attribute { key, value })
}

fn attributes(i: &mut TokenStream) -> PResult<Vec<Attribute>> {
    separated(0.., attribute, whitespace).parse_next(i)
}

fn equals(i: &mut TokenStream) -> PResult<Token> {
    any.verify(|t: &Token| {
        println!("Eq: {:?}", t);
        t.token_type == TokenType::Operator && t.value == "="
    })
    .context(winnow::error::StrContext::Label("expected equals"))
    .parse_next(i)
}

fn word(i: &mut TokenStream) -> PResult<Token> {
    any.verify(|t: &Token| t.token_type == TokenType::Word)
        .context(winnow::error::StrContext::Label("expected word"))
        .parse_next(i)
}

fn string(i: &mut TokenStream) -> PResult<Token> {
    any.verify(|t: &Token| t.token_type == TokenType::String)
        .context(winnow::error::StrContext::Label("expected string"))
        .parse_next(i)
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
}
