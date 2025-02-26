use std::iter::Enumerate;
use std::ops::Range;
use std::vec::IntoIter;
use winnow::{
    ascii::{alpha1, digit1, line_ending, multispace1},
    combinator::{alt, dispatch, eof, peek, repeat},
    error::{ContextError, ErrMode, ErrorKind, ParseError, ParserError},
    stream::{Offset, Stream, StreamIsPartial},
    token::{any, one_of, take_till, take_until},
    LocatingSlice, PResult, Parser,
};

const RUST_KEYWORDS: [&str; 48] = [
    "fn", "struct", "enum", "trait", "impl", "type", "mod", "bool", "char", "i8", "i16", "i32",
    "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64", "str", "isize", "usize", "let",
    "mut", "const", "static", "ref", "move", "if", "else", "match", "loop", "while", "for",
    "break", "continue", "return", "pub", "super", "self", "crate", "panic!", "Result", "Option",
    "Some", "None",
];

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct Token {
    pub start: usize,
    pub end: usize,
    pub token_type: TokenType,
    pub value: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum TokenType {
    Number,
    Word,
    Operator,
    String,
    Keyword,
    WhiteSpace,
    Brace,
    Comma,
    Colon,
    SemiColon,
    Period,
    DoublePeriod,
    LineComment,
    BlockComment,
    Function,
    HtmlBlock,
}

impl Token {
    pub(crate) fn new(start: usize, end: usize, token_type: TokenType, value: String) -> Self {
        Token {
            start,
            end,
            token_type,
            value,
        }
    }

    fn from_range(range: Range<usize>, token_type: TokenType, value: String) -> Self {
        Token {
            start: range.start,
            end: range.end,
            token_type,
            value,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct TokenStream {
    pub tokens: Vec<Token>,
    pub start: usize,
    pub end: usize,
}

impl StreamIsPartial for TokenStream {
    type PartialState = ();

    fn restore_partial(&mut self, _: Self::PartialState) {}

    fn complete(&mut self) -> Self::PartialState {}

    fn is_partial_supported() -> bool {
        false
    }
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        let first = tokens.first();
        let last = tokens.last();

        TokenStream {
            tokens: tokens.clone(),
            start: first.unwrap().start,
            end: last.unwrap().end,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Token> {
        self.tokens.iter()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Checkpoint(usize, usize);

impl Offset for Checkpoint {
    fn offset_from(&self, start: &Self) -> usize {
        start.0 - self.0
    }
}

impl Offset for TokenStream {
    fn offset_from(&self, start: &Self) -> usize {
        start.start - self.start
    }
}

impl Offset<Checkpoint> for TokenStream {
    fn offset_from(&self, start: &Checkpoint) -> usize {
        start.0 - self.start
    }
}

impl Stream for TokenStream {
    type Token = Token;
    type Slice = Self;
    type IterOffsets = Enumerate<std::vec::IntoIter<Token>>;
    type Checkpoint = Checkpoint;

    fn iter_offsets(&self) -> Self::IterOffsets {
        return self.tokens.to_vec().into_iter().enumerate();
    }

    fn eof_offset(&self) -> usize {
        self.end
    }

    fn raw(&self) -> &dyn std::fmt::Debug {
        self
    }

    fn offset_for<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Token) -> bool,
    {
        self.iter().position(|b| predicate(b.clone()))
    }

    fn reset(&mut self, checkpoint: &Self::Checkpoint) {
        self.start = checkpoint.0;
        self.end = checkpoint.1;
    }

    fn offset_at(&self, tokens: usize) -> Result<usize, winnow::error::Needed> {
        Ok(tokens - self.start)
    }

    fn next_token(&mut self) -> Option<Self::Token> {
        let token = self.tokens.iter().next();
        self.start += 1;
        match token {
            Some(token) => Some(token.clone()),
            None => None,
        }
    }

    fn checkpoint(&self) -> Self::Checkpoint {
        Checkpoint(self.start, self.end)
    }

    fn next_slice(&mut self, offset: usize) -> Self::Slice {
        let next = TokenStream {
            tokens: self.tokens.clone(),
            start: self.start,
            end: self.start + offset,
        };
        self.start += offset;
        next
    }
}

impl IntoIterator for TokenStream {
    type Item = Token;
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

fn parse_all(i: &str) -> Result<Vec<Token>, ParseError<LocatingSlice<&str>, ContextError>> {
    match repeat(0.., make_token).parse(LocatingSlice::new(i)) {
        Ok(tokens) => Ok(tokens),
        Err(e) => Err(e),
    }
}

fn make_token(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    dispatch!(
        peek(any);
        '/' => alt((line_comment, operator)),
        ':' => colon,
        ';' => semicolon,
        ',' => comma,
        '.' => alt((double_period, period)),
        '"' => string,
        '(' | '[' | '{' => brace_open,
        ')' | ']' | '}' => brace_close,
        '+' | '-' | '*' | '>' | '<' | '=' => operator,
        ' ' | '\t' | '\n' | '\r' => white_space,
        '0'..='9' => number,
        'a'..='z' | 'A'..='Z' => alt((keyword, word)),
        // This is definitely wrong
        _ => word,
    )
    .parse_next(i)
}

fn period(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = '.'.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::Period,
        value.to_string(),
    ))
}

fn double_period(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = "..".with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::DoublePeriod,
        value.to_string(),
    ))
}

fn comma(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = ','.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::Comma,
        value.to_string(),
    ))
}

fn colon(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = ':'.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::Colon,
        value.to_string(),
    ))
}

fn semicolon(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = ';'.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::SemiColon,
        value.to_string(),
    ))
}

fn white_space(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = multispace1.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::WhiteSpace,
        value.to_string(),
    ))
}

fn inner_word(i: &mut LocatingSlice<&str>) -> PResult<()> {
    one_of(('a'..='z', 'A'..='Z')).parse_next(i)?;
    repeat(0.., one_of(('a'..='z', 'A'..='Z', '0'..='9', '_', '-'))).parse_next(i)?;
    Ok(())
}

pub(crate) fn word(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = inner_word.take().with_span().parse_next(i)?;
    Ok(Token::from_range(range, TokenType::Word, value.to_string()))
}

fn keyword(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let mut token = word(i)?;
    if RUST_KEYWORDS.contains(&token.value.as_str()) {
        token.token_type = TokenType::Keyword;
        return Ok(token);
    }
    Err(ErrMode::Backtrack(ParserError::from_error_kind(
        i,
        ErrorKind::Verify,
    )))
}

fn number(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = digit1.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::Number,
        value.to_string(),
    ))
}

pub(crate) fn operator(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = alt(("+", "-", "*", "/", ">=", "<=", ">", "<", "="))
        .with_span()
        .parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::Operator,
        value.to_string(),
    ))
}

fn line_comment(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let inner = ("//", take_till(0.., ['\n', '\r'])).take();
    let (value, range) = inner.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::LineComment,
        value.to_string(),
    ))
}

fn brace_open(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = alt(("{", "(", "[")).with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::Brace,
        value.to_string(),
    ))
}

fn brace_close(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = one_of(('}', ')', ']')).with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::Brace,
        value.to_string(),
    ))
}

pub(crate) fn string(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let inner = ("\"", take_until(0.., "\""), "\"").take();
    let (value, range) = inner.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::String,
        value.to_string(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comma() {
        let input = ",";
        let expected = Token::new(0, 1, TokenType::Comma, ",".to_string());
        let actual = comma(&mut LocatingSlice::new(input)).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_period() {
        let input = ".";
        let expected = Token::new(0, 1, TokenType::Period, ".".to_string());
        let actual = period(&mut LocatingSlice::new(input)).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_double_period() {
        let input = "..";
        let expected = Token::new(0, 2, TokenType::DoublePeriod, "..".to_string());
        let actual = double_period(&mut LocatingSlice::new(input)).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_colon() {
        let input = ":";
        let expected = Token::new(0, 1, TokenType::Colon, ":".to_string());
        let actual = colon(&mut LocatingSlice::new(input)).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_whitespace() {
        let tests = vec![
            (
                " ",
                Token::new(0, 1, TokenType::WhiteSpace, " ".to_string()),
            ),
            (
                "  ",
                Token::new(0, 2, TokenType::WhiteSpace, "  ".to_string()),
            ),
            (
                "     ",
                Token::new(0, 5, TokenType::WhiteSpace, "     ".to_string()),
            ),
            (
                "\t",
                Token::new(0, 1, TokenType::WhiteSpace, "\t".to_string()),
            ),
            (
                "\n",
                Token::new(0, 1, TokenType::WhiteSpace, "\n".to_string()),
            ),
            (
                "\r",
                Token::new(0, 1, TokenType::WhiteSpace, "\r".to_string()),
            ),
            (
                "\r\n",
                Token::new(0, 2, TokenType::WhiteSpace, "\r\n".to_string()),
            ),
        ];
        for (input, expected) in tests {
            let actual = white_space(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_number() {
        let tests = vec![
            ("0", Token::new(0, 1, TokenType::Number, "0".to_string())),
            ("1", Token::new(0, 1, TokenType::Number, "1".to_string())),
            (
                "123",
                Token::new(0, 3, TokenType::Number, "123".to_string()),
            ),
            (
                "1234567890",
                Token::new(0, 10, TokenType::Number, "1234567890".to_string()),
            ),
        ];
        for (input, expected) in tests {
            let actual = number(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_word() {
        let tests = vec![
            ("a", Token::new(0, 1, TokenType::Word, "a".to_string())),
            ("A", Token::new(0, 1, TokenType::Word, "A".to_string())),
            ("a1", Token::new(0, 2, TokenType::Word, "a1".to_string())),
            ("A1", Token::new(0, 2, TokenType::Word, "A1".to_string())),
            ("a1_", Token::new(0, 3, TokenType::Word, "a1_".to_string())),
            ("A1_", Token::new(0, 3, TokenType::Word, "A1_".to_string())),
            (
                "a1-b",
                Token::new(0, 4, TokenType::Word, "a1-b".to_string()),
            ),
            (
                "A1-B",
                Token::new(0, 4, TokenType::Word, "A1-B".to_string()),
            ),
            (
                "A1_B",
                Token::new(0, 4, TokenType::Word, "A1_B".to_string()),
            ),
        ];
        for (input, expected) in tests {
            let actual = word(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_operator() {
        let tests = vec![
            ("+", Token::new(0, 1, TokenType::Operator, "+".to_string())),
            ("-", Token::new(0, 1, TokenType::Operator, "-".to_string())),
            ("*", Token::new(0, 1, TokenType::Operator, "*".to_string())),
            ("/", Token::new(0, 1, TokenType::Operator, "/".to_string())),
            (">", Token::new(0, 1, TokenType::Operator, ">".to_string())),
            ("<", Token::new(0, 1, TokenType::Operator, "<".to_string())),
            (
                ">=",
                Token::new(0, 2, TokenType::Operator, ">=".to_string()),
            ),
            (
                "<=",
                Token::new(0, 2, TokenType::Operator, "<=".to_string()),
            ),
            ("=", Token::new(0, 1, TokenType::Operator, "=".to_string())),
        ];
        for (input, expected) in tests {
            let actual = operator(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_keyword() {
        let tests = vec![
            (
                "let",
                Token::new(0, 3, TokenType::Keyword, "let".to_string()),
            ),
            (
                "struct",
                Token::new(0, 6, TokenType::Keyword, "struct".to_string()),
            ),
        ];
        for (input, expected) in tests {
            let actual = keyword(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_keyword_fail() {
        let tests = vec![(
            "word",
            Err(ErrMode::Backtrack(ParserError::from_error_kind(
                &"word",
                ErrorKind::Verify,
            ))),
        )];
        for (input, expected) in tests {
            let actual = keyword(&mut LocatingSlice::new(input));
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_string() {
        let tests = vec![(
            r#""let""#,
            Token::new(0, 5, TokenType::String, r#""let""#.to_string()),
        )];
        for (input, expected) in tests {
            let actual = string(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_open_brace() {
        let tests = vec![
            ("{", Token::new(0, 1, TokenType::Brace, "{".to_string())),
            ("(", Token::new(0, 1, TokenType::Brace, "(".to_string())),
            ("[", Token::new(0, 1, TokenType::Brace, "[".to_string())),
        ];
        for (input, expected) in tests {
            let actual = brace_open(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_close_brace() {
        let tests = vec![
            ("}", Token::new(0, 1, TokenType::Brace, "}".to_string())),
            (")", Token::new(0, 1, TokenType::Brace, ")".to_string())),
            ("]", Token::new(0, 1, TokenType::Brace, "]".to_string())),
        ];
        for (input, expected) in tests {
            let actual = brace_close(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_semicolon() {
        let tests = vec![(";", Token::new(0, 1, TokenType::SemiColon, ";".to_string()))];
        for (input, expected) in tests {
            let actual = semicolon(&mut LocatingSlice::new(input)).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_make_token() {
        let tests = vec![
            (
                "+ ",
                vec![
                    Token::new(0, 1, TokenType::Operator, "+".to_string()),
                    Token::new(1, 2, TokenType::WhiteSpace, " ".to_string()),
                ],
            ),
            (
                "test word",
                vec![
                    Token::new(0, 4, TokenType::Word, "test".to_string()),
                    Token::new(4, 5, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(5, 9, TokenType::Word, "word".to_string()),
                ],
            ),
            (
                "{ name }",
                vec![
                    Token::new(0, 1, TokenType::Brace, "{".to_string()),
                    Token::new(1, 2, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(2, 6, TokenType::Word, "name".to_string()),
                    Token::new(6, 7, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(7, 8, TokenType::Brace, "}".to_string()),
                ],
            ),
            (
                r#"<div class="flex">{ name }</div>"#,
                vec![
                    Token::new(0, 1, TokenType::Operator, "<".to_string()),
                    Token::new(1, 4, TokenType::Word, "div".to_string()),
                    Token::new(4, 5, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(5, 10, TokenType::Word, "class".to_string()),
                    Token::new(10, 11, TokenType::Operator, "=".to_string()),
                    Token::new(11, 17, TokenType::String, r#""flex""#.to_string()),
                    Token::new(17, 18, TokenType::Operator, ">".to_string()),
                    Token::new(18, 19, TokenType::Brace, "{".to_string()),
                    Token::new(19, 20, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(20, 24, TokenType::Word, "name".to_string()),
                    Token::new(24, 25, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(25, 26, TokenType::Brace, "}".to_string()),
                    Token::new(26, 27, TokenType::Operator, "<".to_string()),
                    Token::new(27, 28, TokenType::Operator, "/".to_string()),
                    Token::new(28, 31, TokenType::Word, "div".to_string()),
                    Token::new(31, 32, TokenType::Operator, ">".to_string()),
                ],
            ),
            (
                r#"let name = "jake";"#,
                vec![
                    Token::new(0, 3, TokenType::Keyword, "let".to_string()),
                    Token::new(3, 4, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(4, 8, TokenType::Word, "name".to_string()),
                    Token::new(8, 9, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(9, 10, TokenType::Operator, "=".to_string()),
                    Token::new(10, 11, TokenType::WhiteSpace, " ".to_string()),
                    Token::new(11, 17, TokenType::String, r#""jake""#.to_string()),
                    Token::new(17, 18, TokenType::SemiColon, ";".to_string()),
                ],
            ),
        ];
        for (input, expected) in tests {
            let actual: Vec<Token> = parse_all(input).unwrap();
            assert_eq!(expected, actual);
        }
    }
}
