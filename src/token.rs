use winnow::{
    ascii::{alpha1, digit1, line_ending, multispace1},
    combinator::{alt, dispatch, eof, peek, repeat},
    token::{any, one_of, take_till, take_until},
    LocatingSlice, PResult, Parser,
};

use std::ops::Range;

#[derive(Debug, PartialEq, Eq)]
struct Token {
    start: usize,
    end: usize,
    token_type: TokenType,
    value: String,
}

#[derive(Debug, PartialEq, Eq)]
enum TokenType {
    Number,
    Word,
    Operator,
    String,
    Keyword,
    WhiteSpace,
    Brace,
    Comma,
    Colon,
    Period,
    DoublePeriod,
    LineComment,
    BlockComment,
    Function,
    HtmlBlock,
}

impl Token {
    fn new(start: usize, end: usize, token_type: TokenType, value: String) -> Self {
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

fn make_token(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    dispatch!(
        peek(any);
        '/' => line_comment,
        ':' => colon,
        ',' => comma,
        '.' => alt((double_period, period)),
        '(' | '[' | '{' => brace_open,
        ')' | ']' | '}' => brace_close,
        '+' | '-' | '*' | '>' | '<' | '=' => operator,
        ' ' | '\t' | '\n' | '\r' => white_space,
        '0'..='9' => number,
        'a'..='z' | 'A'..='Z' => word,
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

fn word(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = inner_word.take().with_span().parse_next(i)?;
    Ok(Token::from_range(range, TokenType::Word, value.to_string()))
}

fn number(i: &mut LocatingSlice<&str>) -> PResult<Token> {
    let (value, range) = digit1.with_span().parse_next(i)?;
    Ok(Token::from_range(
        range,
        TokenType::Number,
        value.to_string(),
    ))
}

fn operator(i: &mut LocatingSlice<&str>) -> PResult<Token> {
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
    let (value, range) = one_of(('{', '(', '[')).with_span().parse_next(i)?;
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
        ];
        for (input, expected) in tests {
            let actual: Vec<Token> = repeat(1.., make_token)
                .parse(LocatingSlice::new(input))
                .unwrap();
            assert_eq!(expected, actual);
        }
    }
}
