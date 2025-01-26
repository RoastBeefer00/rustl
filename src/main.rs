use std::fs::read_to_string;

use winnow::{
    ascii::{alpha1, multispace0},
    combinator::delimited,
    PResult, Parser,
};

mod html;
mod token;

fn main() {
    // let read = read_to_string("test.rustl").unwrap();
    // let (_, div) = parse_div(&read).unwrap();
    // pri
    // ntln!("{:?}", div);
}
