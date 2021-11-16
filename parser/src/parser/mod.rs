#![allow(dead_code)]

mod comment;
pub mod expression;
pub mod literals;
pub mod statement;
pub mod tokens;

use nom::{error::VerboseError, IResult};

use nom_locate::LocatedSpan;

use crate::parser::statement::{parse_block, Block};

pub type Span<'a> = LocatedSpan<&'a str>;

pub type Res<'a, O> = IResult<Span<'a>, O, VerboseError<Span<'a>>>;

/// Entry point to the parser, which parse the liva language
pub fn parse_source(input: Span) -> Res<Block> {
    parse_block(input)
}
