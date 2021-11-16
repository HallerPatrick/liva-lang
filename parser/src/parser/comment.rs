use crate::parser::{
    tokens::{comment, newline},
    Res, Span,
};

use nom::{bytes::complete::is_not, error::context, sequence::delimited};

/// Parses a comment and discards it
pub(crate) fn parse_comment(input: Span) -> Res<&str> {
    context("Comment", delimited(comment, is_not("\n"), newline))(input)
        .map(|(next_input, _)| (next_input, ""))
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse_comment() {
        let string = "// This is a comment\nSome code";
        let (_, res) = parse_comment(Span::new(string)).unwrap();

        assert_eq!(res, "");
    }
}
