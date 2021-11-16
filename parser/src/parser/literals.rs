//! Collection of all possible literals

use nom::sequence::delimited;
use crate::Span;
use std::borrow::Borrow;
use std::collections::HashMap;

use crate::parser::{
    comment::parse_comment,
    expression::{parse_expression, Expression},
    tokens::KEYWORDS,
    Res,
};
use nom::number::complete::recognize_float;
use nom::{
    self,
    branch::alt,
    bytes::complete::{escaped, tag, tag_no_case, take_while},
    character::complete::char,
    character::complete::{alpha1, alphanumeric1 as alphanumeric, digit1, one_of, space1},
    combinator::{cut, map, opt, recognize},
    error::context,
    error_position,
    multi::{many0, separated_list0},
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Clone, Debug, Copy)]
pub struct Token<'a, T> {
    pub value: T,
    pub pos: Span<'a>,
}

impl<'a, T> Token<'a, T> {
    pub fn new(value: T, pos: Span) -> Token<T> {
        Token { value, pos }
    }
}

pub type Variable<'a> = Token<'a, Identifier<'a>>;

// Collection of all possible literals
#[derive(Clone, Debug, PartialEq)]
pub enum Literal<'a> {
    Str(Token<'a, String>),
    Boolean(Token<'a, bool>),
    Nil(Span<'a>),
    Float(Token<'a, f64>),
    Int(Token<'a, i32>), // Array(Vec<Literal<'a>>),
                         // Map(HashMap<String, Literal<'a>>),
                         // Variable(Token<'a, Identifier<'a>>)
}

/// Collections are seperated from literals, as they allow
/// for expressions
#[derive(Clone, Debug, PartialEq)]
pub enum Collection<'a> {
    Array(Vec<Expression<'a>>),
    Map(HashMap<String, Expression<'a>>),
}

impl<'a, T> PartialEq for Token<'a, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }

    fn ne(&self, other: &Self) -> bool {
        self.value != other.value
    }
}

/// Contains the name of a identifier, which has to start with letter, but can contain
/// numbers and underscores
pub type Identifier<'a> = &'a str;

pub(crate) fn parse_variable_raw(input: Span) -> Res<Span> {
    let res = context(
        "Variable",
        // Keywords should not be parsed
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric, tag("_")))),
        )),
    )(input);

    if let IResult::Ok((_, name)) = res {
        if KEYWORDS.contains(name.borrow()) {
            return Err(nom::Err::Error(error_position!(
                input,
                nom::error::ErrorKind::Tag
            )));
        }
    }
    res
}

pub(crate) fn parse_variable(input: Span) -> Res<Variable> {
    parse_variable_raw(input)
        .map(|(next_input, res)| (next_input, Token::new(*res.fragment(), res)))
}

pub(crate) fn sp(input: Span) -> Res<&str> {
    let chars = " \t\r\n";
    tuple((take_while(move |c| chars.contains(c)), opt(parse_comment)))(input)
        .map(|(next_input, _)| (next_input, ""))
}

fn alphanumeric_ws(input: Span) -> Res<Span> {
    alt((space1, alphanumeric))(input)
}

fn parse_string(input: Span) -> Res<Span> {
    escaped(alphanumeric_ws, '\\', one_of("\"n\\"))(input)
}

fn parse_str_raw(input: Span) -> Res<Span> {
    alt((parse_str_raw_single, parse_str_raw_double))(input)
}
fn parse_str_raw_single(input: Span) -> Res<Span> {
    context(
        "String",
        preceded(char('\"'), cut(terminated(parse_string, char('\"')))),
    )(input)
}

fn parse_str_raw_double(input: Span) -> Res<Span> {
    context(
        "String",
        preceded(char('\''), cut(terminated(parse_string, char('\'')))),
    )(input)
}

fn parse_str(input: Span) -> Res<Literal> {
    parse_str_raw(input).map(|(next_input, res)| {
        (
            next_input,
            Literal::Str(Token::new(String::from(*res.fragment()), res)),
        )
    })
}

fn parse_float(input: Span) -> Res<Literal> {
    context(
        "Float",
        recognize(delimited(digit1, char('.'), digit1))
    )(input).map(|(next_input, res)| {
        (
            next_input,
            Literal::Float(Token::new(
                res.parse::<f64>().expect("Could not parse float"),
                res,
            )),
        )
    })
}

fn parse_int(input: Span) -> Res<Literal> {
    context(
        "Int",
        recognize(digit1)
    )(input).map(|(next_input, res)| {
        (
            next_input,
            Literal::Int(Token::new(
                res.parse::<i32>().expect("Could not parse int"),
                res,
            )),
        )
    })
}

fn parse_num(input: Span) -> Res<Literal> {
    context("Num", recognize_float)(input).map(|(next_input, res)| {
        (
            next_input,
            Literal::Float(Token::new(res.fragment().parse::<f64>().unwrap(), res)),
        )
    })
}

fn parse_false(input: Span) -> Res<Literal> {
    context("False", tag_no_case("False"))(input)
        .map(|(next_input, res)| (next_input, Literal::Boolean(Token::new(false, res))))
}

fn parse_true(input: Span) -> Res<Literal> {
    context("True", tag_no_case("True"))(input)
        .map(|(next_input, res)| (next_input, Literal::Boolean(Token::new(true, res))))
}

fn parse_boolean(input: Span) -> Res<Literal> {
    alt((parse_false, parse_true))(input)
}

fn parse_nil(input: Span) -> Res<Literal> {
    context("Nil", tag_no_case("Nil"))(input)
        .map(|(next_input, res)| (next_input, Literal::Nil(res)))
}

fn parse_array(input: Span) -> Res<Collection> {
    context(
        "Array",
        preceded(
            char('['),
            cut(terminated(
                separated_list0(preceded(sp, char(',')), parse_expression),
                preceded(sp, char(']')),
            )),
        ),
    )(input)
    .map(|(next_input, res)| (next_input, Collection::Array(res)))
}

pub(crate) fn parse_collection<'a>(input: Span<'a>) -> Res<Collection<'a>> {
    context("Collection", preceded(sp, alt((parse_array, parse_map))))(input)
}

fn parse_key_value(input: Span) -> Res<(Span, Expression)> {
    separated_pair(
        preceded(sp, parse_str_raw),
        cut(preceded(sp, char(':'))),
        parse_expression,
    )(input)
}

// TODO: Not only literals, but also expressions are allowed in maps
fn parse_map(input: Span) -> Res<Collection> {
    context(
        "Map",
        preceded(
            char('{'),
            cut(terminated(
                map(
                    separated_list0(preceded(sp, char(',')), parse_key_value),
                    |tuple_vec| {
                        tuple_vec
                            .into_iter()
                            .map(|(k, v)| (String::from(*k.fragment()), v))
                            .collect()
                    },
                ),
                preceded(sp, char('}')),
            )),
        ),
    )(input)
    .map(|(next_input, res)| (next_input, Collection::Map(res)))
}

pub(crate) fn parse_literal<'a>(input: Span<'a>) -> Res<Literal<'a>> {
    context(
        "Literal",
        preceded(
            sp,
            alt((
                parse_nil,
                parse_boolean,
                parse_float,
                parse_int,
                parse_str,
                // parse_array,
                // parse_map,
            )),
        ),
    )(input)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::expression::ExprOrVarname;
    use crate::expression::PrefixExpr;

    #[test]
    fn test_token() {
        // Just a test to see if tokens are compared based on their value and not meta data of the
        // span
        let token1 = Token::new("hello", Span::new("Thisone"));
        let token2 = Token::new("hello", Span::new("other"));
        assert_eq!(token1, token2);
    }
    #[test]
    fn test_sp() {
        let string = " \n\n";
        let (_, res) = sp(Span::new(string)).unwrap();
        assert_eq!(res, "");
    }

    #[test]
    fn test_sp2() {
        let string = " \n\n// \n";
        let (_, res) = sp(Span::new(string)).unwrap();
        assert_eq!(res, "");
    }

    #[test]
    fn parse_nil_test() {
        let string = "Nil";
        let (_, res) = parse_nil(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Nil(Span::new("Nil")));
    }

    // #[ignore]
    // #[test]
    // fn parse_nil_test_fail() {
    //     let string = "False";
    //     let (_, res) = parse_nil(Span::new(string));
    //     assert_eq!(
    //         res,
    //         Err(nom::Err::Error(VerboseError {
    //             errors: vec![
    //                 ("False", VerboseErrorKind::Nom(ErrorKind::Tag)),
    //                 ("False", VerboseErrorKind::Context("Nil"))
    //             ]
    //         }))
    //     );
    // }

    #[test]
    fn parse_boolean_test() {
        let string = "False";
        let (_, res) = parse_boolean(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Boolean(Token::new(false, Span::new("False"))));

        let string = "True";
        let (_, res) = parse_boolean(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Boolean(Token::new(true, Span::new("True"))));

        let string = "False";
        let (_, res) = parse_literal(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Boolean(Token::new(false, Span::new("False"))));

        let string = "True";
        let (_, res) = parse_literal(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Boolean(Token::new(true, Span::new("True"))));
    }

    #[test]
    fn parse_string_test() {
        let string = "\"HelloWorld\" ";
        let (_, res) = parse_str(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Literal::Str(Token::new(
                String::from("HelloWorld"),
                Span::new("HelloWorld")
            ))
        );

        let string = "\"Hello World\" ";
        let (_, res) = parse_str(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Literal::Str(Token::new(
                String::from("Hello World"),
                Span::new("Hello World")
            ))
        );

        let string = "\"Hello    World\" ";
        let (_, res) = parse_str(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Literal::Str(Token::new(
                String::from("Hello    World"),
                Span::new("Hello    World")
            ))
        );
    }

    #[test]
    fn parse_num_test() {
        let string = "1.1";
        let (_, res) = parse_literal(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Float(Token::new(1.1, Span::new("1.1"))));

        let string = "123.0";
        let (_, res) = parse_literal(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Float(Token::new(123.0, Span::new("123.0"))));

        let string = "1";
        let (_, res) = parse_literal(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Int(Token::new(1, Span::new("1"))));
    }

    #[test]
    fn parse_array_test() {
        let string = "[1, 1, 1]";
        let (_, res) = parse_array(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Collection::Array(vec![
                Expression::Literal(Literal::Int(Token::new(1, Span::new("1")))),
                Expression::Literal(Literal::Int(Token::new(1, Span::new("1")))),
                Expression::Literal(Literal::Int(Token::new(1, Span::new("1"))))
            ])
        )
    }

    #[test]
    fn parse_array_test1() {
        let string = "[1, \"String\", 1]";
        let (_, res) = parse_array(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Collection::Array(vec![
                Expression::Literal(Literal::Int(Token::new(1, Span::new("1")))),
                Expression::Literal(Literal::Str(Token::new(
                    String::from("String"),
                    Span::new("String")
                ))),
                Expression::Literal(Literal::Int(Token::new(1, Span::new("1")))),
            ])
        )
    }

    #[test]
    fn parse_array_test1_rest() {
        let string = "[1, \"String\", 1]\nprint(x)";
        let (_, res) = parse_array(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Collection::Array(vec![
                Expression::Literal(Literal::Int(Token::new(1, Span::new("1")))),
                Expression::Literal(Literal::Str(Token::new(
                    String::from("String"),
                    Span::new("String")
                ))),
                Expression::Literal(Literal::Int(Token::new(1, Span::new("1")))),
            ])
        )
    }

    #[test]
    fn parse_array_map() {
        let string = "{}";
        let (_, res) = parse_map(Span::new(string)).unwrap();
        assert_eq!(res, Collection::Map(HashMap::new()));
    }

    #[test]
    fn parse_array_map1() {
        let string = "{\"Helllo\": 2, \"World\": 3}";
        let (_, res) = parse_map(Span::new(string)).unwrap();
        let mut hm = HashMap::new();
        hm.insert(
            String::from("Helllo"),
            Expression::Literal(Literal::Int(Token::new(2, Span::new("2")))),
        );
        hm.insert(
            String::from("World"),
            Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
        );
        assert_eq!(res, Collection::Map(hm));
    }

    #[test]
    fn parse_literal_test() {
        let string = "Nil";
        let (_, res) = parse_literal(Span::new(string)).unwrap();
        assert_eq!(res, Literal::Nil(Span::new("Nil")))
    }

    #[test]
    fn test_parse_variable() {
        let string = "hello123";
        let (_, res) = parse_variable(Span::new(string)).unwrap();
        assert_eq!(res, Token::new("hello123", Span::new("hello123")));
    }

    #[test]
    fn test_parse_variable_cap() {
        let string = "Hello123";
        let (_, res) = parse_variable(Span::new(string)).unwrap();
        assert_eq!(res, Token::new("Hello123", Span::new("Hello123")));
    }

    #[test]
    fn test_parse_variable_func() {
        let string = "hello()";
        let (_, res) = parse_variable(Span::new(string)).unwrap();
        assert_eq!(res, Token::new("hello", Span::new("hello")));
    }

    #[test]
    fn test_parse_array_expr() {
        let string = "[1, 2, hello]";
        let (_, res) = parse_array(Span::new(string)).unwrap();

        assert_eq!(
            res,
            Collection::Array(vec![
                Expression::Literal(Literal::Int(Token::new(1, Span::new("1"))),),
                Expression::Literal(Literal::Int(Token::new(2, Span::new("2"))),),
                Expression::PrefixExpr(Box::new(PrefixExpr {
                    prefix: ExprOrVarname::Varname(Token::new("hello", Span::new("hello"))),
                    suffix_chain: vec![]
                }))
            ])
        )
    }
}
