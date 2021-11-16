//! Collection of all parsers and structs which represent tokens, operators and keywords

use crate::parser::literals::sp;
use crate::parser::{Res, Span};

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::error::context;
use nom::sequence::preceded;

/// Reserved keywords for the liva lang, which can not be used as identifiers
pub const KEYWORDS: [&str; 14] = [
    "return", "class", "end", "fun", "do", "while", "for", "if", "let", "in", "else", "external",
    "as", "import",
];

lazy_static! {

    /// Vector of unary operators which show the precedence of the operators
    /// with which unary expressions should be evaluated
    pub static ref UNOPS: Vec<UnOperator> = {
        let table = &mut [UnOperator::Sub, UnOperator::Not];
        table.sort();
        table.to_vec()
    };

    /// Vector of binary operators which show the precedence of the operators
    /// with which binary expressions should be evaluated
    pub static ref BINOP_PRECEDENCE: Vec<Vec<Operator>> = {
        let table: &mut [&mut [Operator]] = &mut [
            &mut [Operator::Pow],
            &mut [Operator::Mul, Operator::Div, Operator::Mod],
            &mut [Operator::Add, Operator::Sub],
            // &mut [Operator::Concat],
            // &mut [Operator::BitShl, BinOp::BitShr],
            // &mut [Operator::BitAnd],
            // &mut [Operator::BitXor],
            // &mut [Operator::BitOr],
            &mut [
                Operator::Lt,
                Operator::Gt,
                Operator::Leq,
                Operator::Geq,
                Operator::Neq,
                Operator::EQ,
            ],
            &mut [Operator::And],
            &mut [Operator::Or],
        ];
        let mut acc = Vec::new();
        for s in table.iter_mut() {
            s.sort();
            acc.push(s.to_vec());
        }
        acc
    };
}

macro_rules! define_token {
    ( $( { $fn_name:ident, $name:literal, $token:literal } ), *) => {
        $(
            /// Macro generated function
            /// Token parser to parse token
            pub fn $fn_name(input: Span) -> Res<Span> {
                context(
                    $name,
                    tag($token)
                )(input)
            }
        )*
    }
}

define_token! {
    {add, "Add", "+"},
    {sub, "Sub", "-"},
    {mul, "Mul", "*"},
    {div, "Div", "/"},
    {modulo, "Modulu", "%"},
    {equal, "Equal", "=="},
    {unequal, "Unequal", "!="},
    {dot, "Dot", "."},
    {comma, "Comma", ","},
    {left_paren, "LeftParen", "("},
    {right_paren, "RightParen", ")"},
    {left_bracket, "LeftBracket", "["},
    {right_bracket, "RightBracket", "]"},
    {less_than, "LessThan", "<"},
    {greater_than, "GreaterThan", ">"},
    {less_eq_than, "LessEqThan", "<="},
    {greater_eq_than, "GreaterEqThan", ">="},
    {comment, "Comment", "//"},
    {newline, "Newline", "\n"}
}

define_token! {
    {lreturn, "Return", "return"},
    {class, "Class", "class"},
    {end, "End", "end"},
    {fun, "Fun", "fun"},
    {ldo, "Do", "do"},
    {lwhile, "While", "while"},
    {lfor, "For", "for"},
    {lif, "If", "if"},
    {lelse, "Else", "else"},
    {llet, "Let", "let"},
    {lin, "In", "in"},
    {lor, "Or", "or"},
    {land, "And", "and"},
    {external, "External", "external"},
    {import, "Import", "import"},
    {las, "As", "as"}
}

/// All Operators which are used in the language
/// for binary expressions
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Operator {
    Add,
    Sub,
    Div,
    Mul,
    Pow,
    Mod,
    EQ,
    And,
    Lt,
    Gt,
    Or,
    Leq,
    Geq,
    Neq,
}

/// All Operators which are used in the language
/// for unary expressions
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum UnOperator {
    Add,
    Sub,
    Not,
}

pub(crate) fn parse_unary_operator(input: Span) -> Res<UnOperator> {
    context("UnaryOperator", preceded(sp, alt((add, sub, mul, div))))(input).map(
        |(next_input, res)| {
            match *res.fragment() {
                "+" => (next_input, UnOperator::Add),
                "-" => (next_input, UnOperator::Sub),
                "not" => (next_input, UnOperator::Not),
                // This should never reach!
                _ => (next_input, UnOperator::Add),
            }
        },
    )
}

pub(crate) fn parse_binary_operator(input: Span) -> Res<Operator> {
    context(
        "Operator",
        preceded(
            sp,
            alt((
                add,
                sub,
                mul,
                div,
                equal,
                unequal,
                less_than,
                greater_than,
                greater_eq_than,
                less_eq_than,
                land,
                modulo,
            )),
        ),
    )(input)
    .map(|(next_input, res)| {
        match *res.fragment() {
            "+" => (next_input, Operator::Add),
            "-" => (next_input, Operator::Sub),
            "*" => (next_input, Operator::Mul),
            "/" => (next_input, Operator::Div),
            "%" => (next_input, Operator::Mod),
            "==" => (next_input, Operator::EQ),
            "<" => (next_input, Operator::Lt),
            ">" => (next_input, Operator::Gt),
            "<=" => (next_input, Operator::Leq),
            ">=" => (next_input, Operator::Geq),
            "and" => (next_input, Operator::And),
            // This should never reach!
            _ => (next_input, Operator::Add),
        }
    })
}

pub(crate) fn parse_tokens(input: Span) -> Res<Span> {
    alt((
        add,
        sub,
        mul,
        div,
        dot,
        equal,
        unequal,
        left_paren,
        right_paren,
        less_than,
        greater_than,
        less_eq_than,
        greater_eq_than,
        modulo,
    ))(input)
}
