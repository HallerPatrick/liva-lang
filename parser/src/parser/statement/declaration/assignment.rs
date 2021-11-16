use crate::parser::expression::{parse_expression, prefixexpr, Expression, PrefixExpr};
use crate::parser::tokens::llet;
use crate::parser::{
    literals::{parse_variable, sp, Variable},
    Res, Span,
};

use nom::{
    character::complete::char,
    error::context,
    sequence::{preceded, separated_pair},
};

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment<'a> {
    pub variable: PrefixExpr<'a>,
    pub expression: Expression<'a>,
}

/// A assignment is a statement, while a re-assignemnt is an epression
/// so we only parse a statement here, therefore is the let mandatory
#[derive(Debug, PartialEq, Clone)]
pub struct LAssignment<'a> {
    pub variable: Variable<'a>,
    pub expression: Expression<'a>,
}

/// Assignment having following schema
/// <let-keyword> <variable> = <expression>
pub(crate) fn parse_assignment(input: Span) -> Res<Assignment> {
    context(
        "Assignment",
        preceded(
            sp,
            separated_pair(
                preceded(sp, prefixexpr),
                preceded(sp, char('=')),
                parse_expression,
            ),
        ),
    )(input)
    .map(|(next_input, (variable, expression))| {
        (
            next_input,
            Assignment {
                variable,
                expression,
            },
        )
    })
}

/// Assignment having following schema
/// <let-keyword> <variable> = <expression>
pub(crate) fn parse_lassignment(input: Span) -> Res<LAssignment> {
    context(
        "LAssignment",
        preceded(
            preceded(sp, llet),
            preceded(
                sp,
                separated_pair(
                    preceded(sp, parse_variable),
                    preceded(sp, char('=')),
                    parse_expression,
                ),
            ),
        ),
    )(input)
    .map(|(next_input, (variable, expression))| {
        (
            next_input,
            LAssignment {
                variable,
                expression,
            },
        )
    })
}

#[cfg(test)]
mod tests {

    use super::*;

    use crate::literals::Token;
    use crate::parser::expression::Expression;
    use crate::parser::literals::{Collection, Literal};

    #[test]
    fn test_assignment() {
        let string = "let x = 3";
        let (_, res) = parse_lassignment(Span::new(string)).unwrap();
        assert_eq!(
            res,
            LAssignment {
                variable: Token::new("x", Span::new("x")),
                expression: Expression::Literal(Literal::Int(Token::new(3, Span::new("3"))))
            }
        );
    }

    #[test]
    fn test_assignment_list() {
        let string = "let x = [1, 2, 3]\nlet x = 1";
        let (rest, res) = parse_lassignment(Span::new(string)).unwrap();
        println!("{}", rest);
        assert_eq!(
            res,
            LAssignment {
                variable: Token::new("x", Span::new("x")),
                expression: Expression::Collection(Collection::Array(vec![
                    Expression::Literal(Literal::Int(Token::new(1, Span::new("1")))),
                    Expression::Literal(Literal::Int(Token::new(2, Span::new("2")))),
                    Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
                ]))
            }
        );
    }
}
