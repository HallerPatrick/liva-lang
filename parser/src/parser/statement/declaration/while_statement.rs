use crate::parser::expression::parse_expression;
use crate::parser::expression::Expression;
use crate::parser::literals::sp;
use crate::parser::tokens::{end, ldo, lwhile};
use crate::parser::Res;
use crate::Span;

use crate::parser::statement::{parse_block, Block};

use nom::{
    error::context,
    sequence::{delimited, preceded, terminated, tuple},
};

#[derive(Debug, PartialEq, Clone)]
pub struct While<'a> {
    pub cond: Expression<'a>,
    pub block: Block<'a>,
}

pub fn parse_while(input: Span) -> Res<While> {
    tuple((while_condition, terminated(parse_block, preceded(sp, end))))(input).map(
        |(next_input, res)| {
            (
                next_input,
                While {
                    cond: res.0,
                    block: res.1,
                },
            )
        },
    )
}

fn while_condition(input: Span) -> Res<Expression> {
    context(
        "WhileCond",
        delimited(
            preceded(sp, lwhile),
            preceded(sp, parse_expression),
            preceded(sp, ldo),
        ),
    )(input)
}

#[cfg(test)]
mod tests {

    use super::*;

    use crate::literals::Token;
    use crate::parser::expression::binary::BinaryOp;
    use crate::parser::expression::{ExprOrVarname, Expression, PrefixExpr};
    use crate::parser::literals::Literal;
    use crate::parser::statement::declaration::assignment::LAssignment;
    use crate::parser::statement::Statement;
    use crate::parser::tokens::Operator;

    #[test]
    fn parse_while_condition() {
        let string = " while x < 3 do";
        let (_, res) = while_condition(Span::new(string)).unwrap();

        assert_eq!(
            res,
            Expression::BinaryOp(Box::new(BinaryOp {
                op: Operator::Lt,
                left: Expression::PrefixExpr(Box::new(PrefixExpr {
                    prefix: ExprOrVarname::Varname(Token::new("x", Span::new("x"))),
                    suffix_chain: vec![],
                })),
                right: Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
            }))
        )
    }

    #[test]
    fn parse_while_statement() {
        let string = "while x < 3 do \nlet z = x + 3\n let y = 3 \nend";
        let (_, res) = parse_while(Span::new(string)).unwrap();

        assert_eq!(
            res,
            While {
                cond: Expression::BinaryOp(Box::new(BinaryOp {
                    op: Operator::Lt,
                    left: Expression::PrefixExpr(Box::new(PrefixExpr {
                        prefix: ExprOrVarname::Varname(Token::new("x", Span::new("x"))),
                        suffix_chain: vec![],
                    })),
                    right: Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
                })),
                block: Block {
                    statements: vec![
                        Statement::LAssignment(LAssignment {
                            variable: Token::new("z", Span::new("z")),
                            expression: Expression::BinaryOp(Box::new(BinaryOp {
                                left: Expression::PrefixExpr(Box::new(PrefixExpr {
                                    prefix: ExprOrVarname::Varname(Token::new("x", Span::new("x"))),
                                    suffix_chain: vec![]
                                })),
                                op: Operator::Add,
                                right: Expression::Literal(Literal::Int(Token::new(
                                    3,
                                    Span::new("3")
                                )))
                            }))
                        }),
                        Statement::LAssignment(LAssignment {
                            variable: Token::new("y", Span::new("y")),
                            expression: Expression::Literal(Literal::Int(Token::new(
                                3,
                                Span::new("3")
                            )))
                        })
                    ],
                    return_stmt: None
                }
            }
        )
    }
}
