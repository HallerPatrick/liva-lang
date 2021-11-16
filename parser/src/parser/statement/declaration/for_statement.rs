use nom::{
    error::context,
    sequence::{delimited, preceded, terminated, tuple},
};

use crate::parser::expression::{parse_expression, Expression};
use crate::parser::literals::sp;
use crate::parser::literals::{parse_variable, Variable};
use crate::parser::statement::{parse_block, Block};
use crate::parser::tokens::{end, ldo, lfor, lin};
use crate::parser::{Res, Span};

#[derive(Debug, PartialEq, Clone)]
pub struct For<'a> {
    pub iter_item: Variable<'a>,
    pub iterator: Expression<'a>,
    pub block: Block<'a>,
}

pub fn parse_for(input: Span) -> Res<For> {
    tuple((
        parse_iter_item,
        parse_iterator,
        terminated(parse_block, preceded(sp, end)),
    ))(input)
    .map(|(next_input, res)| {
        (
            next_input,
            For {
                iter_item: res.0,
                iterator: res.1,
                block: res.2,
            },
        )
    })
}

fn parse_iter_item<'a>(input: Span<'a>) -> Res<Variable<'a>> {
    context(
        "ForIterItem",
        preceded(preceded(sp, lfor), preceded(sp, parse_variable)),
    )(input)
}

fn parse_iterator(input: Span) -> Res<Expression> {
    context(
        "ForIterator",
        delimited(
            preceded(sp, lin),
            preceded(sp, parse_expression),
            preceded(sp, ldo),
        ),
    )(input)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::literals::Token;
    use crate::parser::expression::{ExprOrVarname, Expression, PrefixExpr};
    use crate::parser::literals::Literal;
    use crate::parser::statement::{Block, LAssignment, Statement};

    #[test]
    fn test_parse_iter_item() {
        let string = "for item in iterator do";
        let (_, res) = parse_iter_item(Span::new(string)).unwrap();
        assert_eq!(res, Token::new("item", Span::new("item")));
    }

    #[test]
    fn test_parse_iterator() {
        let string = " in iterator do";
        let (_, res) = parse_iterator(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Expression::PrefixExpr(Box::new(PrefixExpr {
                prefix: ExprOrVarname::Varname(Token::new("iterator", Span::new("iterator"))),
                suffix_chain: vec![]
            }))
        );
    }

    #[test]
    fn test_parse_for_loop() {
        let string = "\nfor x in y do \n let y = 3 \nend";
        let (_, res) = parse_for(Span::new(string)).unwrap();

        assert_eq!(
            res,
            For {
                iter_item: Token::new("x", Span::new("x")),
                iterator: Expression::PrefixExpr(Box::new(PrefixExpr {
                    prefix: ExprOrVarname::Varname(Token::new("y", Span::new("y"))),
                    suffix_chain: vec![]
                })),
                block: Block {
                    statements: vec![Statement::LAssignment(LAssignment {
                        variable: Token::new("y", Span::new("y")),
                        expression: Expression::Literal(Literal::Int(Token::new(
                            3,
                            Span::new("3")
                        )))
                    })],
                    return_stmt: None
                }
            }
        )
    }
}
