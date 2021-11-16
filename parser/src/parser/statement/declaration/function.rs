//!
//! Functions are declared with the `fun` keyword, followed by the
//! name of the function. The naming convention follows variables
//! names.
//!
//! Function can accept multiple arguments. A function ends with
//! the `end` keyword.
//!
//! ```code
//!
//! fun foo(arg1, arg2)
//!     return arg1 + arg2
//! end
//!
//! ```
//!
use crate::literals::Variable;
use crate::parser::{
    literals::{parse_variable, parse_variable_raw, sp, Identifier},
    statement::opt_line_ending,
    statement::parse_block,
    statement::Block,
    tokens::{end, fun, left_paren, right_paren},
    Res, Span,
};

use nom::{
    character::complete::char,
    error::context,
    multi::separated_list0,
    sequence::{delimited, preceded},
    sequence::{terminated, tuple},
};

// TODO: Impl to get return stmt
// TODO: Function.name does not have to be a literal, but can be str
/// Represents a function declaration.
#[derive(Debug, PartialEq, Clone)]
pub struct Function<'a> {
    /// Name of the function
    pub name: Identifier<'a>,

    /// Parameter list, of the function
    pub parameters: Vec<Variable<'a>>,

    // TODO: Rename to block
    /// Block contains all statements which
    /// are executed, when the function is called
    pub block: Block<'a>,
}

/// Parses a function declaration.
pub fn parse_function(input: Span) -> Res<Function> {
    context(
        "Func",
        terminated(
            tuple((parse_function_name, parse_function_arguments, parse_block)),
            preceded(sp, end),
        ),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            Function {
                name: res.0,
                parameters: res.1,
                block: res.2,
            },
        )
    })
}

fn parse_function_name(input: Span) -> Res<&str> {
    context(
        "FuncName",
        preceded(
            opt_line_ending,
            preceded(sp, preceded(fun, preceded(sp, parse_variable_raw))),
        ),
    )(input)
    .map(|(next_input, res)| (next_input, *res.fragment()))
}

fn parse_function_arguments<'a>(input: Span<'a>) -> Res<Vec<Variable>> {
    context(
        "ParameterList",
        preceded(
            sp,
            delimited(
                left_paren,
                separated_list0(preceded(sp, char(',')), preceded(sp, parse_variable)),
                preceded(sp, right_paren),
            ),
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
    use crate::parser::statement::{If, LAssignment, ReturnStmt, Statement};
    use crate::parser::tokens::Operator;

    #[test]
    fn test_parse_function_name() {
        let string = "fun hello ()";
        let (_, res) = parse_function_name(Span::new(string)).unwrap();
        assert_eq!(res, "hello");
    }

    #[test]
    fn test_parse_function() {
        let string = "fun hello(x, y)\n\tlet some = \"1\"\nend";
        let (_, res) = parse_function(Span::new(string)).unwrap();

        assert_eq!(
            res,
            Function {
                name: "hello",
                parameters: vec![
                    Token::new("x", Span::new("x")),
                    Token::new("y", Span::new("y"))
                ],
                block: Block {
                    statements: vec![Statement::LAssignment(LAssignment {
                        variable: Token::new("some", Span::new("some")),
                        expression: Expression::Literal(Literal::Str(Token::new(
                            String::from("1"),
                            Span::new("1")
                        )))
                    })],
                    return_stmt: None
                }
            }
        );
    }

    #[test]
    fn test_fun_1() {
        let string = "fun fib(n)\n\nend";
        let (_, res) = parse_function(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Function {
                name: "fib",
                parameters: vec![Token::new("n", Span::new("n"))],
                block: Block {
                    statements: vec![],
                    return_stmt: None
                }
            }
        );
    }

    #[test]
    fn test_fun_return() {
        let string = "\n\n\tfun fib(n)\n\t\treturn 'Hello World'\n\tend";
        let (_, res) = parse_function(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Function {
                name: "fib",
                parameters: vec![Token::new("n", Span::new("n"))],
                block: Block {
                    statements: vec![],
                    return_stmt: Some(ReturnStmt {
                        values: vec![Expression::Literal(Literal::Str(Token::new(
                            String::from("Hello World"),
                            Span::new("Hello World")
                        )))],
                    })
                }
            }
        );
    }

    #[test]
    #[test]
    fn test_fun() {
        let string = "fun fib(n)\n    if n == 0  do\n        return 0\n    end\nend";
        let (_, res) = parse_function(Span::new(string)).unwrap();

        assert_eq!(
            res,
            Function {
                name: "fib",
                parameters: vec![Token::new("n", Span::new("n"))],
                block: Block {
                    statements: vec![Statement::If(If {
                        cond: Expression::BinaryOp(Box::new(BinaryOp {
                            left: Expression::PrefixExpr(Box::new(PrefixExpr {
                                prefix: ExprOrVarname::Varname(Token::new("n", Span::new("n"))),
                                suffix_chain: vec![]
                            })),
                            op: Operator::EQ,
                            right: Expression::Literal(Literal::Int(Token::new(
                                0,
                                Span::new("0")
                            )))
                        })),
                        stmts: Block {
                            statements: vec![],
                            return_stmt: Some(ReturnStmt {
                                values: vec![Expression::Literal(Literal::Int(Token::new(
                                    0,
                                    Span::new("0")
                                )))]
                            })
                        },
                        else_statements: None
                    })],
                    return_stmt: None
                }
            }
        )
    }

    #[test]
    fn test_fun_2() {
        let string = "fun init(self, height, weight)\r\n       self.height = height\r\n       self.weight = weight \r\n    end\r\n\r\n     ";
        let (_, _) = parse_function(Span::new(string)).unwrap();
        // println!("{:?}", res);
    }
}
