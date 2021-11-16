//! Collection of all statements

pub mod declaration;
pub mod import;

use nom::branch::alt;
use nom::character::complete::line_ending;
use nom::combinator::{map, opt};
use nom::error::context;
use nom::multi::{many0, separated_list0};
use nom::sequence::{preceded, tuple};
use nom::Err;

use crate::parser::{
    expression::{parse_expression, prefixexpr, ExprSuffix, Expression, PrefixExpr},
    literals::sp,
    statement::declaration::{
        assignment::{Assignment, LAssignment},
        class::Class,
        for_statement::For,
        function::Function,
        if_statement::If,
        while_statement::While,
    },
    statement::import::{parse_import, Import},
    tokens::{comma, lreturn},
    Res, Span,
};

use declaration::{
    assignment::{parse_assignment, parse_lassignment},
    class::parse_class,
    for_statement::parse_for,
    function::parse_function,
    if_statement::parse_if,
    while_statement::parse_while,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
    pub return_stmt: Option<ReturnStmt<'a>>,
}

impl<'a> IntoIterator for Block<'a> {
    type Item = Statement<'a>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Assignment(Assignment<'a>),
    LAssignment(LAssignment<'a>),
    FuncCall(PrefixExpr<'a>),
    While(While<'a>),
    For(For<'a>),
    If(If<'a>),
    Fun(Function<'a>),
    Class(Class<'a>),
    Return(ReturnStmt<'a>),
    Import(Import<'a>),
}

pub fn parse_block(input: Span) -> Res<Block> {
    context(
        "Block",
        tuple((many0(parse_statement), opt(parse_return_stmt))),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            Block {
                statements: res.0,
                return_stmt: res.1,
            },
        )
    })
}

/// Parse a single statement into a [`Statement`]
///
/// # Example
///
/// ```
/// use liva_parser::{
///     Span,
///     parse_statement,
///     statement::{
///         Statement,
///         declaration::assignment::LAssignment
///     },
///     expression::Expression,
///     literals::{Literal, Token}
/// };
///
/// let input = Span::new("let x = 3");
/// let (_, res) = parse_statement(input).unwrap();
/// assert_eq!(
///     res,
///     Statement::LAssignment(LAssignment {
///         variable: Token::new("x", Span::new("x")),
///         expression: Expression::Literal(Literal::Int(Token::new(3, Span::new("3"))))
///     })
/// );
/// ```
pub fn parse_statement(input: Span) -> Res<Statement> {
    context(
        "Stmt",
        preceded(
            sp,
            alt((
                map(parse_assignment, Statement::Assignment),
                map(parse_lassignment, Statement::LAssignment),
                map(parse_if, Statement::If),
                map(parse_for, Statement::For),
                map(parse_while, Statement::While),
                map(parse_function, Statement::Fun),
                map(parse_class, Statement::Class),
                map(parse_import, Statement::Import),
                map(parse_function_call, Statement::FuncCall),
            )),
        ),
    )(input)
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStmt<'a> {
    pub values: Vec<Expression<'a>>,
}

fn parse_function_call(input: Span) -> Res<PrefixExpr> {
    let func_call_expr = prefixexpr(input);

    let is_func_call = match func_call_expr {
        // There should be an uncatched unwrap
        // Shoudl return a error message
        Ok((_, ref o)) => matches!(o.suffix_chain.last().unwrap(), ExprSuffix::FuncCall(_)),
        _ => false,
    };

    if is_func_call {
        func_call_expr
    } else {
        Err(Err::Error(nom::error::VerboseError { errors: vec![] }))
    }
}

fn parse_return_stmt(input: Span) -> Res<ReturnStmt> {
    context(
        "ReturnStmt",
        preceded(preceded(sp, lreturn), preceded(sp, parse_return_list)),
    )(input)
    .map(|(next_input, res)| (next_input, ReturnStmt { values: res }))
}

fn parse_return_list(input: Span) -> Res<Vec<Expression>> {
    context(
        "ReturnList",
        preceded(
            sp,
            separated_list0(preceded(sp, comma), preceded(sp, parse_expression)),
        ),
    )(input)
}

pub(crate) fn opt_line_ending(input: Span) -> Res<&str> {
    opt(many0(line_ending))(input).map(|(next_input, _)| (next_input, ""))
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::parser::expression::binary::BinaryOp;
    use crate::parser::expression::call::Call;
    use crate::parser::expression::ExprOrVarname;
    use crate::parser::literals::{Literal, Token};
    use crate::parser::tokens::Operator;

    fn ass_x_eq_3() -> Statement<'static> {
        Statement::LAssignment(LAssignment {
            variable: Token::new("x", Span::new("x")),
            expression: Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
        })
    }

    #[test]
    fn test_parse_assignemnt() {
        let string = " let x = 3";
        let (_, res) = parse_statement(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Statement::LAssignment(LAssignment {
                variable: Token::new("x", Span::new("x")),
                expression: Expression::Literal(Literal::Int(Token::new(3, Span::new("3.0"))))
            })
        );
    }

    #[test]
    fn test_parse_assignemnt_line_ending() {
        let string = " let x = 3\n\n";
        let (_, res) = parse_statement(Span::new(string)).unwrap();
        assert_eq!(res, ass_x_eq_3());
    }

    #[test]
    fn test_parse_assignemnt_line_ending_indented_newline() {
        let string = "\n\t let x = 3\n\n";
        let (_, res) = parse_statement(Span::new(string)).unwrap();
        assert_eq!(res, ass_x_eq_3());
    }

    #[test]
    fn test_parse_statements() {
        let string = "let x = 3\n let y = 3";
        let (_, res) = parse_statement(Span::new(string)).unwrap();
        assert_eq!(res, ass_x_eq_3())
    }

    #[test]
    fn test_return_stmt() {
        let string = "return hello, 3";
        let (_, res) = parse_return_stmt(Span::new(string)).unwrap();
        assert_eq!(
            res,
            ReturnStmt {
                values: vec![
                    Expression::PrefixExpr(Box::new(PrefixExpr {
                        prefix: ExprOrVarname::Varname(Token::new("hello", Span::new("hello"))),
                        suffix_chain: vec![]
                    })),
                    Expression::Literal(Literal::Int(Token::new(3, Span::new("3"))))
                ]
            }
        )
    }

    #[test]
    fn test_return_stmt_2() {
        let string = "return hello, func(n)";
        let (_, res) = parse_return_stmt(Span::new(string)).unwrap();
        assert_eq!(
            res,
            ReturnStmt {
                values: vec![
                    Expression::PrefixExpr(Box::new(PrefixExpr {
                        prefix: ExprOrVarname::Varname(Token::new("hello", Span::new("hello"))),
                        suffix_chain: vec![]
                    })),
                    Expression::PrefixExpr(Box::new(PrefixExpr {
                        prefix: ExprOrVarname::Varname(Token::new("func", Span::new("func"))),
                        suffix_chain: vec![ExprSuffix::FuncCall(Call {
                            callee: None,
                            args: vec![Expression::PrefixExpr(Box::new(PrefixExpr {
                                prefix: ExprOrVarname::Varname(Token::new("n", Span::new("n"))),
                                suffix_chain: vec![]
                            }))]
                        })]
                    }))
                ]
            }
        )
    }

    #[test]
    fn test_return_stmt_3() {
        let string = "return fib(n-1)";
        let (_, res) = parse_return_stmt(Span::new(string)).unwrap();
        assert_eq!(
            res,
            ReturnStmt {
                values: vec![Expression::PrefixExpr(Box::new(PrefixExpr {
                    prefix: ExprOrVarname::Varname(Token::new("fib", Span::new("fib"))),
                    suffix_chain: vec![ExprSuffix::FuncCall(Call {
                        callee: None,
                        args: vec![Expression::BinaryOp(Box::new(BinaryOp {
                            op: Operator::Sub,
                            left: Expression::PrefixExpr(Box::new(PrefixExpr {
                                prefix: ExprOrVarname::Varname(Token::new("n", Span::new("n"))),
                                suffix_chain: vec![]
                            })),
                            right: Expression::Literal(Literal::Int(Token::new(
                                1,
                                Span::new("1")
                            )))
                        }))]
                    })]
                }))]
            }
        )
    }

    #[test]
    fn test_stmt_leading_nl_sp() {
        let string = "\n\n// Hello Test \nfun hello() end";
        let (_, res) = parse_statement(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Statement::Fun(Function {
                name: "hello",
                parameters: vec![],
                block: Block {
                    statements: vec![],
                    return_stmt: None
                }
            })
        )
    }

    #[test]
    fn empty_block() {
        let string = "";
        let (_, res) = parse_block(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Block {
                statements: vec![],
                return_stmt: None
            }
        )
    }

    #[test]
    fn test_while_stmt() {
        let string = "while x < 3 do\n\nend";
        let (_, res) = parse_statement(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Statement::While(While {
                cond: Expression::BinaryOp(Box::new(BinaryOp {
                    left: Expression::PrefixExpr(Box::new(PrefixExpr {
                        prefix: ExprOrVarname::Varname(Token::new("x", Span::new("x"))),
                        suffix_chain: vec![]
                    })),
                    right: Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
                    op: Operator::Lt
                })),
                block: Block {
                    statements: vec![],
                    return_stmt: None
                }
            })
        )
    }

    #[test]
    fn test_while_stmt2() {
        let string = "while x < 3 do\nprint(\"Hello\")\nend";
        let (_, res) = parse_statement(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Statement::While(While {
                cond: Expression::BinaryOp(Box::new(BinaryOp {
                    left: Expression::PrefixExpr(Box::new(PrefixExpr {
                        prefix: ExprOrVarname::Varname(Token::new("x", Span::new("x"))),
                        suffix_chain: vec![]
                    })),
                    right: Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
                    op: Operator::Lt
                })),
                block: Block {
                    statements: vec![Statement::FuncCall(PrefixExpr {
                        prefix: ExprOrVarname::Varname(Token::new("print", Span::new("print"))),
                        suffix_chain: vec![ExprSuffix::FuncCall(Call {
                            callee: None,
                            args: vec![Expression::Literal(Literal::Str(Token::new(
                                String::from("Hello"),
                                Span::new("Hello")
                            )))]
                        })]
                    })],
                    return_stmt: None
                }
            })
        )
    }
}
