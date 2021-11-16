//! Logic for expressions and their representation

pub mod binary;
pub mod call;

use crate::literals::{Collection, Variable};
use crate::parser::{
    literals::{parse_collection, parse_literal, parse_variable, sp, Literal},
    tokens::{
        dot, left_bracket, left_paren, parse_binary_operator, parse_unary_operator, right_bracket,
        right_paren, Operator, UnOperator, BINOP_PRECEDENCE, UNOPS,
    },
    Res, Span,
};

use crate::parser::expression::binary::{BinaryOp, UnaryOp};
use call::{args, parse_call, Call};

use nom::{
    branch::alt,
    combinator::map,
    error::context,
    multi::many0,
    sequence::{delimited, preceded, tuple},
};

// Represent a token or literal in a expression
#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Call(Call<'a>),
    BinaryOp(Box<BinaryOp<'a>>),
    UnaryOp(Box<UnaryOp<'a>>),
    PrefixExpr(Box<PrefixExpr<'a>>),
    Collection(Collection<'a>),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Expression2<'a> {
    Literal(Literal<'a>),
    Collection(Collection<'a>),
    PrefixExpr(PrefixExpr<'a>),
}

fn parse_flatexp(input: Span) -> Res<FlatExpr> {
    context("FlatExpr", tuple((parse_head, parse_bin_op_chain)))(input)
        .map(|(next_input, res)| (next_input, flat_expr_from_components(res.0, res.1)))
}

pub(crate) fn parse_expression(input: Span) -> Res<Expression> {
    map(parse_flatexp, Expression::from)(input)
}

pub(crate) fn parse_expression2(input: Span) -> Res<Expression2> {
    context(
        "Expression2",
        preceded(
            sp,
            alt((
                // The order is important
                // TODO: Figure out if we need to parse literals, this is done by prefixexpr
                map(parse_literal, Expression2::Literal),
                map(parse_collection, Expression2::Collection),
                map(prefixexpr, Expression2::PrefixExpr),
            )),
        ),
    )(input)
}

#[derive(Clone, PartialEq, Debug)]
struct ExprHead<'a> {
    un_ops: Vec<UnOperator>,
    expr: Expression2<'a>,
}

fn parse_head(input: Span) -> Res<ExprHead> {
    context(
        "NestedExpression",
        preceded(
            sp,
            tuple((many0(parse_unary_operator), preceded(sp, parse_expression2))),
        ),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            ExprHead {
                un_ops: res.0,
                expr: res.1,
            },
        )
    })
}

fn parse_bin_op_chain(input: Span) -> Res<Vec<(Operator, ExprHead)>> {
    context(
        "OpChain",
        preceded(
            sp,
            many0(tuple((parse_binary_operator, preceded(sp, parse_head)))),
        ),
    )(input)
}

#[derive(Clone, PartialEq, Debug)]
pub struct PrefixExpr<'a> {
    pub prefix: ExprOrVarname<'a>,
    pub suffix_chain: Vec<ExprSuffix<'a>>,
}

/// This parser deals with all kind of suffix expressions which are part
/// of a single expression
pub(crate) fn prefixexpr(input: Span) -> Res<PrefixExpr> {
    context(
        "PrefixExpr",
        preceded(sp, tuple((parse_prefix_expr, many0(parse_suffix)))),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            PrefixExpr {
                prefix: res.0,
                suffix_chain: res.1,
            },
        )
    })
}

#[derive(Clone, PartialEq, Debug)]
pub enum ExprOrVarname<'a> {
    Exp(Expression<'a>),
    Varname(Variable<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct FlatExpr<'a>(Vec<OpOrExp2<'a>>);

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum OpOrExp2<'a> {
    Op(UnOrBinOp),
    Exp2(Expression2<'a>),
}

#[derive(Debug)]
enum OpOrExp<'a> {
    Op(UnOrBinOp),
    Exp(Expression<'a>),
}

impl<'a> OpOrExp<'a> {
    fn is_binop(&self) -> bool {
        matches!(self, OpOrExp::Op(UnOrBinOp::BinOp(_)))
    }

    fn is_unop(&self) -> bool {
        matches!(self, OpOrExp::Op(UnOrBinOp::UnOp(_)))
    }

    fn is_exp(&self) -> bool {
        matches!(self, OpOrExp::Exp(_))
    }
}

impl<'a> From<FlatExpr<'a>> for Expression<'a> {
    fn from(fe: FlatExpr<'a>) -> Expression<'a> {
        // println!("{:#?}", fe);
        // Helper function. Expects a,b to be Exps and o to be a BinOp
        fn merge_nodes_binop<'a>(a: OpOrExp<'a>, o: OpOrExp<'a>, b: OpOrExp<'a>) -> OpOrExp<'a> {
            match (a, o, b) {
                (OpOrExp::Exp(a), OpOrExp::Op(UnOrBinOp::BinOp(o)), OpOrExp::Exp(b)) => {
                    let merged_exp = Expression::BinaryOp(Box::new(BinaryOp {
                        left: a,
                        op: o,
                        right: b,
                    }));
                    OpOrExp::Exp(merged_exp)
                }
                _ => panic!("unexpected input variants in merge_nodes_binop"),
            }
        }

        // Helper function. Expects o to be a UnOp and a to be an Exp
        fn merge_nodes_unop<'a>(o: OpOrExp<'a>, a: OpOrExp<'a>) -> OpOrExp<'a> {
            match (o, a) {
                (OpOrExp::Op(UnOrBinOp::UnOp(o)), OpOrExp::Exp(a)) => {
                    let merged_exp = Expression::UnaryOp(Box::new(UnaryOp { op: o, operand: a }));
                    OpOrExp::Exp(merged_exp)
                }
                _ => panic!("unexpected input variants in merge_nodes_unop"),
            }
        }

        // TODO: make this more efficient
        fn merge_all_binops(explist: &mut Vec<OpOrExp>, binops: &[Operator]) {
            loop {
                let mut tojoin_idx: Option<usize> = None;
                for (i, oe) in explist.iter().enumerate().filter(|&(_, oe)| oe.is_binop()) {
                    match *oe {
                        OpOrExp::Op(UnOrBinOp::BinOp(ref o)) => {
                            // Found something to join
                            if binops.binary_search(o).is_ok() {
                                assert!(i > 0);
                                assert!(explist[i - 1].is_exp());
                                assert!(i.checked_add(1).is_some());
                                let next = explist.get(i + 1).unwrap();

                                // If UnOps haven't been merged yet, ignore them. If there are two
                                // subsequent binops, that's an error. Otherwise we have a $ b
                                // where a and b are Exps and $ is a BinOp
                                match *next {
                                    OpOrExp::Op(UnOrBinOp::UnOp(_)) => continue,
                                    OpOrExp::Op(UnOrBinOp::BinOp(_)) => {
                                        panic!("encountered two binops next to each other");
                                    }
                                    OpOrExp::Exp(_) => {
                                        tojoin_idx = Some(i);
                                        break;
                                    }
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                if let Some(i) = tojoin_idx {
                    let a = explist.remove(i - 1);
                    let o = explist.remove(i - 1);
                    let b = explist.remove(i - 1);
                    let merged = merge_nodes_binop(a, o, b);
                    explist.insert(i - 1, merged);
                }
                // Joined everything we could. Break
                else {
                    break;
                }
            }
        }

        fn merge_all_unops(explist: &mut Vec<OpOrExp>, unops: &[UnOperator]) {
            loop {
                let mut tojoin_idx: Option<usize> = None;
                // Reverse iterate, since we want to apply stacked unary operators right-to-left
                for (i, oe) in explist
                    .iter()
                    .enumerate()
                    .filter(|&(_, oe)| oe.is_unop())
                    .rev()
                {
                    match *oe {
                        OpOrExp::Op(UnOrBinOp::UnOp(ref o)) => {
                            // Found something to join
                            if unops.binary_search(o).is_ok() {
                                assert!(i.checked_add(1).is_some());
                                let next = explist.get(i + 1).unwrap();
                                assert!(next.is_exp());

                                tojoin_idx = Some(i);
                                break;
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                if let Some(i) = tojoin_idx {
                    let o = explist.remove(i);
                    let a = explist.remove(i);
                    let merged = merge_nodes_unop(o, a);
                    explist.insert(i, merged);
                }
                // Joined everything we could. Break
                else {
                    break;
                }
            }
        }

        // First apply the highest-precedent binop (^) where applicable. This will miss cases
        // like 10 ^ #"hi" == 100. So then apply all unops, then apply all binops, starting again
        // from the highest-precedent one.

        let mut explist: Vec<OpOrExp> =
            fe.0.into_iter()
                .map(|oe| match oe {
                    OpOrExp2::Op(o) => OpOrExp::Op(o),
                    OpOrExp2::Exp2(e) => OpOrExp::Exp(Expression::from(e)),
                })
                .collect();

        // First pass: find all triplets of the form a $ b where a and b are Exps and $ is a binop
        // of the highest precedence
        merge_all_binops(&mut explist, &*BINOP_PRECEDENCE[0]);
        merge_all_unops(&mut explist, &*UNOPS);

        for binops in BINOP_PRECEDENCE.iter() {
            merge_all_binops(&mut explist, &*binops);
        }

        assert_eq!(explist.len(), 1, "Exp tree construction didn't complete");
        match explist.pop().unwrap() {
            OpOrExp::Exp(e) => e,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnOrBinOp {
    UnOp(UnOperator),
    BinOp(Operator),
}

fn tuple_to_flat_vec(head: ExprHead) -> Vec<OpOrExp2> {
    let mut v: Vec<OpOrExp2> = head
        .un_ops
        .into_iter()
        .map(UnOrBinOp::UnOp)
        .map(OpOrExp2::Op)
        .collect();
    v.push(OpOrExp2::Exp2(head.expr));
    v
}

fn flat_expr_from_components<'a>(
    head: ExprHead<'a>,
    binop_chain: Vec<(Operator, ExprHead<'a>)>,
) -> FlatExpr<'a> {
    let acc = tuple_to_flat_vec(head);
    let res = binop_chain.into_iter().fold(acc, |mut a, head| {
        a.push(OpOrExp2::Op(UnOrBinOp::BinOp(head.0)));
        a.extend_from_slice(&*tuple_to_flat_vec(head.1));
        a
    });

    FlatExpr(res)
}

fn parse_prefix_expr(input: Span) -> Res<ExprOrVarname> {
    preceded(
        sp,
        alt((
            map(
                delimited(left_paren, parse_expression, right_paren),
                ExprOrVarname::Exp,
            ),
            map(parse_variable, ExprOrVarname::Varname),
        )),
    )(input)
}

#[derive(Clone, PartialEq, Debug)]
pub enum ExprSuffix<'a> {
    TableDot(Variable<'a>),
    TableIdx(Expression<'a>),
    FuncCall(Call<'a>),
}

fn parse_suffix(input: Span) -> Res<ExprSuffix> {
    preceded(
        sp,
        alt((
            // Either: .method_name(...)
            map(preceded(dot, parse_call), ExprSuffix::FuncCall),
            // Or: (...)
            map(delimited(left_paren, args, right_paren), |res| {
                ExprSuffix::FuncCall(Call {
                    callee: None,
                    args: res,
                })
            }),
            map(preceded(dot, parse_variable), ExprSuffix::TableDot),
            map(
                delimited(left_bracket, parse_expression, right_bracket),
                ExprSuffix::TableIdx,
            ),
        )),
    )(input)
}

impl<'a> From<Expression2<'a>> for Expression<'a> {
    fn from(e: Expression2<'a>) -> Expression<'a> {
        match e {
            Expression2::Literal(l) => Expression::Literal(l),
            Expression2::Collection(l) => Expression::Collection(l),
            Expression2::PrefixExpr(p) => Expression::PrefixExpr(Box::new(p)),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    use crate::parser::literals::{Collection, Token};

    #[test]
    fn test_parse_head() {
        let string = "-hello and test";
        let (_, res) = parse_head(Span::new(string)).unwrap();

        assert_eq!(
            res,
            ExprHead {
                un_ops: vec![UnOperator::Sub],
                expr: Expression2::PrefixExpr(PrefixExpr {
                    prefix: ExprOrVarname::Varname(Token::new("hello", Span::new("hello"))),
                    suffix_chain: vec![]
                })
            }
        )
    }

    #[test]
    fn test_parse_bin_op_chain() {
        let string = "< hello() - 3";
        let (_, _) = parse_bin_op_chain(Span::new(string)).unwrap();
        // println!("{:?}", res);
    }

    #[test]
    fn test_expr_lexer6() {
        let string = "hello()";
        let (_, res) = parse_expression(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Expression::PrefixExpr(Box::new(PrefixExpr {
                prefix: ExprOrVarname::Varname(Token::new("hello", Span::new("hello"))),
                suffix_chain: vec![ExprSuffix::FuncCall(Call {
                    args: vec![],
                    callee: None
                })]
            }))
        );
    }

    #[test]
    fn test_expr_lexer7() {
        let string = "hello()";
        let (_, res) = prefixexpr(Span::new(string)).unwrap();
        assert_eq!(
            res,
            PrefixExpr {
                prefix: ExprOrVarname::Varname(Token::new("hello", Span::new("hello"))),
                suffix_chain: vec![ExprSuffix::FuncCall(Call {
                    args: vec![],
                    callee: None
                })]
            }
        );
    }

    #[test]
    fn test_expr_lexer4() {
        let string = "3";
        let (_, res) = parse_expression(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Expression::Literal(Literal::Int(Token::new(3, Span::new("3"))))
        )
    }

    #[test]
    fn test_expr_lexer3() {
        let string = "3 < x";
        let (_, res) = parse_expression(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Expression::BinaryOp(Box::new(BinaryOp {
                op: Operator::Lt,
                left: Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
                right: Expression::PrefixExpr(Box::new(PrefixExpr {
                    prefix: ExprOrVarname::Varname(Token::new("x", Span::new("x"))),
                    suffix_chain: vec![]
                }))
            }))
        )
    }

    // That test does not work because we compare actual spans and not tokens, which
    // we can#t modify.
    #[ignore]
    #[test]
    fn test_expr_lexer5() {
        let string = "(3 < x) and Nil == Nil";
        let (_, res) = parse_expression(Span::new(string)).unwrap();

        let left = Expression::PrefixExpr(Box::new(PrefixExpr {
            prefix: ExprOrVarname::Exp(Expression::BinaryOp(Box::new(BinaryOp {
                op: Operator::Lt,
                left: Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
                right: Expression::PrefixExpr(Box::new(PrefixExpr {
                    prefix: ExprOrVarname::Varname(Token::new("x", Span::new("x"))),
                    suffix_chain: vec![],
                })),
            }))),
            suffix_chain: vec![],
        }));

        let right = Expression::BinaryOp(Box::new(BinaryOp {
            op: Operator::EQ,
            left: Expression::Literal(Literal::Nil(Span::new("Nil"))),
            right: Expression::Literal(Literal::Nil(Span::new("Nil"))),
        }));

        assert_eq!(
            res,
            Expression::BinaryOp(Box::new(BinaryOp {
                left: left,
                op: Operator::And,
                right: right
            }))
        )
    }

    #[test]
    fn test_expr_bool() {
        let string = "False + True";
        let (_, res) = parse_expression(Span::new(string)).unwrap();

        assert_eq!(
            res,
            Expression::BinaryOp(Box::new(BinaryOp {
                op: Operator::Add,
                left: Expression::Literal(Literal::Boolean(Token::new(false, Span::new("False")))),
                right: Expression::Literal(Literal::Boolean(Token::new(true, Span::new("True")))),
            }))
        )
    }

    #[test]
    fn test_expr_lexer2() {
        let string = "(3 + 4)";
        let (_, res) = parse_expression(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Expression::PrefixExpr(Box::new(PrefixExpr {
                prefix: ExprOrVarname::Exp(Expression::BinaryOp(Box::new(BinaryOp {
                    op: Operator::Add,
                    left: Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
                    right: Expression::Literal(Literal::Int(Token::new(4, Span::new("4")))),
                }))),
                suffix_chain: vec![]
            }))
        )
    }

    #[test]
    fn test_expr_lexer9() {
        let string = "x + 4";
        let (_, res) = parse_expression(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Expression::BinaryOp(Box::new(BinaryOp {
                op: Operator::Add,
                left: Expression::PrefixExpr(Box::new(PrefixExpr {
                    prefix: ExprOrVarname::Varname(Token::new("x", Span::new("x"))),
                    suffix_chain: vec![]
                })),
                right: Expression::Literal(Literal::Int(Token::new(4, Span::new("4")))),
            }))
        )
    }

    #[test]
    fn test_parse_list_as_expr() {
        let string = "[1, 2, 3, 4]";
        let (_, res) = parse_expression(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Expression::Collection(Collection::Array(vec![
                Expression::Literal(Literal::Int(Token::new(1, Span::new("3")))),
                Expression::Literal(Literal::Int(Token::new(2, Span::new("3")))),
                Expression::Literal(Literal::Int(Token::new(3, Span::new("3")))),
                Expression::Literal(Literal::Int(Token::new(4, Span::new("3"))))
            ]))
        )
    }
}
