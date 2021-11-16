use super::function::{parse_function, Function};

use crate::parser::{
    literals::parse_variable_raw,
    literals::sp,
    statement::opt_line_ending,
    tokens::{class, end},
    Res, Span,
};

use nom::{
    error::context,
    multi::many0,
    sequence::{preceded, terminated, tuple},
};

#[derive(Debug, PartialEq, Clone)]
pub struct Class<'a> {
    pub name: String,
    pub methods: Vec<Function<'a>>,
}

pub fn parse_class(input: Span) -> Res<Class> {
    context(
        "Class",
        preceded(
            sp,
            tuple((
                parse_class_name,
                terminated(parse_methods, preceded(sp, end)),
            )),
        ),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            Class {
                name: String::from(*res.0.fragment()),
                methods: res.1,
            },
        )
    })
}

fn parse_class_name(input: Span) -> Res<Span> {
    context(
        "ClassName",
        preceded(preceded(sp, class), preceded(sp, parse_variable_raw)),
    )(input)
}

fn parse_methods(input: Span) -> Res<Vec<Function>> {
    context(
        "Methods",
        terminated(
            preceded(sp, preceded(sp, many0(parse_function))),
            opt_line_ending,
        ),
    )(input)
}

#[cfg(test)]
mod tests {

    use super::*;

    use crate::literals::Token;
    use crate::parser::{
        expression::Expression,
        literals::Literal,
        statement::declaration::assignment::LAssignment,
        statement::declaration::function::Function,
        statement::{Block, ReturnStmt, Statement},
    };

    #[test]
    fn test_prase_class_name() {
        let string = " class Hello ";
        let (_, res) = parse_class_name(Span::new(string)).unwrap();
        assert_eq!(res.fragment(), &"Hello")
    }

    #[test]
    fn test_parse_class_with_method() {
        let string = "class Hello\n\tfun foo()\n\t\tlet some = 1\n\tend\nend";
        let (_, res) = parse_class(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Class {
                name: String::from("Hello"),
                methods: vec![Function {
                    name: "foo",
                    parameters: vec![],
                    block: Block {
                        return_stmt: None,
                        statements: vec![Statement::LAssignment(LAssignment {
                            variable: Token::new("some", Span::new("some")),
                            expression: Expression::Literal(Literal::Int(Token::new(
                                1,
                                Span::new("1")
                            )))
                        })]
                    }
                }]
            }
        );
    }

    #[test]
    fn test_parse_class_with_method1() {
        let string = "\n\nclass Hello\n\n\tfun method1()\n\t\treturn 'Hello World'\n\tend\n\nend\n";
        let (_, res) = parse_class(Span::new(string)).unwrap();
        assert_eq!(
            res,
            Class {
                name: String::from("Hello"),
                methods: vec![Function {
                    name: "method1",
                    parameters: vec![],
                    block: Block {
                        return_stmt: Some(ReturnStmt {
                            values: vec![Expression::Literal(Literal::Str(Token::new(
                                String::from("Hello World"),
                                Span::new("Hello World")
                            )))],
                        }),
                        statements: vec![]
                    }
                }]
            }
        );
    }

    #[test]
    fn test_empty_class() {
        let string = "class Test\n\n   end\n";
        let (_, res) = parse_class(Span::new(string)).unwrap();

        assert_eq!(
            res,
            Class {
                name: String::from("Test"),
                methods: vec![]
            }
        )
    }

    // #[test]
    // fn test_class_1() {
    //     let string = "class Person\r\n\r\n    fun init(self, height, weight)\r\n       self.height = height\r\n       self.weight = weight \r\n    end\r\n\r\n    fun get_bmi(self)\r\n       let w_res = self.height * self.height\r\n       return self.weight / w_res\r\n    end\r\nend\r\n\r\n";
    //     let (_, _) = parse_class(Span::new(string)).unwrap();
    //     // println!("{:?}", res);
    // }
}
