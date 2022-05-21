use crate::env::Env;
use crate::{utils, val::Val};

use self::binding_usage::BindingUsage;
use self::block::Block;
use self::function_call::FuncCall;

pub(crate) mod binding_usage;
pub(crate) mod block;
pub(crate) mod function_call;
#[derive(Debug, PartialEq, Clone)]
pub struct Number(pub i32);

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expr {
    Number(Number),
    Operation {
        lhs: Box<Self>,
        rhs: Box<Self>,
        op: Op,
    },
    BindingUsage(BindingUsage),
    Block(Block),
    FuncCall(FuncCall),
}

impl Number {
    fn new(s: &str) -> Result<(&str, Self), String> {
        let (s, number) = utils::extract_digits(s)?;

        Ok((s, Self(number.parse().unwrap())))
    }
}

impl Op {
    fn new(s: &str) -> Result<(&str, Self), String> {
        utils::tag("+", s)
            .map(|s| (s, Self::Add))
            .or_else(|_| utils::tag("-", s).map(|s| (s, Self::Subtract)))
            .or_else(|_| utils::tag("*", s).map(|s| (s, Self::Multiply)))
            .or_else(|_| utils::tag("/", s).map(|s| (s, Self::Divide)))
    }
}

impl Expr {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        Self::new_operation(s).or_else(|_| Self::new_non_operation(s))
    }

    pub(crate) fn new_non_operation(s: &str) -> Result<(&str, Self), String> {
        Self::new_number(s)
            .or_else(|_| FuncCall::new(s).map(|(s, func_call)| (s, Self::FuncCall(func_call))))
            .or_else(|_| {
                BindingUsage::new(s)
                    .map(|(s, binding_usage)| (s, Self::BindingUsage(binding_usage)))
            })
            .or_else(|_| Block::new(s).map(|(s, block)| (s, Self::Block(block))))
    }

    fn new_operation(s: &str) -> Result<(&str, Self), String> {
        let (s, lhs) = Self::new_non_operation(s)?;
        let (s, _) = utils::extract_whitespace(s);

        let (s, op) = Op::new(s)?;
        let (s, _) = utils::extract_whitespace(s);

        let (s, rhs) = Self::new_non_operation(s)?;

        Ok((
            s,
            Self::Operation {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            },
        ))
    }

    fn new_number(s: &str) -> Result<(&str, Self), String> {
        Number::new(s).map(|(s, number)| (s, Self::Number(number)))
    }

    pub(crate) fn evaluate(&self, env: &mut Env) -> Result<Val, String> {
        match self {
            Self::Number(Number(n)) => Ok(Val::Number(*n)),
            Self::Operation { lhs, rhs, op } => {
                let lhs = lhs.evaluate(env)?;
                let rhs = rhs.evaluate(env)?;

                let (lhs, rhs) = match (lhs, rhs) {
                    (Val::Number(lhs), Val::Number(rhs)) => (lhs, rhs),
                    _ => return Err("cannot evaluate operation whose left-hand side and right-hand side are not both numbers".to_string()),
                };

                let result = match op {
                    Op::Add => lhs + rhs,
                    Op::Subtract => lhs - rhs,
                    Op::Multiply => lhs * rhs,
                    Op::Divide => lhs / rhs,
                };

                Ok(Val::Number(result))
            }
            Self::BindingUsage(binding_usage) => binding_usage.eval(env),
            Self::Block(block) => block.eval(&mut env.create_child()),
            Self::FuncCall(func_call) => func_call.eval(env),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binding_def::BindingDef;
    use crate::statements::Stmt;

    #[test]
    fn it_works() {
        assert_eq!(Number::new("123"), Ok(("", Number(123))));
    }

    #[test]
    fn parse_add_op() {
        assert_eq!(Op::new("+"), Ok(("", Op::Add)))
    }

    #[test]
    fn parse_sub_op() {
        assert_eq!(Op::new("-"), Ok(("", Op::Subtract)))
    }

    #[test]
    fn parse_mul_op() {
        assert_eq!(Op::new("*"), Ok(("", Op::Multiply)))
    }

    #[test]
    fn parse_div_op() {
        assert_eq!(Op::new("/"), Ok(("", Op::Divide)))
    }

    #[test]
    fn parse_expr_without_whitespace() {
        assert_eq!(
            Expr::new("2+2"),
            Ok((
                "",
                Expr::Operation {
                    lhs: Box::new(Expr::Number(Number(2))),
                    rhs: Box::new(Expr::Number(Number(2))),
                    op: Op::Add
                }
            ))
        )
    }

    #[test]
    fn parse_expr_with_whitespace() {
        assert_eq!(
            Expr::new("2 + 2"),
            Ok((
                "",
                Expr::Operation {
                    lhs: Box::new(Expr::Number(Number(2))),
                    rhs: Box::new(Expr::Number(Number(2))),
                    op: Op::Add,
                }
            )),
        );
    }

    #[test]
    fn expr_evaluate_gives_correct_result() {
        assert_eq!(
            Expr::Operation {
                lhs: Box::new(Expr::Number(Number(2))),
                rhs: Box::new(Expr::Number(Number(2))),
                op: Op::Add
            }
            .evaluate(&mut Env::default()),
            Ok(Val::Number(4))
        );
    }

    #[test]
    fn parse_binding_usage() {
        assert_eq!(
            Expr::new("bar"),
            Ok((
                "",
                Expr::BindingUsage(BindingUsage {
                    name: "bar".to_string(),
                }),
            )),
        );
    }

    #[test]
    fn parse_block() {
        assert_eq!(
            Expr::new("[ 200 ]"),
            Ok((
                "",
                Expr::Block(Block {
                    stmts: vec![Stmt::Expr(Expr::Number(Number(200)))],
                }),
            )),
        );
    }

    #[test]
    fn parse_block_with_multiple_stmts() {
        assert_eq!(
            Block::new(
                "[
    def a = 10.
    def b = a.
    b
]",
            ),
            Ok((
                "",
                Block {
                    stmts: vec![
                        Stmt::BindingDef(BindingDef {
                            name: "a".to_string(),
                            value: Expr::Number(Number(10)),
                        }),
                        Stmt::BindingDef(BindingDef {
                            name: "b".to_string(),
                            value: Expr::BindingUsage(BindingUsage {
                                name: "a".to_string(),
                            }),
                        }),
                        Stmt::Expr(Expr::BindingUsage(BindingUsage {
                            name: "b".to_string(),
                        })),
                    ],
                },
            )),
        );
    }

    #[test]
    fn parse_func_call() {
        assert_eq!(
            Expr::new("add@ 1."),
            Ok((
                "",
                Expr::FuncCall(FuncCall {
                    callee: "add".to_string(),
                    params: vec![Expr::Number(Number(1))],
                }),
            )),
        );
    }

    #[test]
    fn eval_func_call() {
        let mut env = Env::default();

        env.store_func(
            "add".to_string(),
            vec!["x".to_string(), "y".to_string()],
            Stmt::Expr(Expr::Operation {
                lhs: Box::new(Expr::BindingUsage(BindingUsage {
                    name: "x".to_string(),
                })),
                rhs: Box::new(Expr::BindingUsage(BindingUsage {
                    name: "y".to_string(),
                })),
                op: Op::Add,
            }),
        );

        assert_eq!(
            Expr::FuncCall(FuncCall {
                callee: "add".to_string(),
                params: vec![Expr::Number(Number(2)), Expr::Number(Number(2))],
            })
            .evaluate(&mut env),
            Ok(Val::Number(4)),
        );
    }
}
