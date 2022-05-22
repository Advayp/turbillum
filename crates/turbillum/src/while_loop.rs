use crate::boolean_expr::Boolean;
use crate::expr::block::Block;
use crate::utils;
use crate::env::Env;
use crate::val::Val;

#[derive(Debug, PartialEq, Clone)]
pub struct WhileLoop {
    condition: Boolean,
    body: Block,
}

impl WhileLoop {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        // $while$[1 %< 2] []
        let s = utils::tag("$", s)?;
        let s = utils::tag("while", s)?;
        let s = utils::tag("$", s)?;

        let (s, _) = utils::extract_whitespace(s);
        let s = utils::tag("[", s)?;

        let (s, condition) = Boolean::new(s)?;

        let s = utils::tag("]", s)?;

        let (s, _) = utils::extract_whitespace(s);

        let (s, body) = Block::new(s)?;

        Ok((s, Self {
            body,
            condition,
        }))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        let mut val = match self.condition.eval(env) {
            Err(_) => false,
            Ok(Val::Bool(bool)) => bool,
            _ => true
        };

        while val {
            self.body.eval_with_parent_env(env)?;
            val = match self.condition.eval(env) {
                Err(_) => false,
                Ok(Val::Bool(bool)) => bool,
                _ => true
            };
        }

        Ok(Val::Unit)
    }
}

#[cfg(test)]
mod tests {
    use crate::boolean_expr::{Boolean, Comparison, ComparisonOp};
    use crate::{Env, Val};
    use crate::change_statement::ChangeStatement;
    use crate::expr::block::Block;
    use crate::expr::{Expr, Number, Op};
    use crate::expr::binding_usage::BindingUsage;
    use crate::statements::Stmt;
    use super::WhileLoop;

    #[test]
    fn parse_while_loop() {
        assert_eq!(WhileLoop::new("$while$[1 >% 2] []"), Ok(("", WhileLoop {
            condition: Boolean::Comparison(Comparison {
                lhs: Expr::Number(Number(1)),
                rhs: Expr::Number(Number(2)),
                op: ComparisonOp::Geq,
            }),
            body: Block {
                stmts: Vec::new()
            },
        })));
    }

    #[test]
    fn eval_while_loop() {
        assert_eq!(WhileLoop {
            condition: Boolean::Comparison(Comparison {
                lhs: Expr::Number(Number(1)),
                rhs: Expr::Number(Number(2)),
                op: ComparisonOp::Geq,
            }),
            body: Block {
                stmts: Vec::new()
            },
        }.eval(&mut Env::default()), Ok(Val::Unit));
    }

    #[test]
    fn eval_while_loop_body() {
        let mut env = Env::default();

        env.store_binding("a".to_string(), Val::Number(0));
        env.store_binding("b".to_string(), Val::Number(0));

       WhileLoop {
            condition: Boolean::Comparison(Comparison {
                lhs: Expr::BindingUsage(BindingUsage {
                    name: "a".to_string()
                }),
                rhs: Expr::Number(Number(4)),
                op: ComparisonOp::Lt,
            }),
            body: Block {
                stmts: vec![Stmt::ChangeStatment(ChangeStatement {
                    name: "a".to_string(),
                    new_expr: Expr::Operation {
                        lhs: Box::new(Expr::BindingUsage(BindingUsage {
                            name: "a".to_string()
                        })),
                        rhs: Box::new(Expr::Number(Number(1))),
                        op: Op::Add
                    }
                }), Stmt::ChangeStatment(ChangeStatement {
                    name: "b".to_string(),
                    new_expr: Expr::Operation {
                        lhs: Box::new(Expr::BindingUsage(BindingUsage {
                            name: "b".to_string()
                        })),
                        rhs: Box::new(Expr::Number(Number(1))),
                        op: Op::Add
                    }
                })]
            },
        }.eval(&mut env).unwrap();

        assert_eq!(env.get_binding("b"), Ok(Val::Number(4)));
    }
}