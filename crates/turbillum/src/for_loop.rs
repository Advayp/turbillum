use crate::{env::Env, expr::block::Block, utils, val::Val};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ForLoop {
    repetitions: i32,
    body: Block,
}

impl ForLoop {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        // $for$[4x] {}
        let s = utils::tag("$for$", s)?;

        let s = utils::tag("[", s)?;
        let (s, repetitions) = utils::extract_digits(s)?;
        let s = utils::tag("x", s)?;
        let s = utils::tag("]", s)?;

        let (s, _) = utils::extract_whitespace(s);
        let (s, body) = Block::new(s)?;

        Ok((
            "",
            Self {
                repetitions: repetitions.parse().unwrap(),
                body,
            },
        ))
    }
    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        for _ in 0..(self.repetitions) {
            self.body.eval_with_parent_env(env);
        }
        Ok(Val::Unit)
    }
}

mod tests {

    use std::vec;

    use crate::binding_def::BindingDef;
    use crate::change_statement::ChangeStatement;
    use crate::expr::binding_usage::BindingUsage;
    use crate::expr::{Expr, Number, Op};
    use crate::statements::Stmt;

    use super::*;

    #[test]
    fn for_loop_parse_works() {
        assert_eq!(
            ForLoop::new(
                "$for$[4x] [
            def a = 0.
        ]"
            ),
            Ok((
                "",
                ForLoop {
                    repetitions: 4,
                    body: Block {
                        stmts: vec![Stmt::BindingDef(BindingDef {
                            name: "a".to_string(),
                            value: Expr::Number(Number(0))
                        })]
                    }
                }
            ))
        )
    }

    #[test]
    fn for_loop_eval_works() {
        let mut env = Env::default();
        env.store_binding("a".to_string(), Val::Number(4));
        let value = ForLoop {
            repetitions: 3,
            body: Block {
                stmts: vec![Stmt::ChangeStatment(ChangeStatement {
                    name: "a".to_string(),
                    new_expr: Expr::Operation {
                        lhs: Box::new(Expr::BindingUsage(BindingUsage {
                            name: "a".to_string(),
                        })),
                        rhs: Box::new(Expr::Number(Number(1))),
                        op: Op::Add,
                    },
                })],
            },
        }
        .eval(&mut env)
        .unwrap();

        assert_eq!(value, Val::Unit);
    }

    #[test]
    fn for_loop_eval_evaluates_block() {
        let mut env = Env::default();
        env.store_binding("a".to_string(), Val::Number(0));
        ForLoop {
            repetitions: 3,
            body: Block {
                stmts: vec![Stmt::ChangeStatment(ChangeStatement {
                    name: "a".to_string(),
                    new_expr: Expr::Operation {
                        lhs: Box::new(Expr::BindingUsage(BindingUsage {
                            name: "a".to_string(),
                        })),
                        rhs: Box::new(Expr::Number(Number(1))),
                        op: Op::Add,
                    },
                })],
            },
        }
        .eval(&mut env)
        .unwrap();

        assert_eq!(env.get_binding("a"), Ok(Val::Number(3)));
    }
}
