use crate::{boolean_expr::Boolean, expr::block::Block, env::Env, utils, binding_def::BindingDef, change_statement::ChangeStatement, Val};

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ForCondition {
    pub(crate) boolean: Boolean,
    pub(crate) body: Block,
    pub(crate) binding_def: BindingDef,
    pub(crate) apply_per_iteration: ChangeStatement
}

impl ForCondition {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        let s = utils::tag("$", s)?;
        let s = utils::tag("forc", s)?;
        let s = utils::tag("$", s)?;
        let s = utils::tag("[", s)?;

        let (s, binding_def) = BindingDef::new(s)?;
        let (s, _) = utils::extract_whitespace(s);

        let (s, boolean) = Boolean::new(s)?;
        dbg!(s);
        let s = utils::tag(".", s)?;
        let (s, _) = utils::extract_whitespace(s);

        let (s, apply_per_iteration) = ChangeStatement::new(s)?;
        let (s, _) = utils::extract_whitespace(s);

        let s = utils::tag("]", s)?;

        let (s, _) = utils::extract_whitespace1(s)?;

        let (s, body) = Block::new(s)?;

        Ok((s, Self {
            binding_def,
            boolean,
            body,
            apply_per_iteration   
        }))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        self.binding_def.eval(env)?;

        let mut condition = match self.boolean.eval(env)? {
            Val::Bool(bool) => bool,
            _ => false
        };
        while condition {
            self.body.eval_with_parent_env(env)?;
            self.apply_per_iteration.eval(env)?;
            condition = match self.boolean.eval(env)? {
                Val::Bool(bool) => bool,
                _ => false
            };
        }

        Ok(Val::Unit)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{boolean_expr::Boolean, change_statement::ChangeStatement};
    use crate::boolean_expr::{Comparison, ComparisonOp};
    use crate::expr::binding_usage::BindingUsage;
    use crate::expr::{Expr, Number, Op};
    use crate::statements::Stmt;

    #[test]
    fn parse_for_condition_correctly() {
        assert_eq!(ForCondition::new("$forc$[def a = 0. a %< 4. change a -> a + 2.] [change a -> a - 1.]"), Ok(("", ForCondition {
            boolean: Boolean::Comparison(Comparison {
                lhs: Expr::BindingUsage(BindingUsage{name: "a".to_string()}),
                rhs: Expr::Number(Number(4)),
                op: ComparisonOp::Leq
            }),
            body: Block {
                stmts: vec![Stmt::ChangeStatment(ChangeStatement {
                    name: "a".to_string(),
                    new_expr: Expr::Operation {
                        lhs: Box::new(Expr::BindingUsage(BindingUsage {name: "a".to_string()})),
                        rhs: Box::new(Expr::Number(Number(1))),
                        op: Op::Subtract
                    }
                })]
            },
            binding_def: BindingDef {
                name: "a".to_string(),
                value: Expr::Number(Number(0))
            },
            apply_per_iteration: ChangeStatement {
                name: "a".to_string(),
                new_expr: Expr::Operation {
                    lhs: Box::new(Expr::BindingUsage(BindingUsage {
                        name: "a".to_string()
                    })),
                    rhs: Box::new(Expr::Number(Number(2))),
                    op: Op::Add
                }
            }
        })));
    }

    #[test]
    fn eval_for_condition_properly() {

        let mut env = Env::default();

        env.store_binding("b".to_string(), Val::Number(0));

        ForCondition {
            boolean: Boolean::Comparison(Comparison {
                lhs: Expr::BindingUsage(BindingUsage{name: "a".to_string()}),
                rhs: Expr::Number(Number(4)),
                op: ComparisonOp::Lt
            }),
            body: Block {
                stmts: vec![Stmt::ChangeStatment(ChangeStatement {
                    name: "b".to_string(),
                    new_expr: Expr::Operation {
                        lhs: Box::new(Expr::BindingUsage(BindingUsage {name: "b".to_string()})),
                        rhs: Box::new(Expr::Number(Number(1))),
                        op: Op::Add
                    }
                })]
            },
            binding_def: BindingDef {
                name: "a".to_string(),
                value: Expr::Number(Number(0))
            },
            apply_per_iteration: ChangeStatement {
                name: "a".to_string(),
                new_expr: Expr::Operation {
                    lhs: Box::new(Expr::BindingUsage(BindingUsage {
                        name: "a".to_string()
                    })),
                    rhs: Box::new(Expr::Number(Number(1))),
                    op: Op::Add
                }
            }
        }.eval(&mut env).unwrap();

        assert_eq!(env.get_binding("b"), Ok(Val::Number(4)));
    }
}
