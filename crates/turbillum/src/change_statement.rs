use crate::expr::Expr;
use crate::val::Val;
use crate::{utils, Env};

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ChangeStatement {
    pub(crate) name: String,
    pub(crate) new_expr: Expr,
}

impl ChangeStatement {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        let s = utils::tag("change", s)?;
        let (s, _) = utils::extract_whitespace1(s)?;

        let (s, name) = utils::extract_ident(s)?;
        let (s, _) = utils::extract_whitespace1(s)?;

        let s = utils::tag("->", s)?;
        let (s, _) = utils::extract_whitespace1(s)?;

        let (s, new_expr) = Expr::new(s)?;
        let s = utils::tag(".", s)?;

        Ok((
            s,
            Self {
                name: name.to_string(),
                new_expr,
            },
        ))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        let value = self.new_expr.evaluate(&mut env.create_child())?;
        env.adjust_value(self.name.clone(), value.clone())?;
        Ok(value)
    }
}

mod tests {
    use super::*;
    use crate::expr::{binding_usage::BindingUsage, Expr, Number, Op};

    #[test]
    fn parse_change_stmt_correctly() {
        assert_eq!(
            ChangeStatement::new("change a -> 1 + 1."),
            Ok((
                "",
                ChangeStatement {
                    name: "a".to_string(),
                    new_expr: Expr::Operation {
                        lhs: Box::new(Expr::Number(Number(1))),
                        rhs: Box::new(Expr::Number(Number(1))),
                        op: Op::Add
                    }
                }
            ))
        );
    }

    #[test]
    fn eval_change_stmt() {
        let mut env = Env::default();
        env.store_binding("a".to_string(), Val::Number(1));

        assert_eq!(
            ChangeStatement {
                name: "a".to_string(),
                new_expr: Expr::Operation {
                    lhs: Box::new(Expr::BindingUsage(BindingUsage {
                        name: "a".to_string()
                    })),
                    rhs: Box::new(Expr::Number(Number(1))),
                    op: Op::Add
                }
            }
            .eval(&mut env),
            Ok(Val::Number(2))
        );
    }

    #[test]
    fn eval_change_stmt_var() {
        let mut env = Env::default();
        env.store_binding("a".to_string(), Val::Number(1));

        let val = ChangeStatement {
            name: "a".to_string(),
            new_expr: Expr::Operation {
                lhs: Box::new(Expr::BindingUsage(BindingUsage {
                    name: "a".to_string(),
                })),
                rhs: Box::new(Expr::Number(Number(1))),
                op: Op::Add,
            },
        }
        .eval(&mut env)
        .unwrap();

        assert_eq!(env.get_binding("a"), Ok(Val::Number(2)));
    }
}
