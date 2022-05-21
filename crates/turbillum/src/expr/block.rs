use crate::env::Env;
use crate::val::Val;
use crate::{statements::Stmt, utils};

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Block {
    pub(crate) stmts: Vec<Stmt>,
}

impl Block {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        let s = utils::tag("[", s)?;
        let (s, _) = utils::extract_whitespace(s);

        let (s, stmts) = utils::sequence(Stmt::new, s)?;

        let (s, _) = utils::extract_whitespace(s);
        let s = utils::tag("]", s)?;

        Ok((s, Block { stmts }))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        if self.stmts.is_empty() {
            return Ok(Val::Unit);
        }

        let mut env = env.create_child();

        let stmts_except_last = &self.stmts[..self.stmts.len() - 1];
        for stmt in stmts_except_last {
            stmt.eval(&mut env)?;
        }

        self.stmts.last().unwrap().eval(&mut env)
    }

    pub(crate) fn eval_with_parent_env(&self, env: &mut Env) -> Result<Val, String> {
        if self.stmts.is_empty() {
            return Ok(Val::Unit);
        }

        let stmts_except_last = &self.stmts[..self.stmts.len() - 1];
        for stmt in stmts_except_last {
            stmt.eval(env)?;
        }

        self.stmts.last().unwrap().eval(env)
    }
}

#[cfg(test)]
mod tests {

    use super::super::{BindingUsage, Expr, Number, Op};
    use super::*;
    use crate::binding_def::BindingDef;
    use crate::change_statement::ChangeStatement;
    use crate::env::Env;
    use crate::val::Val;

    #[test]
    fn parse_empty_block() {
        assert_eq!(Block::new("[]"), Ok(("", Block { stmts: Vec::new() })));
    }

    #[test]
    fn parse_empty_block_with_whitespace() {
        assert_eq!(Block::new("[   ]"), Ok(("", Block { stmts: Vec::new() })));
    }

    #[test]
    fn parse_block_with_one_stmt() {
        assert_eq!(
            Block::new("[ 5 ]"),
            Ok((
                "",
                Block {
                    stmts: vec![Stmt::Expr(Expr::Number(Number(5)))],
                },
            )),
        );
    }

    #[test]
    fn eval_empty_block() {
        assert_eq!(
            Block { stmts: Vec::new() }.eval(&mut Env::default()),
            Ok(Val::Unit),
        );
    }

    #[test]
    fn eval_block() {
        assert_eq!(
            Expr::Block(Block {
                stmts: vec![Stmt::Expr(Expr::Number(Number(10)))],
            })
            .evaluate(&mut Env::default()),
            Ok(Val::Number(10)),
        );
    }

    #[test]
    fn eval_block_with_binding_def_and_usage() {
        assert_eq!(
            Block {
                stmts: vec![
                    Stmt::BindingDef(BindingDef {
                        name: "one".to_string(),
                        value: Expr::Number(Number(1)),
                    }),
                    Stmt::Expr(Expr::BindingUsage(BindingUsage {
                        name: "one".to_string(),
                    })),
                ],
            }
            .eval(&mut Env::default()),
            Ok(Val::Number(1)),
        );
    }

    #[test]
    fn eval_block_with_multiple_binding_defs() {
        assert_eq!(
            Block {
                stmts: vec![
                    Stmt::BindingDef(BindingDef {
                        name: "foo".to_string(),
                        value: Expr::Number(Number(5)),
                    }),
                    Stmt::BindingDef(BindingDef {
                        name: "bar".to_string(),
                        value: Expr::Number(Number(4)),
                    }),
                    Stmt::BindingDef(BindingDef {
                        name: "baz".to_string(),
                        value: Expr::Number(Number(3)),
                    }),
                ],
            }
            .eval(&mut Env::default()),
            Ok(Val::Unit),
        );
    }

    #[test]
    fn eval_block_with_bind_def_and_change_stmt() {
        assert_eq!(
            Block {
                stmts: vec![
                    Stmt::BindingDef(BindingDef {
                        name: "one".to_string(),
                        value: Expr::Number(Number(1))
                    }),
                    Stmt::ChangeStatment(ChangeStatement {
                        name: "one".to_string(),
                        new_expr: Expr::Operation {
                            lhs: Box::new(Expr::BindingUsage(BindingUsage {
                                name: "one".to_string()
                            })),
                            rhs: Box::new(Expr::Number(Number(1))),
                            op: Op::Add
                        }
                    }),
                    Stmt::Expr(Expr::BindingUsage(BindingUsage {
                        name: "one".to_string(),
                    }),)
                ]
            }
            .eval(&mut Env::default()),
            Ok(Val::Number(2))
        )
    }
}
