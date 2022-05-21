use crate::{
    binding_def::{BindingDef},
    change_statement::ChangeStatement,
    env::Env,
    expr::Expr,
    function::FuncDef,
    val::Val,
    for_loop::ForLoop,
    if_statement::IfStmt
};

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum Stmt {
    BindingDef(BindingDef),
    Expr(Expr),
    FuncDef(FuncDef),
    ChangeStatment(ChangeStatement),
    ForLoop(ForLoop),
    IfStmt(IfStmt)
}

impl Stmt {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        BindingDef::new(s)
            .map(|(s, binding_def)| (s, Self::BindingDef(binding_def)))
            .or_else(|_| FuncDef::new(s).map(|(s, func_def)| (s, Self::FuncDef(func_def))))
            .or_else(|_| ChangeStatement::new(s).map(|(s, stmt)| (s, Self::ChangeStatment(stmt))))
            .or_else(|_| ForLoop::new(s).map(|(s, lp)| (s, Self::ForLoop(lp))))
            .or_else(|_| IfStmt::new(s).map(|(s, stmt)| (s, Self::IfStmt(stmt))))
            .or_else(|_| Expr::new(s).map(|(s, expr)| (s, Self::Expr(expr))))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        match self {
            Self::BindingDef(def) => {
                def.eval(env)?;
                Ok(Val::Unit)
            }
            Self::Expr(expr) => expr.evaluate(env),
            Self::FuncDef(def) => {
                def.eval(env)?;
                Ok(Val::Unit)
            }
            Self::ChangeStatment(stmt) => stmt.eval(env),
            Self::ForLoop(lp) => lp.eval(env),
            Self::IfStmt(stmt) => stmt.eval(env)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::{
        binding_def::BindingDef,
        change_statement::ChangeStatement,
        expr::binding_usage::BindingUsage,
        expr::block::Block,
        expr::Number,
        expr::{Expr, Op},
    };

    #[test]
    pub fn statement_can_be_binding_def() {
        assert_eq!(
            Stmt::new("def a = 2."),
            Ok((
                "",
                Stmt::BindingDef(BindingDef {
                    name: "a".to_string(),
                    value: Expr::Number(Number(2))
                })
            ))
        );
    }

    #[test]
    pub fn statement_can_be_expr() {
        assert_eq!(
            Stmt::new("1 + 1"),
            Ok((
                "",
                Stmt::Expr(Expr::Operation {
                    lhs: Box::new(Expr::Number(Number(1))),
                    rhs: Box::new(Expr::Number(Number(1))),
                    op: Op::Add
                })
            ))
        );
    }

    #[test]
    fn parse_func_def() {
        assert_eq!(
            Stmt::new("funky asdf|x| => x"),
            Ok((
                "",
                Stmt::FuncDef(FuncDef {
                    name: "asdf".to_string(),
                    params: vec!["x".to_string()],
                    body: Box::new(Stmt::Expr(Expr::BindingUsage(BindingUsage {
                        name: "x".to_string(),
                    }))),
                }),
            )),
        );
    }

    #[test]
    fn parse_func_def_with_no_params_and_empty_body() {
        assert_eq!(
            FuncDef::new("funky asdf|| => []"),
            Ok((
                "",
                FuncDef {
                    name: "asdf".to_string(),
                    params: Vec::new(),
                    body: Box::new(Stmt::Expr(Expr::Block(Block { stmts: Vec::new() }))),
                },
            )),
        );
    }

    #[test]
    fn parse_func_def_with_one_param_and_empty_body() {
        assert_eq!(
            FuncDef::new("funky asdf|x| => []"),
            Ok((
                "",
                FuncDef {
                    name: "asdf".to_string(),
                    params: vec!["x".to_string()],
                    body: Box::new(Stmt::Expr(Expr::Block(Block { stmts: Vec::new() })))
                }
            ))
        )
    }

    #[test]
    fn parse_func_def_with_multiple_params() {
        assert_eq!(
            FuncDef::new("funky asdf|x y| => [x + y]"),
            Ok((
                "",
                FuncDef {
                    name: "asdf".to_string(),
                    params: vec!["x".to_string(), "y".to_string()],
                    body: Box::new(Stmt::Expr(Expr::Block(Block {
                        stmts: vec![Stmt::Expr(Expr::Operation {
                            lhs: Box::new(Expr::BindingUsage(BindingUsage {
                                name: "x".to_string()
                            })),
                            rhs: Box::new(Expr::BindingUsage(BindingUsage {
                                name: "y".to_string()
                            })),
                            op: Op::Add
                        })]
                    })))
                }
            ))
        )
    }
    #[test]
    fn eval_func_def() {
        assert_eq!(
            Stmt::FuncDef(FuncDef {
                name: "always_return_one".to_string(),
                params: Vec::new(),
                body: Box::new(Stmt::Expr(Expr::Number(Number(1)))),
            })
            .eval(&mut Env::default()),
            Ok(Val::Unit),
        );
    }

    #[test]
    fn stmt_can_be_change_statement() {
        assert_eq!(
            Stmt::new("change a -> 1 + 1."),
            Ok((
                "",
                Stmt::ChangeStatment(ChangeStatement {
                    name: "a".to_string(),
                    new_expr: Expr::Operation {
                        lhs: Box::new(Expr::Number(Number(1))),
                        rhs: Box::new(Expr::Number(Number(1))),
                        op: Op::Add
                    }
                })
            ))
        )
    }
}
