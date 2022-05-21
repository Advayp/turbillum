use crate::boolean_expr::Boolean;
use crate::expr::block::Block;
use crate::{Env, utils, Val};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct IfStmt {
    condition: Boolean,
    body: Block,
    alternate_ifs: Vec<BooleanBlockPair>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BooleanBlockPair {
    boolean: Boolean,
    block: Block,
}

impl IfStmt {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        let (s, BooleanBlockPair { boolean: condition, block: body }) = Self::extract_if_variant("if", s)?;

        let (s, _) = utils::extract_whitespace(s);

        let (s, mut seq) = utils::sequence(Self::extract_elif, s)?;

        let (s, _) = utils::extract_whitespace(s);

        let s = Self::extract_else_if_possible(s, &mut seq)?;


        Ok((s, Self {
            condition,
            body,
            alternate_ifs: seq,
        }))
    }

    fn extract_if_variant<'a, 'b>(prefix: &'b str, s: &'a str) -> Result<(&'a str, BooleanBlockPair), String> {
        let s = utils::tag("$", s)?;
        let s = utils::tag(prefix, s)?;
        let s = utils::tag("$", s)?;

        let (s, _) = utils::extract_whitespace(s);

        let s = utils::tag("[", s)?;
        let (s, boolean) = Boolean::new(s)?;
        let s = utils::tag(".", s)?;
        let s = utils::tag("]", s)?;

        let (s, _) = utils::extract_whitespace(s);
        let (s, block) = Block::new(s)?;

        Ok((s, BooleanBlockPair { boolean, block }))
    }

    fn extract_else_if_possible<'a, 'b>(s: &'a str, seq: &'b mut Vec<BooleanBlockPair>) -> Result<&'a str, String> {

        let result = Self::extract_if_variant("othelse", s);

        match result {
            Err(_) => Ok(s),
            Ok((s, pair)) => {
                seq.push(pair);
                Ok(s)
            }
        }

    }

    pub(crate) fn extract_elif(s: &str) -> Result<(&str, BooleanBlockPair), String> {
        Self::extract_if_variant("othif", s)
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        let condition = match self.condition.eval(env)? {
            Val::Bool(bool) => bool,
            _ => false
        };

        if condition {
            self.body.eval_with_parent_env(env)?;
        } else {
            for BooleanBlockPair { boolean, block } in &self.alternate_ifs {
                let condition = match boolean.eval(env)? {
                    Val::Bool(bool) => bool,
                    _ => false
                };

                if condition {
                    block.eval_with_parent_env(env);
                    break;
                }
            }
        }

        Ok(Val::Unit)
    }
}

#[cfg(test)]
mod tests {
    use crate::boolean_expr::{Boolean, Comparison, ComparisonOp};
    use crate::{Env, Val};
    use crate::change_statement::ChangeStatement;
    use crate::expr::binding_usage::BindingUsage;
    use crate::expr::{Expr, Number};
    use crate::expr::block::Block;
    use crate::if_statement::{BooleanBlockPair, IfStmt};
    use crate::statements::Stmt;

    #[test]
    fn parse_if_stmt() {
        assert_eq!(IfStmt::new("$if$[a %< 0.] []"), Ok(("", IfStmt {
            condition: Boolean::Comparison(Comparison {
                lhs: Expr::BindingUsage(BindingUsage { name: "a".to_string() }),
                rhs: Expr::Number(Number(0)),
                op: ComparisonOp::Leq,
            }),
            body: Block {
                stmts: Vec::new()
            },
            alternate_ifs: Vec::new(),
        })));
    }

    #[test]
    fn eval_if_stmt_works() {
        let mut env = Env::default();

        env.store_binding("a".to_string(), Val::Number(4));
        IfStmt {
            condition: Boolean::Comparison(Comparison {
                lhs: Expr::Number(Number(-1)),
                rhs: Expr::Number(Number(0)),
                op: ComparisonOp::Leq,
            }),
            body: Block {
                stmts: vec![Stmt::ChangeStatment(ChangeStatement {
                    name: "a".to_string(),
                    new_expr: Expr::Number(Number(6)),
                })]
            },
            alternate_ifs: Vec::new(),
        }.eval(&mut env).unwrap();

        assert_eq!(env.get_binding("a"), Ok(Val::Number(6)));
    }

    #[test]
    fn parse_elif_stmts() {
        assert_eq!(IfStmt::new("$if$[1 >% 2.] [ ] $othif$[1 %< 2.] []"), Ok(("", IfStmt {
            body: Block {
                stmts: Vec::new()
            }
            ,
            condition: Boolean::Comparison(Comparison {
                lhs: Expr::Number(Number(1)),
                op: ComparisonOp::Geq,
                rhs: Expr::Number(Number(2)),
            }),
            alternate_ifs: vec![BooleanBlockPair {
                boolean: Boolean::Comparison(Comparison {
                    lhs: Expr::Number(Number(1)),
                    op: ComparisonOp::Leq,
                    rhs: Expr::Number(Number(2)),
                }),

                block: Block {
                    stmts: Vec::new()
                },
            }],
        })))
    }


    #[test]
    fn eval_elif_stmts() {
        let mut env = Env::default();

        env.store_binding("a".to_string(), Val::Number(0));

        IfStmt {
            body: Block {
                stmts: Vec::new()
            }
            ,
            condition: Boolean::Comparison(Comparison {
                lhs: Expr::Number(Number(1)),
                op: ComparisonOp::Geq,
                rhs: Expr::Number(Number(2)),
            }),
            alternate_ifs: vec![BooleanBlockPair {
                boolean: Boolean::Comparison(Comparison {
                    lhs: Expr::Number(Number(1)),
                    op: ComparisonOp::Leq,
                    rhs: Expr::Number(Number(2)),
                }),

                block: Block {
                    stmts: vec![Stmt::ChangeStatment(ChangeStatement {
                        name: "a".to_string(),
                        new_expr: Expr::Number(Number(2)),
                    })]
                },
            }],
        }.eval(&mut env).unwrap();

        assert_eq!(env.get_binding("a"), Ok(Val::Number(2)));
    }

    #[test]
    fn parse_else_stmts() {
        assert_eq!(IfStmt::new("$if$[1 >% 2.] [ ] $othif$[1 >% 3.] [] $othelse$[1 %< 2.] []"), Ok(("", IfStmt {
            body: Block {
                stmts: Vec::new()
            }
            ,
            condition: Boolean::Comparison(Comparison {
                lhs: Expr::Number(Number(1)),
                op: ComparisonOp::Geq,
                rhs: Expr::Number(Number(2)),
            }),
            alternate_ifs: vec![BooleanBlockPair {
                boolean: Boolean::Comparison(Comparison {
                    lhs: Expr::Number(Number(1)),
                    rhs: Expr::Number(Number(3)),
                    op: ComparisonOp::Geq
                }),
                block: Block {
                    stmts: Vec::new()
                }
            }, BooleanBlockPair {
                boolean: Boolean::Comparison(Comparison {
                    lhs: Expr::Number(Number(1)),
                    op: ComparisonOp::Leq,
                    rhs: Expr::Number(Number(2)),
                }),

                block: Block {
                    stmts: Vec::new()
                },
            }],
        })))
    }
}

