use crate::{expr::Expr, utils, Env, Val};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ComparisonOp {
    Geq,
    Leq,
    Lt,
    Gt,
    Eq,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Comparison {
    pub(crate) lhs: Expr,
    pub(crate) op: ComparisonOp,
    pub(crate) rhs: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Boolean {
    Comparison(Comparison),
    BooleanExpr {
        lhs: Box<Self>,
        rhs: Box<Self>,
        op: BooleanOp,
    },
    True,
    False,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BooleanOp {
    And,
    Or,
}

impl ComparisonOp {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        utils::tag(">%", s)
            .map(|s| (s, Self::Geq))
            .or_else(|_| utils::tag("%%", s).map(|s| (s, Self::Eq)))
            .or_else(|_| utils::tag("%<", s).map(|s| (s, Self::Leq)))
            .or_else(|_| utils::tag("<", s).map(|s| (s, Self::Lt)))
            .or_else(|_| utils::tag(">", s).map(|s| (s, Self::Gt)))
    }
}

impl Comparison {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        let (s, lhs) = Expr::new(s)?;

        let (s, _) = utils::extract_whitespace1(s)?;
        let (s, op) = ComparisonOp::new(s)?;
        let (s, _) = utils::extract_whitespace1(s)?;

        let (s, rhs) = Expr::new(s)?;

        Ok((s, Self { lhs, rhs, op }))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        let (lhs, rhs) = self.get_values(&mut env.create_child())?;

        match self.op {
            ComparisonOp::Geq => Ok(Val::Bool(lhs >= rhs)),
            ComparisonOp::Leq => Ok(Val::Bool(lhs <= rhs)),
            ComparisonOp::Gt => Ok(Val::Bool(lhs > rhs)),
            ComparisonOp::Lt => Ok(Val::Bool(lhs < rhs)),
            ComparisonOp::Eq => Ok(Val::Bool(lhs == rhs)),
        }
    }

    pub(crate) fn get_values(&self, env: &mut Env) -> Result<(i32, i32), String> {
        match (
            self.lhs.evaluate(&mut env.create_child())?,
            self.rhs.evaluate(&mut env.create_child())?,
        ) {
            (Val::Number(a), Val::Number(b)) => Ok((a, b)),
            _ => Err("Type not permitted in comparison expressions".to_string()),
        }
    }
}

impl BooleanOp {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        utils::tag("||", s)
            .map(|f| (f, Self::And))
            .or_else(|_| utils::tag("&", s).map(|f| (f, Self::Or)))
    }
}

impl Boolean {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        Self::new_non_comparison(s).or_else(|_| Self::new_comparable(s))
    }

    fn new_comparable(s: &str) -> Result<(&str, Self), String> {
        Comparison::new(s)
            .map(|(s, comp)| (s, Self::Comparison(comp)))
            .or_else(|_| utils::tag("tRuE", s).map(|_| (s, Boolean::True)))
            .or_else(|_| utils::tag("fAlSe", s).map(|_| (s, Boolean::False)))
    }

    fn new_non_comparison(s: &str) -> Result<(&str, Self), String> {
        let (s, lhs) = Boolean::new_comparable(s)?;
        let (s, _) = utils::extract_whitespace1(s)?;
        let (s, op) = BooleanOp::new(s)?;
        let (s, _) = utils::extract_whitespace1(s)?;
        let (s, rhs) = Boolean::new_comparable(s)?;

        Ok((
            s,
            Self::BooleanExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            },
        ))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<Val, String> {
        match self {
            Boolean::Comparison(comp) => comp.eval(&mut env.create_child()),
            Boolean::BooleanExpr { lhs, rhs, op } => {
                let lhs = lhs.eval(env)?;
                let rhs = rhs.eval(env)?;

                match op {
                    BooleanOp::And => {
                        let lhs_val = match lhs {
                            Val::Bool(bool) => bool,
                            _ => true,
                        };

                        let rhs_val = match rhs {
                            Val::Bool(bool) => bool,
                            _ => true,
                        };

                        Ok(Val::Bool(lhs_val && rhs_val))
                    }
                    BooleanOp::Or => {
                        let lhs_val = match lhs {
                            Val::Bool(bool) => bool,
                            _ => true,
                        };

                        let rhs_val = match rhs {
                            Val::Bool(bool) => bool,
                            _ => true,
                        };

                        Ok(Val::Bool(lhs_val || rhs_val))
                    }
                }
            }
            Boolean::True => Ok(Val::Bool(true)),
            Boolean::False => Ok(Val::Bool(false)),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::env::Env;
    use crate::expr::{Expr, Number, Op};
    use crate::val::Val;

    #[test]
    fn parse_geq() {
        assert_eq!(ComparisonOp::new(">%"), Ok(("", ComparisonOp::Geq)));
    }
    #[test]
    fn parse_leq() {
        assert_eq!(ComparisonOp::new("%<"), Ok(("", ComparisonOp::Leq)));
    }
    #[test]
    fn parse_lt() {
        assert_eq!(ComparisonOp::new("<"), Ok(("", ComparisonOp::Lt)));
    }
    #[test]
    fn parse_gt() {
        assert_eq!(ComparisonOp::new(">"), Ok(("", ComparisonOp::Gt)));
    }
    #[test]
    fn parse_eq() {
        assert_eq!(ComparisonOp::new("%%"), Ok(("", ComparisonOp::Eq)));
    }

    #[test]
    fn parse_and() {
        assert_eq!(BooleanOp::new("||"), Ok(("", BooleanOp::And)));
    }

    #[test]
    fn parse_or() {
        assert_eq!(BooleanOp::new("&"), Ok(("", BooleanOp::Or)));
    }

    #[test]
    fn parse_comparison() {
        assert_eq!(
            Boolean::new("2 >% 1"),
            Ok((
                "",
                Boolean::Comparison(Comparison {
                    lhs: Expr::Number(Number(2)),
                    op: ComparisonOp::Geq,
                    rhs: Expr::Number(Number(1))
                })
            ))
        )
    }

    #[test]
    fn parse_boolean_expr() {
        assert_eq!(
            Boolean::new("2 >% 1 & 3 %% 2"),
            Ok((
                "",
                Boolean::BooleanExpr {
                    lhs: Box::new(Boolean::Comparison(Comparison {
                        lhs: Expr::Number(Number(2)),
                        rhs: Expr::Number(Number(1)),
                        op: ComparisonOp::Geq
                    })),
                    rhs: Box::new(Boolean::Comparison(Comparison {
                        lhs: Expr::Number(Number(3)),
                        rhs: Expr::Number(Number(2)),
                        op: ComparisonOp::Eq
                    })),
                    op: BooleanOp::Or
                }
            ))
        )
    }

    #[test]
    fn eval_boolean_expr() {
        assert_eq!(
            Boolean::BooleanExpr {
                lhs: Box::new(Boolean::True),
                rhs: Box::new(Boolean::False),
                op: BooleanOp::And
            }
            .eval(&mut Env::default()),
            Ok(Val::Bool(false))
        )
    }
}
