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
            Ok(Val(bool)) => bool
        };

        while val {
            self.body.eval(env)?;
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
    use crate::expr::block::Block;
    use crate::expr::{Expr, Number};
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
}