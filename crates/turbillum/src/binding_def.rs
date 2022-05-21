use crate::env::Env;
use crate::expr::Expr;
use crate::utils;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct BindingDef {
    pub(crate) name: String,
    pub(crate) value: Expr,
}

impl BindingDef {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        // def s = 10 / 20
        let s = utils::tag("def", s)?;
        let (s, _) = utils::extract_whitespace1(s)?;

        let (s, name) = utils::extract_ident(s)?;
        let (s, _) = utils::extract_whitespace(s);

        let s = utils::tag("=", s)?;

        let (s, _) = utils::extract_whitespace(s);

        let (s, value) = Expr::new(s)?;
        let s = utils::tag(".", s)?;

        Ok((
            s,
            Self {
                name: name.to_string(),
                value,
            },
        ))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<(), String> {
        let mut child = env.create_child();
        env.store_binding(self.name.clone(), self.value.evaluate(&mut child)?);
        Ok(())
    }
}

mod tests {
    use super::*;
    use crate::expr::*;

    #[test]
    fn parse_binding_def() {
        assert_eq!(
            BindingDef::new("def a = 10 / 20."),
            Ok((
                "",
                BindingDef {
                    name: "a".to_string(),
                    value: Expr::Operation {
                        lhs: Box::new(Expr::Number(Number(10))),
                        op: Op::Divide,
                        rhs: Box::new(Expr::Number(Number(20)))
                    }
                }
            ))
        );
    }

    #[test]
    fn cannot_parse_binding_def_without_space_after_def() {
        assert_eq!(
            BindingDef::new("defaaa=1+2"),
            Err("expected a space".to_string()),
        );
    }

    #[test]
    fn cannot_parse_unexpected_variable_declarer() {
        assert_eq!(
            BindingDef::new("let a = 1+2."),
            Err("expected def".to_string())
        );
    }
}
