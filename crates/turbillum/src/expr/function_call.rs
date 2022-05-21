use crate::env::Env;
use crate::utils;
use crate::val::Val;

use super::Expr;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FuncCall {
    pub(crate) callee: String,
    pub(crate) params: Vec<Expr>,
}

impl FuncCall {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        let (s, callee) = utils::extract_ident(s)?;

        let s = utils::tag("@", s)?;

        let (s, _) = utils::extract_whitespace1(s)?;
        let (s, params) = utils::sequence1(Expr::new, s)?;
        let s = utils::tag(".", s)?;

        Ok((
            s,
            Self {
                callee: callee.to_string(),
                params,
            },
        ))
    }

    pub(crate) fn eval(&self, env: &Env) -> Result<Val, String> {
        let mut child_env = env.create_child();

        let (param_names, body) = env.get_func(&self.callee)?;

        let number_of_expected_parameters = param_names.len();
        let number_of_current_parameters = self.params.len();

        if number_of_expected_parameters != number_of_current_parameters {
            return Err(format!(
                "expected {} parameters, received {}",
                number_of_expected_parameters, number_of_current_parameters
            ));
        }

        for (param_name, param_expr) in param_names.into_iter().zip(&self.params) {
            let param_val = param_expr.evaluate(&mut child_env)?;
            child_env.store_binding(param_name, param_val);
        }

        body.eval(&mut child_env)
    }
}

mod tests {
    use super::super::*;

    #[test]
    fn parse_func_call_with_params() {
        assert_eq!(
            FuncCall::new("factorial@ 1."),
            Ok((
                "",
                FuncCall {
                    callee: "factorial".to_string(),
                    params: vec![Expr::Number(Number(1))],
                },
            )),
        );
    }
}
