use crate::{env::Env, utils, val::Val};

use super::function_call::FuncCall;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct BindingUsage {
    pub(crate) name: String,
}

impl BindingUsage {
    pub(super) fn new(s: &str) -> Result<(&str, Self), String> {
        let (s, name) = utils::extract_ident(s)?;

        Ok((
            s,
            Self {
                name: name.to_string(),
            },
        ))
    }

    pub(super) fn eval(&self, env: &Env) -> Result<Val, String> {
        env.get_binding(&self.name).or_else(|err_msg| {
            if env.get_func(&self.name).is_ok() {
                FuncCall {
                    callee: self.name.clone(),
                    params: Vec::new(),
                }
                .eval(env)
            } else {
                Err(err_msg)
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Env;

    #[test]
    fn retrive_binding_value_if_created() {
        let mut env = Env::default();
        env.store_binding("foo".to_string(), Val::Number(10));
        assert_eq!(env.get_binding("foo"), Ok(Val::Number(10)));
    }

    #[test]
    fn do_not_retrieve_value_if_not_created() {
        let env = Env::default();
        assert_eq!(
            env.get_binding("foo"),
            Err("variable with name 'foo' does not exist".to_string())
        );
    }
}
