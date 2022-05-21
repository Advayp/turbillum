use crate::{statements::Stmt, utils, Env};

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct FuncDef {
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) body: Box<Stmt>,
}

impl FuncDef {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        let s = utils::tag("funky", s)?;
        let (s, _) = utils::extract_whitespace1(s)?;

        let (s, name) = utils::extract_ident(s)?;
        let (s, _) = utils::extract_whitespace(s);
        let s = utils::tag("|", s)?;
        let (s, params) = utils::sequence(
            |s| utils::extract_ident(s).map(|(s, ident)| (s, ident.to_string())),
            s,
        )?;
        dbg!(s);

        let s = utils::tag("|", s)?;
        let (s, _) = utils::extract_whitespace(s);
        let s = utils::tag("=>", s)?;
        let (s, _) = utils::extract_whitespace(s);

        let (s, body) = Stmt::new(s)?;

        dbg!(s);

        Ok((
            s,
            Self {
                name: name.to_string(),
                params,
                body: Box::new(body),
            },
        ))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<(), String> {
        env.store_func(self.name.clone(), self.params.clone(), *self.body.clone());
        Ok(())
    }
}

mod tests {
    use crate::expr::{binding_usage::BindingUsage, Expr, Op};
    use crate::function::FuncDef;
    use crate::statements::Stmt;

    #[test]
    fn parse_func_def_with_multiple_params() {
        assert_eq!(
            FuncDef::new("funky asdf|x y| => x + y"),
            Ok((
                "",
                FuncDef {
                    name: "asdf".to_string(),
                    params: vec!["x".to_string(), "y".to_string()],
                    body: Box::new(Stmt::Expr(Expr::Operation {
                        lhs: Box::new(Expr::BindingUsage(BindingUsage {
                            name: "x".to_string(),
                        })),
                        rhs: Box::new(Expr::BindingUsage(BindingUsage {
                            name: "y".to_string(),
                        })),
                        op: Op::Add,
                    })),
                },
            )),
        );
    }
}
