mod binding_def;
mod boolean_expr;
mod change_statement;
mod expr;
mod for_loop;
mod function;
mod statements;
mod val;
mod for_condition;
mod if_statement;
mod while_loop;
mod turbillum_library;

mod env;
mod utils;

pub use env::Env;
pub use val::Val;

#[derive(Debug)]
pub struct Parse(statements::Stmt);

impl Parse {
    pub fn eval(&self, env: &mut Env) -> Result<Val, String> {
        self.0.eval(env)
    }
}

pub fn parse(s: &str) -> Result<Parse, String> {
    let (s, stmt) = statements::Stmt::new(s)?;

    if s.is_empty() {
        Ok(Parse(stmt))
    } else {
        Err("failed to parse input".to_string())
    }
}
