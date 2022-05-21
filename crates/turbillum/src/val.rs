use std::fmt::{self, write};

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Number(i32),
    Unit,
    Bool(bool),
}
impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Unit => write!(f, "Unit"),
            Self::Bool(bl) => write!(f, "{}", bl),
        }
    }
}
