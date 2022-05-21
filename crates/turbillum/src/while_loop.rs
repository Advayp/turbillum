use crate::boolean_expr::Boolean;
use crate::expr::block::Block;
use crate::utils;

#[derive(Debug, PartialEq, Clone)]
pub struct WhileLoop {
    condition: Boolean, 
    body: Block
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

        let (s, _)  = utils::extract_whitespace(s);

        let (s, body) = Block::new(s)?;

        Ok((s, Self {
            body,
            condition
        }))
    }
}