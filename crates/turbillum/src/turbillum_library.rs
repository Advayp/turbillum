use crate::statements::Stmt;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FunctionType {
    pub(crate) params: Vec<String>,
    pub(crate) body: Stmt,
    pub(crate) name: String
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Library {
    pub(crate) functions: Vec<FunctionType>
}
