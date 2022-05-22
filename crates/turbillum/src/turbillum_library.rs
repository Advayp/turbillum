use crate::statements::Stmt;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FunctionType {
    pub(crate) params: Vec<String>,
    pub(crate) body: Stmt
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Library {
    functions: Vec<FunctionType>
}

impl Library {
    pub(crate) fn unpack(self) -> Vec<FunctionType> {
       self.functions
    }
}