use std::collections::HashMap;

use crate::statements::Stmt;
use crate::turbillum_library::{FunctionType, Library};
use crate::val::Val;

#[derive(Debug, PartialEq, Default)]
pub struct Env<'parent> {
    named: HashMap<String, NamedInfo>,
    parent: Option<&'parent Self>,
}

#[derive(Debug, PartialEq, Clone)]
enum NamedInfo {
    Binding(Val),
    Func {params: Vec<String>, body: Stmt},
}

impl<'parent> Env<'parent> {
    pub(crate) fn store_binding(&mut self, name: String, val: Val) {
        self.named.insert(name, NamedInfo::Binding(val));
    }

    pub(crate) fn get_binding(&self, name: &str) -> Result<Val, String> {
        self.get_named_info(name)
            .and_then(NamedInfo::into_binding)
            .ok_or_else(|| format!("variable with name '{}' does not exist", name))
    }

    pub(crate) fn store_func(&mut self, name: String, params: Vec<String>, body: Stmt) {
        self.named.insert(name, NamedInfo::Func {params, body});
    }

    pub(crate) fn get_func(&self, name: &str) -> Result<(Vec<String>, Stmt), String> {
        self.get_named_info(name)
            .and_then(NamedInfo::into_func)
            .ok_or_else(|| format!("function with name ‘{}’ does not exist", name))
    }

    pub(crate) fn create_child(&'parent self) -> Self {
        Self {
            named: HashMap::new(),
            parent: Some(self),
        }
    }

    pub(crate) fn adjust_value(&mut self, name: String, new_val: Val) -> Result<Val, String> {
        if !self.named.contains_key(&name) {
            return Err("that variable does not exist".to_string());
        }

        match new_val {
            Val::Number(num) => match self.named.get(&name).cloned() {
                Some(NamedInfo::Binding(_)) => {
                    *self.named.get_mut(&name).unwrap() = NamedInfo::Binding(Val::Number(num));
                }
                _ => todo!(),
            },
            _ => todo!(),
        }

        Ok(new_val)
    }

    pub(crate) fn import(&mut self, library: Library) -> Result<(), String> {
        for (name, params, body) in library.functions {
            self.named.insert(name, NamedInfo::Func {params, body});
        }

        Ok(())
    }

    fn get_named_info(&self, name: &str) -> Option<NamedInfo> {
        self.named
            .get(name)
            .cloned()
            .or_else(|| self.parent.and_then(|parent| parent.get_named_info(name)))
    }
}

impl NamedInfo {
    fn into_binding(self) -> Option<Val> {
        if let Self::Binding(val) = self {
            Some(val)
        } else {
            None
        }
    }

    fn into_func(self) -> Option<(Vec<String>, Stmt)> {
        if let Self::Func { params, body } = self {
            Some((params, body))
        } else {
            None
        }
    }
}
