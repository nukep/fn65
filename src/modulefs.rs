use std::fmt;

use identifier::Identifier;

pub trait ModuleFs {
    type ModuleError: fmt::Display + fmt::Debug;

    fn open_module(self, name: &Identifier) -> Result<Self, Self::ModuleError>;

    fn read_module(&self) -> &str;

    fn path(&self) -> &str;
}
