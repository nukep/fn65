extern crate fn65;

use std::fs;
use std::io;
use std::path;

use fn65::modulefs::ModuleFs;
use fn65::identifier::Identifier;

pub struct TestModuleFs {
    path: String,
    contents: String
}

impl ModuleFs for TestModuleFs {
    type ModuleError = io::Error;

    fn open_module(self, name: &Identifier) -> Result<Self, Self::ModuleError> {
        unimplemented!()
    }

    fn read_module(&self) -> &str { &self.contents }

    fn path(&self) -> &str { &self.path }
}

fn modulefs_from<P: AsRef<path::Path>>(file_path: P) -> Result<TestModuleFs, io::Error> {
    use std::io::Read;

    let mut file = try!(fs::File::open(&file_path));

    let mut file_contents = String::new();
    try!(file.read_to_string(&mut file_contents));

    Ok(TestModuleFs {
        path: file_path.as_ref().to_str().unwrap().to_string(),
        contents: file_contents
    })
}

pub fn read_entry(entry: io::Result<fs::DirEntry>) -> TestModuleFs {
    let path = entry.unwrap().path();

    modulefs_from(path).unwrap()
}

pub trait UnwrapExt {
    type T;
    fn unwrap_with_path(self, path: &str) -> Self::T;
}

impl<T, E: ::std::fmt::Debug> UnwrapExt for Result<T, E> {
    type T = T;

    fn unwrap_with_path(self, path: &str) -> T {
        match self {
            Ok(v) => v,
            Err(err) => panic!("failed on {:?}: {:?}", path, err)
        }
    }
}
