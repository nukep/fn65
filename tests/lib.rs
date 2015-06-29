extern crate fn65;

use std::fs;
use fn65::syntax;
use fn65::modulefs::ModuleFs;

mod util;
use self::util::*;

#[test]
fn should_pass() {
    for m in fs::read_dir("./tests/should-pass").unwrap().map(read_entry) {
        let path = m.path().to_string();
        let module = syntax::parse(m).unwrap_with_path(&path);
    }
}

#[test]
fn should_fail_syntax() {
    for m in fs::read_dir("./tests/should-fail-syntax").unwrap().map(read_entry) {
        let path = m.path().to_string();

        match syntax::parse(m) {
            Ok(..) => panic!("Test should fail: {}", path),
            Err(..) => ()
        }
    }
}
