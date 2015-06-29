use std::ops::Deref;

/// An identifier is a subset of a `String`.
///
/// An identifier must:
/// * have a length of at least one
/// * only contain alphanumeric characters and underscores [a-zA-Z0-9_]
/// * not begin with a number
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct Identifier {
    value: String
}

impl Identifier {
    pub fn new(value: String) -> Option<Identifier> {
        let valid = {
            let mut it = value.chars();
            it.nth(0).map_or(false, |first| {
                let first_valid = match first {
                    'a'...'z' | 'A'...'Z' | '_' => true,
                    _ => false
                };

                first_valid && it.all(|c| match c {
                    'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => true,
                    _ => false
                })
            })
        };

        if valid { Some(Identifier { value: value }) }
        else { None }
    }

    pub fn into_string(self) -> String {
        self.value
    }
}

impl Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &str {
        &self.value
    }
}

#[cfg(test)]
mod test {
    use super::Identifier;

    fn ident(s: &str) -> Option<Identifier> {
        Identifier::new(s.to_string())
    }

    #[test]
    fn test_identifier() {
        let pass = &["_7h", "hello_world", "FooBar", "F", "_0"];
        let fail = &["7_h", "hello-world", "", "hello^"];

        for &s in pass {
            let i = ident(s);
            assert_eq!(i.as_ref().map(|v| v as &str), Some(s));
        }

        for &s in fail {
            let i = ident(s);
            assert_eq!(i, None);
        }
    }
}
