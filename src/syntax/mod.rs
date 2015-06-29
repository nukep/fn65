use std::fmt;

use modulefs::ModuleFs;

pub mod ast;
mod lexer;
mod parser;


pub enum ParseError<M: fmt::Debug> {
    LexerError(lexer::LexerError),
    ExpectingError {
        message: &'static str,
        token: Option<lexer::TokenLoc>
    },
    ModuleError(M)
}

impl<M: fmt::Debug> From<lexer::LexerError> for ParseError<M> {
    fn from(err: lexer::LexerError) -> ParseError<M> {
        ParseError::LexerError(err)
    }
}

impl<M: fmt::Debug> From<parser::RuleError> for ParseError<M> {
    fn from(err: parser::RuleError) -> ParseError<M> {
        let (message, token) = match err {
            parser::RuleError::ExpectingFirst(m, t) => (m, t),
            parser::RuleError::Expecting(m, t) => (m, t)
        };

        ParseError::ExpectingError {
            message: message,
            token: token
        }
    }
}

impl<M: fmt::Debug> fmt::Debug for ParseError<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &ParseError::LexerError(ref err) => {
                write!(f, "lexer error: {:?}", err)
            },
            &ParseError::ExpectingError { ref message, ref token } => {
                let got = match token {
                    &Some(ref t) => format!("{:?}", t),
                    &None => format!("nothing")
                };

                write!(f, "syntax error: expecting {}, got {}", message, got)
            },
            &ParseError::ModuleError(ref err) => {
                write!(f, "module error: {:?}", err)
            }
        }
    }
}

pub fn parse<M>(module: M) -> Result<ast::Module, ParseError<M::ModuleError>>
where M: ModuleFs
{
    let mut lexer = lexer::Lexer::new();

    let input = module.read_module();

    let tokens = try!(lexer.feed(input.chars()));

    let module = try!(parser::parse(&tokens));

    Ok(module)
}
