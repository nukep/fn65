use identifier::Identifier;

use syntax::lexer::{Token, TokenLoc, PunctToken, WordToken};
use super::{RuleError, RuleResult};

#[derive(Copy, Clone)]
pub struct TokenList<'a> {
    tokens: &'a [TokenLoc]
}

impl<'a> TokenList<'a> {
    pub fn new(tokens: &'a [TokenLoc]) -> TokenList<'a> {
        TokenList { tokens: tokens }
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.len() == 0
    }

    pub fn expecting(&self, expecting_message: &'static str) -> RuleError {
        let next = if self.tokens.len() > 0 {
            Some(self.tokens[0].clone())
        } else {
            None
        };

        RuleError::ExpectingFirst(expecting_message, next)
    }

    pub fn pop_if<F, G, R>(&mut self, no_tokens: R, f: F, pop: G) -> R
    where F: FnOnce(&Token) -> R, G: FnOnce(&R) -> bool
    {
        if self.tokens.len() > 0 {
            let token = &self.tokens[0].token;

            let result = f(token);

            if pop(&result) {
                self.tokens = &self.tokens[1..];
            }

            result
        } else {
            no_tokens
        }
    }

    pub fn pop_expecting<F, R>(&mut self, expecting_message: &'static str, f: F) -> RuleResult<R>
    where F: FnOnce(&Token) -> Option<R>
    {
        let err = self.expecting(expecting_message);
        self.pop_if(Err(err.clone()), |token| f(token).ok_or(err), |v| v.is_ok())
    }

    pub fn pop_if_punct(&mut self, token: PunctToken) -> bool {
        self.pop_if(false, |t| match t {
            &Token::Punct(p) => p == token,
            _ => false
        }, |&v| v)
    }

    pub fn pop_punct_expecting(&mut self, token: PunctToken, expecting_message: &'static str) -> RuleResult<()> {
        if self.pop_if_punct(token) { Ok(()) }
        else { Err(self.expecting(expecting_message)) }
    }

    pub fn pop_if_word(&mut self, token: WordToken) -> bool {
        self.pop_if(false, |t| match t {
            &Token::Word(p) => p == token,
            _ => false
        }, |&v| v)
    }

    pub fn pop_word_expecting(&mut self, token: WordToken, expecting_message: &'static str) -> RuleResult<()> {
        if self.pop_if_word(token) { Ok(()) }
        else { Err(self.expecting(expecting_message)) }
    }

    pub fn pop_if_number(&mut self) -> Option<u32> {
        self.pop_if(None, |t| match t {
            &Token::Number(p) => Some(p),
            _ => None
        }, |v| v.is_some())
    }

    pub fn pop_number_expecting(&mut self, expecting_message: &'static str) -> RuleResult<u32> {
        if let Some(number) = self.pop_if_number() { Ok(number) }
        else { Err(self.expecting(expecting_message)) }
    }

    pub fn pop_if_ident(&mut self) -> Option<Identifier> {
        self.pop_if(None, |t| match t {
            &Token::Ident(ref i) => Some(i.clone()),
            _ => None
        }, |v| v.is_some())
    }

    pub fn pop_ident_expecting(&mut self, expecting_message: &'static str) -> RuleResult<Identifier> {
        self.pop_if_ident().ok_or_else(|| self.expecting(expecting_message))
    }
}
