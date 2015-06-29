use std::fmt;
use identifier::Identifier;
use vecplus::VecPlus;

#[derive(Clone, Debug)]
pub struct TokenLoc {
    pub loc: Loc,
    pub token: Token
}

#[derive(Copy, Clone, Debug)]
pub struct Loc {
    pub line: u32,
    pub column: u32
}

impl Loc {
    pub fn new() -> Loc {
        Loc {
            line: 1,
            column: 1
        }
    }

    pub fn offset_column(self, diff: u32) -> Loc {
        Loc {
            line: self.line,
            column: self.column + diff
        }
    }

    pub fn next_line(self) -> Loc {
        Loc {
            line: self.line + 1,
            column: 1
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token {
    Ident(Identifier),         // identifier
    MacroIdent(Identifier),    // identifier!
    Number(u32),

    Word(WordToken),
    Punct(PunctToken)
}

macro_rules! gen_word {
    ($($word:expr => $token:ident),*) => (
        #[derive(Copy, Clone, PartialEq, Eq)]
        pub enum WordToken {
            $($token),*
        }

        impl fmt::Debug for WordToken {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                write!(fmt, "`{}`", self.word())
            }
        }

        impl WordToken {
            pub fn word(self) -> &'static str {
                match self {
                    $(
                        WordToken::$token => $word
                    ),*
                }
            }
        }

        fn match_word_or_ident(buf: String) -> Token {
            // Make all letters lowercase for comparison
            let word_cmp: String = buf.chars().flat_map( |c| c.to_lowercase() ).collect();

            match &word_cmp as &str {
                $(
                    $word => Token::Word(WordToken::$token)
                ),*,
                _ => Token::Ident(Identifier::new(buf).unwrap())
            }
        }
    )
}

macro_rules! gen_punct {
    ($($punct:expr => $token:ident),*) => (
        #[derive(Copy, Clone, PartialEq, Eq)]
        pub enum PunctToken {
            $($token),*
        }

        impl fmt::Debug for PunctToken {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> ::std::result::Result<(), fmt::Error> {
                write!(fmt, "`{}`", self.punct())
            }
        }

        impl PunctToken {
            pub fn punct(self) -> &'static str {
                match self {
                    $(
                        PunctToken::$token => $punct
                    ),*
                }
            }
        }

        const PUNCTS: &'static [PunctToken] = &[$(
            PunctToken::$token
        ),*];


        fn match_punct(buf: &str, loc: Loc) -> LexerResult<Vec<TokenLoc>> {
            // Worst case: O(n^3), where n = buf.len().

            fn m(buf: &str) -> Option<PunctToken> {
                match buf {
                    $(
                        $punct => Some(PunctToken::$token)
                    ),*,
                    _ => None
                }
            }

            if buf.len() == 0 {
                Ok(vec![])
            } else {
                // Try to match the longest punctuation possible,
                // then recursively match the remainder.

                let err = || {
                    Err(LexerError {
                        loc: loc,
                        kind: LexerErrorKind::UnmatchedOperatorString(buf.to_string())
                    })
                };

                (1..buf.len()+1).rev()
                    .map(|i| (&buf[0..i], &buf[i..], loc.offset_column(i as u32)))
                    .filter_map(|(a, b, c)| m(a).map(|punct| (punct, b, c)))
                    .nth(0)
                    .map_or_else(err, |(punct, remainder, remainder_loc)| {
                        let mut l = vec![TokenLoc {
                            loc: remainder_loc,
                            token: Token::Punct(punct)
                        }];
                        let tail = try!(match_punct(remainder, remainder_loc));
                        l.extend(tail);
                        Ok(l)
                    })
            }
        }
    )
}

fn is_punctuation_char(c: char) -> bool {
    "<>-=:;,.#(){}[]!+-*/&|^".chars().any(|v| v == c)
}

gen_word! {
    "fn" => Fn,
    "if" => If,
    "else" => Else,
    "tozero" => ToZero,
    "toneg" => ToNeg,
    "asm" => Asm,
    "let" => Let,
    "pub" => Pub,
    "mod" => Mod,
    "return" => Return,
    "unsafe" => Unsafe
}

gen_punct! {
    "->" => OutArrow,
    "=>" => FatArrow,
    "<-" => InArrow,
    ":" => Colon,
    ";" => Semicolon,
    "," => Comma,
    "." => Dot,
    ".." => DoubleDot,
    "#" => Pound,
    "(" => LeftParen,
    ")" => RightParen,
    "{" => LeftBrace,
    "}" => RightBrace,
    "[" => LeftBracket,
    "]" => RightBracket,
    "=" => Assign,
    "<" => LessThan,
    "<=" => LessThanOrEqual,
    ">" => GreaterThan,
    ">=" => GreaterThanOrEqual,
    "==" => Equal,
    "!=" => NotEqual,
    "+" => Plus,
    "-" => Minus,
    "*" => Asterisk,
    "/" => ForwardSlash,
    "<<" => ShiftLeft,
    ">>" => ShiftRight,
    "&&" => DoubleAmpersand,
    "||" => DoublePipe,
    "&" => Ampersand,
    "|" => Pipe,
    "^" => Caret,
    "++" => DoublePlus,
    "--" => DoubleMinus
}

#[derive(Debug)]
enum LexerState {
    NoState,
    Word,
    Number { start: Option<char>, is_hex: bool },
    Punctuation,
    LineComment(Box<LexerState>),
    BlockComment {
        prev_char: Option<char>,
        prev_state: Box<LexerState>
    }
}

impl LexerState {
    fn can_parse_comment(&self) -> bool {
        match self {
            &LexerState::LineComment(..) => false,
            &LexerState::BlockComment {..} => false,
            _ => true
        }
    }
}

pub type LexerResult<T> = ::std::result::Result<T, LexerError>;

#[derive(Clone, Debug)]
pub struct LexerError {
    pub loc: Loc,
    pub kind: LexerErrorKind
}

#[derive(Clone, Debug)]
pub enum LexerErrorKind {
    UnexpectedCharacter(char),
    UnmatchedOperatorString(String),
    UnfinishedBlockComment
}

pub struct Lexer
{
    buffer_beginning_loc: Loc,
    loc: Loc,
    buffer: String,
    state: LexerState,
    tokens: Vec<TokenLoc>
}

impl Lexer
{
    pub fn new() -> Lexer {
        Lexer {
            buffer_beginning_loc: Loc::new(),
            loc: Loc::new(),
            buffer: String::new(),
            state: LexerState::NoState,
            tokens: Vec::new()
        }
    }

    fn no_state(mut self, c: char) -> LexerResult<Lexer> {
        match c {
            'a'...'z' | 'A'...'Z' | '_' => {
                self.buffer_push(c);
                self.state = LexerState::Word;
                Ok(self)
            },
            '0'...'9' => {
                self.buffer_push(c);
                self.state = LexerState::Number { start: Some(c), is_hex: false };
                Ok(self)
            },
            ' ' | '\t' | '\n' => {
                self.state = LexerState::NoState;
                Ok(self)
            },
            c if is_punctuation_char(c) => {
                self.buffer_push(c);
                self.state = LexerState::Punctuation;
                Ok(self)
            },
            c => Err(LexerError {
                loc: self.loc,
                kind: LexerErrorKind::UnexpectedCharacter(c)
            })
        }
    }

    fn word(mut self, c: char)-> LexerResult<Lexer> {
        match c {
            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => {
                self.buffer_push(c);
                self.state = LexerState::Word;
                Ok(self)
            },
            '!' => {
                let buf = self.flush_buffer();
                self.push_token(Token::MacroIdent(Identifier::new(buf).unwrap()));
                self.state = LexerState::NoState;
                Ok(self)
            },
            c => {
                let buf = self.flush_buffer();
                let token = match_word_or_ident(buf);

                self.push_token(token);
                self.no_state(c)
            }
        }
    }

    fn number(mut self, c: char, start: Option<char>, is_hex: bool)-> LexerResult<Lexer> {
        if start == Some('0') && c == 'x' {
            // 0x
            self.buffer.clear();
            self.state = LexerState::Number { start: None, is_hex: true };
            Ok(self)
        } else if is_hex {
            match c {
                '0'...'9' | 'a'...'f' | 'A'...'F' => {
                    self.buffer_push(c);
                    self.state = LexerState::Number { start: None, is_hex: true };
                    Ok(self)
                },
                c => {
                    let buf = self.flush_buffer();

                    if buf.is_empty() {
                        // This is a bit odd. We should pretend as if we were never parsing
                        // a hexadecimal constant in the first place.
                        self.push_token(Token::Number(0));
                        self.buffer_push('x');
                        self.word(c)
                    } else {
                        let num = u32::from_str_radix(&buf, 16).unwrap();
                        self.push_token(Token::Number(num));
                        self.no_state(c)
                    }
                }
            }
        } else {
            match c {
                '0'...'9' => {
                    self.buffer_push(c);
                    self.state = LexerState::Number { start: None, is_hex: false };
                    Ok(self)
                },
                c => {
                    let buf = self.flush_buffer();
                    let num = u32::from_str_radix(&buf, 10).unwrap();
                    self.push_token(Token::Number(num));
                    self.no_state(c)
                }
            }
        }
    }

    fn punctuation(mut self, c: char) -> LexerResult<Lexer> {
        if is_punctuation_char(c) {
            self.buffer_push(c);
            self.state = LexerState::Punctuation;
            Ok(self)
        } else {
            let buf = self.flush_buffer();
            let l = try!(match_punct(&buf, self.buffer_beginning_loc));
            self.tokens.extend(l);
            self.no_state(c)
        }
    }

    fn line_comment(c: char, prev_state: Box<LexerState>) -> LexerState {
        if c == '\n' {
            *prev_state
        } else {
            LexerState::LineComment(prev_state)
        }
    }

    fn block_comment(c: char, prev_char: Option<char>, prev_state: Box<LexerState>) -> LexerState {
        match (prev_char, c) {
            (Some('*'), '/') => {
                *prev_state
            },
            _ => {
                LexerState::BlockComment {
                    prev_char: Some(c),
                    prev_state: prev_state
                }
            }
        }
    }

    fn feed_character(mut self, c: char, loc: Loc) -> LexerResult<Lexer> {
        self.loc = loc;

        match self.state {
            LexerState::NoState => self.no_state(c),
            LexerState::Word => self.word(c),
            LexerState::Number { start, is_hex } => self.number(c, start, is_hex),
            LexerState::Punctuation => self.punctuation(c),
            LexerState::LineComment(prev_state) => {
                let state = Lexer::line_comment(c, prev_state);
                self.state = state;
                Ok(self)
            },
            LexerState::BlockComment { prev_char, prev_state } => {
                let state = Lexer::block_comment(c, prev_char, prev_state);
                self.state = state;
                Ok(self)
            }
        }
    }

    fn finish(mut self) -> LexerResult<Vec<TokenLoc>> {
        let loc = self.loc;
        let lexer = try!(self.feed_character('\n', loc));

        match lexer.state {
            LexerState::NoState => Ok(lexer.tokens),
            LexerState::BlockComment { .. } => {
                Err(LexerError {
                    loc: loc,
                    kind: LexerErrorKind::UnfinishedBlockComment
                })
            },
            state => panic!("Unexpected lexer state: {:?}", state)
        }
    }

    pub fn feed<I>(self, mut it: I) -> LexerResult<Vec<TokenLoc>>
    where I: Iterator<Item=char> + Clone
    {
        let mut lexer = self;

        let mut loc = Loc::new();

        while let Some(c) = it.next() {
            let might_be_comment = c == '/' && lexer.state.can_parse_comment();

            if might_be_comment {
                let mut it_clone = it.clone();
                if let Some(next_c) = it_clone.next() {
                    match next_c {
                        '/' => {
                            it = it_clone;
                            lexer.state = LexerState::LineComment(Box::new(lexer.state));
                        },
                        '*' => {
                            it = it_clone;
                            lexer.state = LexerState::BlockComment {
                                prev_char: None,
                                prev_state: Box::new(lexer.state)
                            };
                        },
                        _ => {
                            lexer = try!(lexer.feed_character(c, loc));
                        }
                    }
                } else {
                    lexer = try!(lexer.feed_character(c, loc));
                }
            } else {
                lexer = try!(lexer.feed_character(c, loc));
            }

            loc = match c {
                '\t' => loc.offset_column(8),
                '\n' => loc.next_line(),
                _ => loc.offset_column(1)
            };
        }

        lexer.finish()
    }

    fn buffer_push(&mut self, c: char) {
        if self.buffer.len() == 0 {
            self.buffer_beginning_loc = self.loc;
        }
        self.buffer.push(c);
    }

    fn flush_buffer(&mut self) -> String {
        ::std::mem::replace(&mut self.buffer, String::new())
    }

    fn push_token(&mut self, token: Token) {
        self.tokens.push(TokenLoc {
            loc: self.buffer_beginning_loc,
            token: token,
        })
    }
}

#[cfg(test)]
mod test {
    use super::{PUNCTS, is_punctuation_char};
    use super::{Token, PunctToken, Lexer};

    fn new_lexer() -> Lexer {
        Lexer::new()
    }

    #[test]
    fn test_punctuation_chars() {
        for punct in PUNCTS.iter().cloned() {
            assert!(punct.punct().chars().all(is_punctuation_char));
        }
    }

    #[test]
    fn test_lexer_punct_ambiguity() {
        use super::PunctToken::*;

        fn test(s: &str, punct: &[PunctToken]) {
            let mut l = new_lexer();
            let tokens = l.feed(s.chars()).unwrap();

            assert_eq!(tokens.len(), punct.len());

            for (p, tokenloc) in punct.iter().zip(tokens) {
                match tokenloc.token {
                    Token::Punct(v) => assert_eq!(&v, p),
                    _ => panic!()
                }
            }
        }

        test("->=!=", &[OutArrow, Assign, NotEqual]);
        test(">=> =>", &[GreaterThanOrEqual, GreaterThan, FatArrow]);
        test("<-< -", &[InArrow, LessThan, Minus]);
        test("<-<->", &[InArrow, InArrow, GreaterThan]);
        test("<<-", &[ShiftLeft, Minus]);
        test("->>", &[OutArrow, GreaterThan]);
    }

    #[test]
    fn test_lexer() {
        use identifier::Identifier;
        use super::Token::{Punct, Word, Number};
        use super::PunctToken::*;
        use super::WordToken::*;

        fn ident(s: &str) -> Token { Token::Ident(Identifier::new(s.to_string()).unwrap()) }
        fn macro_ident(s: &str) -> Token { Token::MacroIdent(Identifier::new(s.to_string()).unwrap()) }

        fn test(s: &str, t: &[Token]) {
            let mut l = new_lexer();
            let tokens = l.feed(s.chars()).unwrap();

            assert_eq!(tokens.len(), t.len());

            for (p, tokenloc) in t.iter().zip(tokens) {
                assert_eq!(&tokenloc.token, p);
            }
        }

        test("fn test(var: u8) -> A, X {}", &[
            Word(Fn), ident("test"), Punct(LeftParen), ident("var"), Punct(Colon), ident("u8"), Punct(RightParen),
            Punct(OutArrow), ident("A"), Punct(Comma), ident("X"), Punct(LeftBrace), Punct(RightBrace)
        ]);

        test("foo!=", &[macro_ident("foo"), Punct(Assign)]);
        test("foo !=", &[ident("foo"), Punct(NotEqual)]);

        test("65535", &[Number(65535)]);
        test("0x1FG", &[Number(0x1F), ident("G")]);
        test("0xG", &[Number(0), ident("xG")]);

        test("asm {
            LDA #5
            STA (test), x
        };", &[
        Word(Asm), Punct(LeftBrace),
        ident("LDA"), Punct(Pound), Number(5),
        ident("STA"), Punct(LeftParen), ident("test"), Punct(RightParen), Punct(Comma), ident("x"),
        Punct(RightBrace), Punct(Semicolon)]);
    }
}
