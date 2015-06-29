use std::marker::PhantomData;

use identifier::Identifier;
use vecplus::VecPlus;

use syntax::ast;
use syntax::lexer::{Token, TokenLoc, PunctToken, WordToken};
use self::tokenlist::TokenList;

mod tokenlist;

#[derive(Clone)]
pub enum RuleError {
    ExpectingFirst(&'static str, Option<TokenLoc>),
    Expecting(&'static str, Option<TokenLoc>)
}

pub type RuleResult<T> = Result<T, RuleError>;

trait Rule: Sized {
    type Output: Sized = Self;

    fn parse(tokens: &mut TokenList) -> RuleResult<Self::Output>;
}

trait RuleExt: Rule {
    /// Attempts to parse a rule. If the rule is "wrong", None is returned.
    /// The parser will backtrack if the rule doesn't match or there's an error.
    ///
    /// This parses a rule with a lookahead of 1.
    /// If the error from parse is ExpectingFirst, it's converted to None.
    /// All other errors are unmodified.
    #[inline]
    fn parse_lookahead(tokens: &mut TokenList) -> RuleResult<Option<Self::Output>> {
        let mut tokens_copy = *tokens;

        match Self::parse(&mut tokens_copy) {
            Ok(v) => {
                *tokens = tokens_copy;
                Ok(Some(v))
            },
            Err(RuleError::ExpectingFirst(..)) => {
                Ok(None)
            },
            Err(e) => Err(e)
        }
    }

    #[inline]
    fn parse_entire<'a>(tokens: &mut TokenList) -> Option<Self::Output> {
        let mut tokens_copy = *tokens;

        match Self::parse(&mut tokens_copy) {
            Ok(v) => {
                *tokens = tokens_copy;
                Some(v)
            },
            Err(..) => None
        }
    }
}

impl<R> RuleExt for R where R: Rule {}

#[inline]
fn rule_result_not_first<T>(rule_result: RuleResult<T>) -> RuleResult<T> {
    match rule_result {
        Err(RuleError::ExpectingFirst(s, t)) => Err(RuleError::Expecting(s, t)),
        value => value
    }
}

macro_rules! try_notfirst {
    ($r:expr) => {
        try!(rule_result_not_first($r))
    }
}

/// Expects to match 0 or more items (star)
struct DelimitedStarRule<R: Rule, D: Rule> { _marker: PhantomData<(R, D)> }

impl<R: Rule, D: Rule> Rule for DelimitedStarRule<R, D> {
    type Output = Vec<R::Output>;

    fn parse(tokens: &mut TokenList) -> RuleResult<Vec<R::Output>> {
        let mut v = Vec::new();

        if let Some(value) = try!(R::parse_lookahead(tokens)) {
            v.push(value);

            while D::parse_entire(tokens).is_some() {
                let value = try_notfirst!(R::parse(tokens));
                v.push(value);
            }
        }

        Ok(v)
    }
}

/// Expects to match 1 or more items (plus)
struct DelimitedPlusRule<R: Rule, D: Rule> { _marker: PhantomData<(R, D)> }

impl<R: Rule, D: Rule> Rule for DelimitedPlusRule<R, D> {
    type Output = VecPlus<R::Output>;

    fn parse(tokens: &mut TokenList) -> RuleResult<VecPlus<R::Output>> {
        let mut v = Vec::new();

        let value = try!(R::parse(tokens));
        v.push(value);

        while D::parse_entire(tokens).is_some() {
            let value = try_notfirst!(R::parse(tokens));
            v.push(value);
        }

        Ok(VecPlus::from_vec(v).unwrap())
    }
}

macro_rules! delimiter {
    ($t:ident, $punct:expr, $expecting:expr) => (
        struct $t;

        impl Rule for $t {
            type Output = ();

            fn parse(tokens: &mut TokenList) -> RuleResult<()> {
                tokens.pop_punct_expecting($punct, $expecting)
            }
        }
    )
}

delimiter!(Comma, PunctToken::Comma, ",");
delimiter!(Dot, PunctToken::Dot, ".");

struct SeriesRule<R: Rule> { _marker: PhantomData<R> }

impl<R: Rule> Rule for SeriesRule<R> {
    type Output = Vec<R::Output>;

    fn parse(tokens: &mut TokenList) -> RuleResult<Vec<R::Output>> {
        let mut v = Vec::new();

        while let Some(value) = try!(R::parse_lookahead(tokens)) {
            v.push(value);
        }

        Ok(v)
    }
}

macro_rules! rule {
    ($t:ty, ($tokens:ident) $code:block) => (
        impl Rule for $t {
            type Output = $t;
            fn parse($tokens: &mut TokenList) -> RuleResult<$t> {
                $code
            }
        }
    )
}

rule!(Identifier, (tokens) {
    tokens.pop_ident_expecting("identifier")
});

rule!(ast::Module, (tokens) {
    let items = try!(SeriesRule::<ast::ModuleItem>::parse(tokens));

    if tokens.is_empty() {
        Ok(ast::Module {
            items: items
        })
    } else {
        Err(tokens.expecting("end of file"))
    }
});

rule!(ast::ModuleItem, (tokens) {
    let (public, item_type) = if tokens.pop_if_word(WordToken::Pub) {
        (true, try_notfirst!(ast::ModuleItemType::parse(tokens)))
    } else {
        (false, try!(ast::ModuleItemType::parse(tokens)))
    };

    Ok(ast::ModuleItem {
        public: public,
        item_type: item_type
    })
});

rule!(ast::ModuleItemType, (tokens) {
    if let Some(function) = try!(ast::Function::parse_lookahead(tokens)) {
        Ok(ast::ModuleItemType::Function(function))
    } else {
        Err(tokens.expecting("function"))
    }
});

rule!(ast::VarType, (tokens) {
    let primitive = try!(tokens.pop_ident_expecting("primitive type"));

    if tokens.pop_if_punct(PunctToken::InArrow) {
        let arena = try_notfirst!(tokens.pop_ident_expecting("arena after `<-`"));

        Ok(ast::VarType::StaticArena {
            primitive: primitive,
            arena: arena
        })
    } else {
        Ok(ast::VarType::Simple(primitive))
    }
});

rule!(ast::Singleton, (tokens) {
    let name = try!(tokens.pop_ident_expecting("singleton"));

    Ok(ast::Singleton {
        name: name
    })
});

rule!(ast::FunctionParameter, (tokens) {
    let name = try!(tokens.pop_ident_expecting("parameter name"));

    try_notfirst!(tokens.pop_punct_expecting(PunctToken::Colon, "colon after parameter name"));

    let var_type = try_notfirst!(ast::VarType::parse(tokens));

    Ok(ast::FunctionParameter {
        name: name,
        var_type: var_type
    })
});

rule!(ast::Function, (tokens) {
    try!(tokens.pop_word_expecting(WordToken::Fn, "`fn`"));

    let name = try_notfirst!(tokens.pop_ident_expecting("function name"));

    try_notfirst!(tokens.pop_punct_expecting(PunctToken::LeftParen, "`(`, followed by function parameters"));

    let parameters = try_notfirst!(DelimitedStarRule::<ast::FunctionParameter, Comma>::parse(tokens));

    try_notfirst!(tokens.pop_punct_expecting(PunctToken::RightParen, "`,` or `)`"));

    let output = if tokens.pop_if_punct(PunctToken::OutArrow) {
        try_notfirst!(DelimitedPlusRule::<ast::Singleton, Comma>::parse(tokens)).into()
    } else {
        Vec::new()
    };

    let block = try_notfirst!(ast::Block::parse(tokens));

    Ok(ast::Function {
        name: name,
        parameters: parameters,
        output: output,
        block: block
    })
});

rule!(ast::Block, (tokens) {
    try!(tokens.pop_punct_expecting(PunctToken::LeftBrace, "`{`"));

    // This is a somewhat idiosyncratic syntax inspired by the Rust/Ruby style of using a
    // trailing outward expression as the block's evaluated value.

    let mut statements = Vec::new();
    let mut out_expr = None;

    let mut encountered_brace = false;

    while let Some(statement) = try!(ast::Statement::parse_lookahead(tokens)) {
        if statement.requires_semicolon() {
            if tokens.pop_if_punct(PunctToken::Semicolon) {
                // it was a statement!
                statements.push(statement);
            } else {
                // it needs to be an expression. if it isn't, then error.
                match statement {
                    ast::Statement::Expression(expr) => {
                        out_expr = Some(expr);
                        break;
                    },
                    _ => {
                        return Err(tokens.expecting("; after statement"));
                    }
                }
            }
        } else {
            if tokens.pop_if_punct(PunctToken::RightBrace) {
                // end of block
                // it needs to be an expression. if it isn't, then error.

                encountered_brace = true;

                match statement {
                    ast::Statement::Expression(expr) => {
                        out_expr = Some(expr);
                        break;
                    },
                    _ => {
                        return Err(tokens.expecting("; after statement"));
                    }
                }
            } else {
                // it was a statement!
                statements.push(statement);
            }
        }
    }

    if !encountered_brace {
        try_notfirst!(tokens.pop_punct_expecting(PunctToken::RightBrace, "statement, expression or `}`"));
    }

    Ok(ast::Block {
        statements: statements,
        out_expr: out_expr
    })
});

rule!(ast::Path, (tokens) {
    let idents = try!(DelimitedPlusRule::<Identifier, Dot>::parse(tokens));

    Ok(ast::Path {
        idents: idents
    })
});

rule!(ast::Statement, (tokens) {
    let output = if tokens.pop_if_word(WordToken::Let) {
        let name = try_notfirst!(tokens.pop_ident_expecting("variable name"));

        try_notfirst!(tokens.pop_punct_expecting(PunctToken::Colon, "`:` followed by variable type"));

        let vartype = try_notfirst!(ast::VarType::parse(tokens));

        let value = if tokens.pop_if_punct(PunctToken::Assign) {
            Some(try_notfirst!(ast::Expression::parse(tokens)))
        } else {
            None
        };

        ast::Statement::Let {
            name: name,
            vartype: vartype,
            value: value
        }
    } else if tokens.pop_if_word(WordToken::ToZero) {
        let singleton_value = try_notfirst!(ast::Expression::parse(tokens));

        let block = try_notfirst!(ast::Block::parse(tokens));

        ast::Statement::ToZero {
            singleton_value: singleton_value,
            block: block
        }
    } else if tokens.pop_if_word(WordToken::Return) {
        let expr = try_notfirst!(ast::Expression::parse(tokens));

        ast::Statement::Return(expr)
    } else {
        let tokens_copy = *tokens;

        let assignment = if let Some(paths) = try!(DelimitedPlusRule::<ast::Path, Comma>::parse_lookahead(tokens)) {
            if tokens.pop_if_punct(PunctToken::Assign) {
                Some(paths)
            } else {
                None
            }
        } else {
            None
        };

        if let Some(paths) = assignment {
            // path1, path2 = expr
            if let Some(expr) = try!(ast::Expression::parse_lookahead(tokens)) {
                ast::Statement::Assign {
                    paths: paths,
                    value: expr
                }
            } else {
                return Err(tokens.expecting("expression after `=`"))
            }
        } else {
            // nope - it's just an expression. backtracking...
            *tokens = tokens_copy;

            if let Some(expr) = try!(ast::Expression::parse_lookahead(tokens)) {
                ast::Statement::Expression(expr)
            } else {
                return Err(tokens.expecting("expression"))
            }
        }
    };

    Ok(output)
});

impl ast::Statement {
    fn requires_semicolon(&self) -> bool {
        // rule of thumb: any statement that ends with a code block doesn't need a trailing semicolon.
        match self {
            &ast::Statement::Expression(ref expr) => match expr {
                &ast::Expression::Block(..) => false,
                &ast::Expression::If(..) => false,
                _ => true
            },
            &ast::Statement::ToZero {..} => false,
            _ => true
        }
    }
}

rule!(ast::Expression, (tokens) {
    ast::Expression::parse_precedence(tokens, 0)
});

impl ast::Expression {
    /// Expressions are parsed using an algorithm known as "precedence climbing".
    ///
    /// Precedence can be tricky to implement with recursive descent parsers,
    /// so this is a simple method that doesn't involve creating different
    /// rules for different precedence levels.
    fn parse_precedence(tokens: &mut TokenList, min_precedence: u8) -> RuleResult<ast::Expression> {
        let mut expr = try!(ast::Expression::parse_beginning(tokens));
        let mut prev_tokens = *tokens;

        // Test for after-expression tokens
        while let Some(binary_op) = try_notfirst!(ast::BinaryOp::parse_lookahead(tokens)) {
            let binary_op_precedence = binary_op.precedence();

            if binary_op_precedence >= min_precedence {
                // Assuming left associative
                let q = binary_op_precedence + 1;
                let rhs = try_notfirst!(ast::Expression::parse_precedence(tokens, q));

                let new_expr = ast::Expression::BinaryOp {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    op: binary_op
                };

                expr = new_expr;

                prev_tokens = *tokens;
            } else {
                // Backtrack if the precedence is lower
                *tokens = prev_tokens;
                // Let the previous expression rule with the lower precedence (if any) take over
                break;
            }
        }

        Ok(expr)
    }

    fn parse_beginning(tokens: &mut TokenList) -> RuleResult<ast::Expression> {
        if tokens.pop_if_punct(PunctToken::LeftParen) {
            if let Some(encased_expression) = try!(ast::Expression::parse_lookahead(tokens)) {
                try!(tokens.pop_punct_expecting(PunctToken::RightParen, "`)` after expression"));
                Ok(encased_expression)
            } else {
                Err(tokens.expecting("expression after `(`"))
            }
        } else if let Some(path) = try!(ast::Path::parse_lookahead(tokens)) {
            if tokens.pop_if_punct(PunctToken::LeftParen) {
                // Function call
                let arguments = try_notfirst!(DelimitedStarRule::<ast::Expression, Comma>::parse(tokens));

                try_notfirst!(tokens.pop_punct_expecting(PunctToken::RightParen, "`)` after function arguments"));

                Ok(ast::Expression::FunctionCall {
                    path: path,
                    arguments: arguments
                })
            } else {
                Ok(ast::Expression::Item(path))
            }
        } else if let Some(number) = tokens.pop_if_number() {
            Ok(ast::Expression::Number(number))
        } else if let Some(if_) = try!(ast::If::parse_lookahead(tokens)) {
            Ok(ast::Expression::If(if_))
        } else {
            Err(tokens.expecting("identifier, function call or number"))
        }
    }
}

rule!(ast::If, (tokens) {
    try!(tokens.pop_word_expecting(WordToken::If, "if"));

    let predicate = try_notfirst!(ast::Expression::parse(tokens));

    let on_true = try_notfirst!(ast::Block::parse(tokens));

    let otherwise = if tokens.pop_if_word(WordToken::Else) {
        Some(try_notfirst!(ast::BlockOrIf::parse(tokens)))
    } else {
        None
    };

    Ok(ast::If {
        predicate: Box::new(predicate),
        on_true: Box::new(on_true),
        otherwise: otherwise.map(|v| Box::new(v))
    })
});

rule!(ast::BlockOrIf, (tokens) {
    if let Some(block) = try!(ast::Block::parse_lookahead(tokens)) {
        Ok(ast::BlockOrIf::Block(block))
    } else if let Some(if_) = try!(ast::If::parse_lookahead(tokens)) {
        Ok(ast::BlockOrIf::If(if_))
    } else {
        Err(tokens.expecting("block or if expression after `else`"))
    }
});

rule!(ast::BinaryOp, (tokens) {
    tokens.pop_expecting("binary operator", |token| {
        macro_rules! puncts {
            ($($punct:ident => $binaryop:ident),*) => (match token {
                $(&Token::Punct(::syntax::lexer::PunctToken::$punct) => Some(ast::BinaryOp::$binaryop)),*,
                _ => None
            })
        }

        // PunctToken => BinaryOp
        puncts! {
            Equal => Equal,
            NotEqual => NotEqual,
            LessThan => LessThan,
            LessThanOrEqual => LessThanOrEqual,
            GreaterThan => GreaterThan,
            GreaterThanOrEqual => GreaterThanOrEqual,
            Plus => Add,
            Minus => Subtract,
            DoubleAmpersand => LogicAnd,
            DoublePipe => LogicOr,
            Ampersand => BitAnd,
            Pipe => BitOr,
            Caret => BitXor,
            ShiftLeft => ShiftLeft,
            ShiftRight => ShiftRight,
            Colon => Cast
        }
    })
});

impl ast::BinaryOp {
    /// Operators with a higher precedence have a higher number.
    fn precedence(self) -> u8 {
        use super::ast::BinaryOp::*;

        match self {
            Cast => 10,
            Add | Subtract => 9,
            ShiftLeft | ShiftRight => 8,
            LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => 7,
            Equal | NotEqual => 6,
            BitAnd => 5,
            BitXor => 4,
            BitOr => 3,
            LogicAnd => 2,
            LogicOr => 1,
        }
    }
}

pub fn parse(token_slice: &[TokenLoc]) -> RuleResult<ast::Module> {
    let mut tokens = TokenList::new(token_slice);

    ast::Module::parse(&mut tokens)
}
