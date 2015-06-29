use vecplus::VecPlus;
use identifier::Identifier;

pub struct Module {
    pub items: Vec<ModuleItem>
}

pub struct ModuleItem {
    pub public: bool,
    pub item_type: ModuleItemType
}

pub enum ModuleItemType {
    Function(Function)
}

pub enum VarType {
    // primitive <- arena
    StaticArena {
        primitive: Identifier,
        arena: Identifier
    },
    // Primitive or singleton
    Simple(Identifier)
}

pub struct Singleton {
    pub name: Identifier
}

pub struct FunctionParameter {
    pub name: Identifier,
    pub var_type: VarType
}

pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<FunctionParameter>,
    pub output: Vec<Singleton>,
    pub block: Block
}

pub struct Block {
    pub statements: Vec<Statement>,
    pub out_expr: Option<Expression>
}

// an item location, delimited by dots
pub struct Path {
    pub idents: VecPlus<Identifier>
}

pub enum Statement {
    Expression(Expression),
    Let {
        name: Identifier,
        vartype: VarType,
        value: Option<Expression>
    },
    Assign {
        paths: VecPlus<Path>,
        value: Expression
    },
    ToZero {
        singleton_value: Expression,
        block: Block
    },
    Return(Expression)
}

pub enum Expression {
    Item(Path),
    Number(u32),
    BinaryOp {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BinaryOp
    },
    TernaryOp {
        lhs: Box<Expression>,
        mhs: Box<Expression>,
        rhs: Box<Expression>,
        op: TernaryOp
    },
    FunctionCall {
        path: Path,
        arguments: Vec<Expression>
    },
    Block(Box<Block>),
    If(If)
}

pub struct If {
    pub predicate: Box<Expression>,
    pub on_true: Box<Block>,
    pub otherwise: Option<Box<BlockOrIf>>
}

pub enum BlockOrIf {
    Block(Block),
    If(If)
}

#[derive(Copy, Clone)]
pub enum BinaryOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Add,
    Subtract,
    LogicAnd,
    LogicOr,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    Cast
}

pub enum TernaryOp {
    AddWithCarry,
    SubtractWithCarry
}
