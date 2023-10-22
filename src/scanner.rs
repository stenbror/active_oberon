
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Symbols {
    /* Reserved keywords */
    Await(u32, u32),
    Begin(u32, u32),
    By(u32, u32),
    Const(u32, u32),
    Case(u32, u32),
    Cell(u32, u32),
    Cellnet(u32, u32),
    Code(u32, u32),
    Definition(u32, u32),
    Do(u32, u32),
    Div(u32, u32),
    End(u32, u32),
    Enum(u32, u32),
    Else(u32, u32),
    Elsif(u32, u32),
    Extern(u32, u32),
    False(u32, u32),
    For(u32, u32),
    Finally(u32, u32),
    If(u32, u32),
    Ignore(u32, u32),
    Imag(u32, u32),
    In(u32, u32),
    Is(u32, u32),
    Import(u32, u32),
    Loop(u32, u32),
    Module(u32, u32),
    Mod(u32, u32),
    Nil(u32, u32),
    Of(u32, u32),
    Or(u32, u32),
    Out(u32, u32),
    Operator(u32, u32),
    Procedure(u32, u32),
    Port(u32, u32),
    Repeat(u32, u32),
    Return(u32, u32),
    Self_(u32, u32),
    New(u32, u32),
    Result(u32, u32),
    Then(u32, u32),
    True(u32, u32),
    To(u32, u32),
    Type(u32, u32),
    Until(u32, u32),
    Var(u32, u32),
    While(u32, u32),
    With(u32, u32),

    /* Types */
    Any(u32, u32),
    Array(u32, u32),
    Object(u32, u32),
    Pointer(u32, u32),
    Record(u32, u32),
    Address(u32, u32),
    Size(u32, u32),
    Alias(u32, u32),

    /* Operators */
    NotEqual(u32, u32), // '#'
    And(u32, u32), // '&'
    LeftParen(u32, u32), // '('
    RightParen(u32, u32), // ')'
    Times(u32, u32), // '*'
    TimesTimes(u32, u32), // '**'
    Plus(u32, u32), // '+'
    PlusTimes(u32, u32), // '+*'
    Comma(u32, u32), // ','
    Minus(u32, u32), // '-'
    Period(u32, u32), // '.'
    Upto(u32, u32), // '..'
    DotTimes(u32, u32), // '.*'
    DotSlash(u32, u32), // './'
    DotEqual(u32, u32), // '.='
    DotUnequal(u32, u32), // '.#'
    DotGreater(u32, u32), // '.>'
    DotGreaterEqual(u32, u32), // '.<='
    DotLess(u32, u32), // '.<'
    DotLessEqual(u32, u32), // '.<='
    Slash(u32, u32), // '/'
    Colon(u32, u32), // ':'
    Becomes(u32, u32), // ':='
    Semicolon(u32, u32), // ';'
    Less(u32, u32), // '<'
    LessEqual(u32, u32), // '<='
    Equal(u32, u32), // '='
    Greater(u32, u32), // '>'
    GreaterEqual(u32, u32), // '>='
    LeftBracket(u32, u32), // '['
    RightBracket(u32, u32), // ']'
    Arrow(u32, u32), // '^'
    LeftBrace(u32, u32), // '{'
    Bar(u32, u32), // '|'
    RightBrace(u32, u32), // '}'
    Not(u32, u32), // '~'
    BackSlash(u32, u32), // '\'
    Transpose(u32, u32), // '`'
    QuestionMark(u32, u32), // '?'
    QuestionMarks(u32, u32), // '??'
    ExclaimMark(u32, u32), // '!'
    ExclaimMarks(u32, u32), // '!!'
    LessLess(u32, u32), // '<<'
    LessLessQ(u32, u32), // '<<?'
    GreaterGreater(u32, u32), // '>>'
    GreaterGreaterQ(u32, u32), // >>?

    /* Literals */
    Ident(u32, u32, String),
    Integer(u32, u32, String),
    Real(u32, u32, String),
    String(u32, u32, String),
    Character(u32, u32, char),

    EOF
}

pub trait ActiveOberonScannerMethods {
    fn is_operator(ch1: char, ch2: char, ch3: char, pos: u32) -> Option<(Symbols, u8)>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ActiveOberonScanner {

}

impl ActiveOberonScannerMethods for ActiveOberonScanner {
    fn is_operator(ch1: char, ch2: char, ch3: char, pos: u32) -> Option<(Symbols, u8)> {
        match (ch1, ch2, ch3) {
            ( '.', '>', '=' )   => Some( (Symbols::DotGreaterEqual(pos, pos + 3), 3) ),
            ( '.', '>', _ )     => Some( (Symbols::DotGreater(pos, pos + 2), 2) ),
            ( '.', '<', '=' )   => Some( (Symbols::DotLessEqual(pos, pos + 3), 3) ),
            ( '.', '<', _ )     => Some( (Symbols::DotLess(pos, pos + 2), 2) ),
            ( '.', '.', _ )     => Some( (Symbols::Upto(pos, pos + 2), 2) ),
            ( '.', '*', _ )     => Some( (Symbols::DotTimes(pos, pos + 2), 2) ),
            ( '.', '/', _ )     => Some( (Symbols::DotSlash(pos, pos + 2), 2) ),
            ( '.', '=', _ )     => Some( (Symbols::DotEqual(pos, pos + 2), 2) ),
            ( '.', '#', _ )     => Some( (Symbols::DotUnequal(pos, pos + 2), 2) ),
            ( '.', _ , _ )      => Some( (Symbols::Period(pos, pos + 1), 1) ),
            ( '<', '=', _ )     => Some( (Symbols::LessEqual(pos, pos + 2), 2) ),
            ( '<', _ , _ )      => Some( (Symbols::Less(pos, pos + 1), 1) ),
            ( '>', '=', _ )     => Some( (Symbols::GreaterEqual(pos, pos + 2), 2) ),
            ( '>', _ , _ )      => Some( (Symbols::Greater(pos, pos + 1), 1) ),
            ( ':', '=', _ )     => Some( (Symbols::Becomes(pos, pos + 2), 2) ),
            ( ':', _ , _ )      => Some( (Symbols::Colon(pos, pos + 1), 1) ),
            ( '+', '*', _ )     => Some( (Symbols::PlusTimes(pos, pos + 2), 2) ),
            ( '+', _ , _ )      => Some( (Symbols::Plus(pos, pos + 1), 1) ),
            ( '*', '*', _ )     => Some( (Symbols::TimesTimes(pos, pos + 2), 2) ),
            ( '*', _ , _ )      => Some( (Symbols::Times(pos, pos + 1), 1) ),
            ( '?', '?', _ )     => Some( (Symbols::QuestionMarks(pos, pos + 2), 2) ),
            ( '?', _ , _ )      => Some( (Symbols::QuestionMark(pos, pos + 1), 1) ),
            ( '!', '!', _ )     => Some( (Symbols::ExclaimMarks(pos, pos + 2), 2) ),
            ( '!', _ , _ )      => Some( (Symbols::ExclaimMark(pos, pos + 1), 1) ),
            ( '\\', _ , '"' )   => None, /* Escaped string */
            ( '\\', '"', _  )   => None,
            ( '\\', _ , _ )     => Some( (Symbols::BackSlash(pos, pos + 1), 1) ),
            ( '(', '*', _ )     => None, /* Comment start */
            ( '(', _ , _ )      => Some( (Symbols::LeftParen(pos, pos + 1), 1) ),
            ( ')', _ , _ )      => Some( (Symbols::RightParen(pos, pos + 1), 1) ),
            ( '#', _ , _ )      => Some( (Symbols::NotEqual(pos, pos + 1), 1) ),
            ( '&', _ , _ )      => Some( (Symbols::And(pos, pos + 1), 1) ),
            ( ',', _ , _ )      => Some( (Symbols::Comma(pos, pos + 1), 1) ),
            ( '-', _ , _ )      => Some( (Symbols::Minus(pos, pos + 1), 1) ),
            ( '/', _ , _ )      => Some( (Symbols::Slash(pos, pos + 1), 1) ),
            ( ';', _ , _ )      => Some( (Symbols::Semicolon(pos, pos + 1), 1) ),
            ( '=', _ , _ )      => Some( (Symbols::Equal(pos, pos + 1), 1) ),
            ( '[', _ , _ )      => Some( (Symbols::LeftBracket(pos, pos + 1), 1) ),
            ( ']', _ , _ )      => Some( (Symbols::RightBracket(pos, pos + 1), 1) ),
            ( '^', _ , _ )      => Some( (Symbols::Arrow(pos, pos + 1), 1) ),
            ( '{', _ , _ )      => Some( (Symbols::LeftBrace(pos, pos + 1), 1) ),
            ( '|', _ , _ )      => Some( (Symbols::Bar(pos, pos + 1), 1) ),
            ( '}', _ , _ )      => Some( (Symbols::RightBrace(pos, pos + 1), 1) ),
            ( '~', _ , _ )      => Some( (Symbols::Not(pos, pos + 1), 1) ),
            ( '`', _ , _ )      => Some( (Symbols::Transpose(pos, pos + 1), 1) ),
            _ => None
        }
    }
}