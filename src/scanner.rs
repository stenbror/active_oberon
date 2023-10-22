
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Symbol {
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
    Exit(u32, u32),
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
    fn new() -> Self;
    fn is_operator(ch1: char, ch2: char, ch3: char, pos: u32) -> Option<(Symbol, u8)>;
    fn is_reserved_keyword(&self, text: &str, pos: u32) -> Option<(Symbol, u8)>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ActiveOberonScanner {

}

impl ActiveOberonScannerMethods for ActiveOberonScanner {
    fn new() -> Self {
        ActiveOberonScanner {

        }
    }

    fn is_operator(ch1: char, ch2: char, ch3: char, pos: u32) -> Option<(Symbol, u8)> {
        match (ch1, ch2, ch3) {
            ( '.', '>', '=' )   => Some( (Symbol::DotGreaterEqual(pos, pos + 3), 3) ),
            ( '.', '>', _ )     => Some( (Symbol::DotGreater(pos, pos + 2), 2) ),
            ( '.', '<', '=' )   => Some( (Symbol::DotLessEqual(pos, pos + 3), 3) ),
            ( '.', '<', _ )     => Some( (Symbol::DotLess(pos, pos + 2), 2) ),
            ( '.', '.', _ )     => Some( (Symbol::Upto(pos, pos + 2), 2) ),
            ( '.', '*', _ )     => Some( (Symbol::DotTimes(pos, pos + 2), 2) ),
            ( '.', '/', _ )     => Some( (Symbol::DotSlash(pos, pos + 2), 2) ),
            ( '.', '=', _ )     => Some( (Symbol::DotEqual(pos, pos + 2), 2) ),
            ( '.', '#', _ )     => Some( (Symbol::DotUnequal(pos, pos + 2), 2) ),
            ( '.', _ , _ )      => Some( (Symbol::Period(pos, pos + 1), 1) ),
            ( '<', '=', _ )     => Some( (Symbol::LessEqual(pos, pos + 2), 2) ),
            ( '<', _ , _ )      => Some( (Symbol::Less(pos, pos + 1), 1) ),
            ( '>', '=', _ )     => Some( (Symbol::GreaterEqual(pos, pos + 2), 2) ),
            ( '>', _ , _ )      => Some( (Symbol::Greater(pos, pos + 1), 1) ),
            ( ':', '=', _ )     => Some( (Symbol::Becomes(pos, pos + 2), 2) ),
            ( ':', _ , _ )      => Some( (Symbol::Colon(pos, pos + 1), 1) ),
            ( '+', '*', _ )     => Some( (Symbol::PlusTimes(pos, pos + 2), 2) ),
            ( '+', _ , _ )      => Some( (Symbol::Plus(pos, pos + 1), 1) ),
            ( '*', '*', _ )     => Some( (Symbol::TimesTimes(pos, pos + 2), 2) ),
            ( '*', _ , _ )      => Some( (Symbol::Times(pos, pos + 1), 1) ),
            ( '?', '?', _ )     => Some( (Symbol::QuestionMarks(pos, pos + 2), 2) ),
            ( '?', _ , _ )      => Some( (Symbol::QuestionMark(pos, pos + 1), 1) ),
            ( '!', '!', _ )     => Some( (Symbol::ExclaimMarks(pos, pos + 2), 2) ),
            ( '!', _ , _ )      => Some( (Symbol::ExclaimMark(pos, pos + 1), 1) ),
            ( '\\', _ , '"' )   => None, /* Escaped string */
            ( '\\', '"', _  )   => None,
            ( '\\', _ , _ )     => Some( (Symbol::BackSlash(pos, pos + 1), 1) ),
            ( '(', '*', _ )     => None, /* Comment start */
            ( '(', _ , _ )      => Some( (Symbol::LeftParen(pos, pos + 1), 1) ),
            ( ')', _ , _ )      => Some( (Symbol::RightParen(pos, pos + 1), 1) ),
            ( '#', _ , _ )      => Some( (Symbol::NotEqual(pos, pos + 1), 1) ),
            ( '&', _ , _ )      => Some( (Symbol::And(pos, pos + 1), 1) ),
            ( ',', _ , _ )      => Some( (Symbol::Comma(pos, pos + 1), 1) ),
            ( '-', _ , _ )      => Some( (Symbol::Minus(pos, pos + 1), 1) ),
            ( '/', _ , _ )      => Some( (Symbol::Slash(pos, pos + 1), 1) ),
            ( ';', _ , _ )      => Some( (Symbol::Semicolon(pos, pos + 1), 1) ),
            ( '=', _ , _ )      => Some( (Symbol::Equal(pos, pos + 1), 1) ),
            ( '[', _ , _ )      => Some( (Symbol::LeftBracket(pos, pos + 1), 1) ),
            ( ']', _ , _ )      => Some( (Symbol::RightBracket(pos, pos + 1), 1) ),
            ( '^', _ , _ )      => Some( (Symbol::Arrow(pos, pos + 1), 1) ),
            ( '{', _ , _ )      => Some( (Symbol::LeftBrace(pos, pos + 1), 1) ),
            ( '|', _ , _ )      => Some( (Symbol::Bar(pos, pos + 1), 1) ),
            ( '}', _ , _ )      => Some( (Symbol::RightBrace(pos, pos + 1), 1) ),
            ( '~', _ , _ )      => Some( (Symbol::Not(pos, pos + 1), 1) ),
            ( '`', _ , _ )      => Some( (Symbol::Transpose(pos, pos + 1), 1) ),
            _ => None
        }
    }

    fn is_reserved_keyword(&self, text: &str, pos: u32) -> Option<(Symbol, u8)> {
        match text {
            "AWAIT" => Some( ( Symbol::Await(pos, pos + 4), 5 ) ),
            "BEGIN" => Some( ( Symbol::Begin(pos, pos + 4), 5 ) ),
            "BY" => Some( ( Symbol::By(pos, pos + 1), 2 ) ),
            "CONST" => Some( ( Symbol::Const(pos, pos + 4), 5 ) ),
            "CASE" => Some( ( Symbol::Case(pos, pos + 3), 4 ) ),
            "CELL" => Some( ( Symbol::Cell(pos, pos + 3), 4 ) ),
            "CELLNET" => Some( ( Symbol::Cellnet(pos, pos + 6), 5 ) ),
            "CODE" => Some( ( Symbol::Code(pos, pos + 3), 4 ) ),
            "DEFINITION" => Some( ( Symbol::Definition(pos, pos + 9), 10 ) ),
            "DO" => Some( ( Symbol::Do(pos, pos + 1), 2 ) ),
            "DIV" => Some( ( Symbol::Div(pos, pos + 2), 3 ) ),
            "END" => Some( ( Symbol::End(pos, pos + 2), 3 ) ),
            "ENUM" => Some( ( Symbol::Enum(pos, pos + 3), 4 ) ),
            "ELSE" => Some( ( Symbol::Else(pos, pos + 3), 4 ) ),
            "ELSIF" => Some( ( Symbol::Elsif(pos, pos + 4), 5 ) ),
            "EXIT" => Some( ( Symbol::Exit(pos, pos + 3), 4 ) ),
            "EXTERN" => Some( ( Symbol::Extern(pos, pos + 5), 6 ) ),
            "FALSE" => Some( ( Symbol::False(pos, pos + 4), 5 ) ),
            "FOR" => Some( ( Symbol::For(pos, pos + 2), 3 ) ),
            "FINALLY" => Some( ( Symbol::Finally(pos, pos + 6), 7 ) ),
            "If" => Some( ( Symbol::If(pos, pos + 1), 2 ) ),
            "IGNORE" => Some( ( Symbol::Ignore(pos, pos + 5), 6 ) ),
            "IMAG" => Some( ( Symbol::Imag(pos, pos + 3), 4 ) ),
            "IN" => Some( ( Symbol::In(pos, pos + 1), 2 ) ),
            "IS" => Some( ( Symbol::Is(pos, pos + 1), 2 ) ),
            "IMPORT" => Some( ( Symbol::Import(pos, pos + 5), 6 ) ),
            "LOOP" => Some( ( Symbol::Loop(pos, pos + 3), 4 ) ),
            "MODULE" => Some( ( Symbol::Module(pos, pos + 5), 6 ) ),
            "MOD" => Some( ( Symbol::Mod(pos, pos + 2), 3 ) ),
            "NIL" => Some( ( Symbol::Nil(pos, pos + 2), 3 ) ),
            "OF" => Some( ( Symbol::Of(pos, pos + 1), 2 ) ),
            "OR" => Some( ( Symbol::Or(pos, pos + 1), 2 ) ),
            "OUT" => Some( ( Symbol::Out(pos, pos + 2), 3 ) ),
            "OPERATOR" => Some( ( Symbol::Operator(pos, pos + 7), 8 ) ),
            "PROCEDURE" => Some( ( Symbol::Procedure(pos, pos + 8), 9 ) ),
            "PORT" => Some( ( Symbol::Port(pos, pos + 3), 4 ) ),
            "REPEAT" => Some( ( Symbol::Repeat(pos, pos + 5), 6 ) ),
            "RETURN" => Some( ( Symbol::Return(pos, pos + 5), 6 ) ),
            "SELF" => Some( ( Symbol::Self_(pos, pos + 3), 4 ) ),
            "NEW" => Some( ( Symbol::New(pos, pos + 2), 3 ) ),
            "RESULT" => Some( ( Symbol::Result(pos, pos + 5), 6 ) ),
            "THEN" => Some( ( Symbol::Then(pos, pos + 3), 4 ) ),
            "TRUE" => Some( ( Symbol::True(pos, pos + 3), 4 ) ),
            "TO" => Some( ( Symbol::To(pos, pos + 1), 2 ) ),
            "TYPE" => Some( ( Symbol::Type(pos, pos + 3), 4 ) ),
            "UNTIL" => Some( ( Symbol::Until(pos, pos + 4), 5 ) ),
            "VAR" => Some( ( Symbol::Var(pos, pos + 2), 3 ) ),
            "WHILE" => Some( ( Symbol::While(pos, pos + 4), 5 ) ),
            "With" => Some( ( Symbol::With(pos, pos + 3), 4 ) ),
            "ANY" => Some( ( Symbol::Any(pos, pos + 2), 3 ) ),
            "ARRAY" => Some( ( Symbol::Array(pos, pos + 4), 5 ) ),
            "OBJECT" => Some( ( Symbol::Object(pos, pos + 5), 6 ) ),
            "POINTER" => Some( ( Symbol::Pointer(pos, pos + 6), 7 ) ),
            "RECORD" => Some( ( Symbol::Record(pos, pos + 5), 6 ) ),
            "ADDRESS" => Some( ( Symbol::Address(pos, pos + 6), 7 ) ),
            "SIZE" => Some( ( Symbol::Size(pos, pos + 3), 4 ) ),
            "ALIAS" => Some( ( Symbol::Alias(pos, pos + 4), 5 ) ),
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reserved_keyword_await() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("AWAIT", 0);
        match res {
            Some( ( Symbol::Await(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_begin() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("BEGIN", 0);
        match res {
            Some( ( Symbol::Begin(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_by() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("BY", 0);
        match res {
            Some( ( Symbol::By(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }
}