
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
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
    fn is_operator(&self, ch1: char, ch2: char, ch3: char, pos: u32) -> Option<(Symbol, u8)>;
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

    fn is_operator(&self, ch1: char, ch2: char, ch3: char, pos: u32) -> Option<(Symbol, u8)> {
        match (ch1, ch2, ch3) {
            ( '.', '>', '=' )   => Some( (Symbol::DotGreaterEqual(pos, pos + 2), 3) ),
            ( '.', '>', _ )     => Some( (Symbol::DotGreater(pos, pos + 1), 2) ),
            ( '.', '<', '=' )   => Some( (Symbol::DotLessEqual(pos, pos + 2), 3) ),
            ( '.', '<', _ )     => Some( (Symbol::DotLess(pos, pos + 1), 2) ),
            ( '.', '.', _ )     => Some( (Symbol::Upto(pos, pos + 1), 2) ),
            ( '.', '*', _ )     => Some( (Symbol::DotTimes(pos, pos + 1), 2) ),
            ( '.', '/', _ )     => Some( (Symbol::DotSlash(pos, pos + 1), 2) ),
            ( '.', '=', _ )     => Some( (Symbol::DotEqual(pos, pos + 1), 2) ),
            ( '.', '#', _ )     => Some( (Symbol::DotUnequal(pos, pos + 1), 2) ),
            ( '.', _ , _ )      => Some( (Symbol::Period(pos, pos), 1) ),
            ( '<', '=', _ )     => Some( (Symbol::LessEqual(pos, pos + 1), 2) ),
            ( '<', _ , _ )      => Some( (Symbol::Less(pos, pos), 1) ),
            ( '>', '=', _ )     => Some( (Symbol::GreaterEqual(pos, pos + 1), 2) ),
            ( '>', _ , _ )      => Some( (Symbol::Greater(pos, pos), 1) ),
            ( ':', '=', _ )     => Some( (Symbol::Becomes(pos, pos + 1), 2) ),
            ( ':', _ , _ )      => Some( (Symbol::Colon(pos, pos), 1) ),
            ( '+', '*', _ )     => Some( (Symbol::PlusTimes(pos, pos + 1), 2) ),
            ( '+', _ , _ )      => Some( (Symbol::Plus(pos, pos), 1) ),
            ( '*', '*', _ )     => Some( (Symbol::TimesTimes(pos, pos + 1), 2) ),
            ( '*', _ , _ )      => Some( (Symbol::Times(pos, pos), 1) ),
            ( '?', '?', _ )     => Some( (Symbol::QuestionMarks(pos, pos + 1), 2) ),
            ( '?', _ , _ )      => Some( (Symbol::QuestionMark(pos, pos), 1) ),
            ( '!', '!', _ )     => Some( (Symbol::ExclaimMarks(pos, pos + 1), 2) ),
            ( '!', _ , _ )      => Some( (Symbol::ExclaimMark(pos, pos), 1) ),
            ( '\\', _ , '"' )   => None, /* Escaped string */
            ( '\\', '"', _  )   => None,
            ( '\\', _ , _ )     => Some( (Symbol::BackSlash(pos, pos), 1) ),
            ( '(', '*', _ )     => None, /* Comment start */
            ( '(', _ , _ )      => Some( (Symbol::LeftParen(pos, pos), 1) ),
            ( ')', _ , _ )      => Some( (Symbol::RightParen(pos, pos), 1) ),
            ( '#', _ , _ )      => Some( (Symbol::NotEqual(pos, pos), 1) ),
            ( '&', _ , _ )      => Some( (Symbol::And(pos, pos), 1) ),
            ( ',', _ , _ )      => Some( (Symbol::Comma(pos, pos), 1) ),
            ( '-', _ , _ )      => Some( (Symbol::Minus(pos, pos), 1) ),
            ( '/', _ , _ )      => Some( (Symbol::Slash(pos, pos), 1) ),
            ( ';', _ , _ )      => Some( (Symbol::Semicolon(pos, pos), 1) ),
            ( '=', _ , _ )      => Some( (Symbol::Equal(pos, pos), 1) ),
            ( '[', _ , _ )      => Some( (Symbol::LeftBracket(pos, pos), 1) ),
            ( ']', _ , _ )      => Some( (Symbol::RightBracket(pos, pos), 1) ),
            ( '^', _ , _ )      => Some( (Symbol::Arrow(pos, pos), 1) ),
            ( '{', _ , _ )      => Some( (Symbol::LeftBrace(pos, pos), 1) ),
            ( '|', _ , _ )      => Some( (Symbol::Bar(pos, pos), 1) ),
            ( '}', _ , _ )      => Some( (Symbol::RightBrace(pos, pos), 1) ),
            ( '~', _ , _ )      => Some( (Symbol::Not(pos, pos), 1) ),
            ( '`', _ , _ )      => Some( (Symbol::Transpose(pos, pos), 1) ),
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
            "CELLNET" => Some( ( Symbol::Cellnet(pos, pos + 6), 7 ) ),
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
            "IF" => Some( ( Symbol::If(pos, pos + 1), 2 ) ),
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
            "WITH" => Some( ( Symbol::With(pos, pos + 3), 4 ) ),
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

    #[test]
    fn reserved_keyword_const() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("CONST", 0);
        match res {
            Some( ( Symbol::Const(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_case() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("CASE", 0);
        match res {
            Some( ( Symbol::Case(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_cell() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("CELL", 0);
        match res {
            Some( ( Symbol::Cell(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_cellnet() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("CELLNET", 0);
        match res {
            Some( ( Symbol::Cellnet(0, 6), 7 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_code() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("CODE", 0);
        match res {
            Some( ( Symbol::Code(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_definition() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("DEFINITION", 0);
        match res {
            Some( ( Symbol::Definition(0, 9), 10 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_do() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("DO", 0);
        match res {
            Some( ( Symbol::Do(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_div() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("DIV", 0);
        match res {
            Some( ( Symbol::Div(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_end() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("END", 0);
        match res {
            Some( ( Symbol::End(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_enum() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("ENUM", 0);
        match res {
            Some( ( Symbol::Enum(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_else() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("ELSE", 0);
        match res {
            Some( ( Symbol::Else(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_elsif() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("ELSIF", 0);
        match res {
            Some( ( Symbol::Elsif(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_exit() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("EXIT", 0);
        match res {
            Some( ( Symbol::Exit(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_extern() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("EXTERN", 0);
        match res {
            Some( ( Symbol::Extern(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_false() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("FALSE", 0);
        match res {
            Some( ( Symbol::False(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_for() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("FOR", 0);
        match res {
            Some( ( Symbol::For(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_finally() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("FINALLY", 0);
        match res {
            Some( ( Symbol::Finally(0, 6), 7 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_if() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("IF", 0);
        match res {
            Some( ( Symbol::If(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_ignore() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("IGNORE", 0);
        match res {
            Some( ( Symbol::Ignore(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_imag() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("IMAG", 0);
        match res {
            Some( ( Symbol::Imag(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_in() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("IN", 0);
        match res {
            Some( ( Symbol::In(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_is() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("IS", 0);
        match res {
            Some( ( Symbol::Is(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_import() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("IMPORT", 0);
        match res {
            Some( ( Symbol::Import(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_loop() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("LOOP", 0);
        match res {
            Some( ( Symbol::Loop(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_module() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("MODULE", 0);
        match res {
            Some( ( Symbol::Module(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_mod() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("MOD", 0);
        match res {
            Some( ( Symbol::Mod(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_nil() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("NIL", 0);
        match res {
            Some( ( Symbol::Nil(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_of() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("OF", 0);
        match res {
            Some( ( Symbol::Of(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_or() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("OR", 0);
        match res {
            Some( ( Symbol::Or(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_out() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("OUT", 0);
        match res {
            Some( ( Symbol::Out(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_operator() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("OPERATOR", 0);
        match res {
            Some( ( Symbol::Operator(0, 7), 8 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_procedure() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("PROCEDURE", 0);
        match res {
            Some( ( Symbol::Procedure(0, 8), 9 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_port() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("PORT", 0);
        match res {
            Some( ( Symbol::Port(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_repeat() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("REPEAT", 0);
        match res {
            Some( ( Symbol::Repeat(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_return() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("RETURN", 0);
        match res {
            Some( ( Symbol::Return(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_self() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("SELF", 0);
        match res {
            Some( ( Symbol::Self_(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_new() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("NEW", 0);
        match res {
            Some( ( Symbol::New(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_result() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("RESULT", 0);
        match res {
            Some( ( Symbol::Result(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_then() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("THEN", 0);
        match res {
            Some( ( Symbol::Then(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_true() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("TRUE", 0);
        match res {
            Some( ( Symbol::True(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_to() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("TO", 0);
        match res {
            Some( ( Symbol::To(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_type() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("TYPE", 0);
        match res {
            Some( ( Symbol::Type(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_until() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("UNTIL", 0);
        match res {
            Some( ( Symbol::Until(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_var() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("VAR", 0);
        match res {
            Some( ( Symbol::Var(0, 2), 3) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_while() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("WHILE", 0);
        match res {
            Some( ( Symbol::While(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_with() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("WITH", 0);
        match res {
            Some( ( Symbol::With(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_any() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("ANY", 0);
        match res {
            Some( ( Symbol::Any(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_array() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("ARRAY", 0);
        match res {
            Some( ( Symbol::Array(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_object() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("OBJECT", 0);
        match res {
            Some( ( Symbol::Object(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_pointer() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("POINTER", 0);
        match res {
            Some( ( Symbol::Pointer(0, 6), 7 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_record() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("RECORD", 0);
        match res {
            Some( ( Symbol::Record(0, 5), 6 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_address() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("ADDRESS", 0);
        match res {
            Some( ( Symbol::Address(0, 6), 7 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_size() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("SIZE", 0);
        match res {
            Some( ( Symbol::Size(0, 3), 4 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_alias() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_reserved_keyword("ALIAS", 0);
        match res {
            Some( ( Symbol::Alias(0, 4), 5 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_greater_equal() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '>', '=', 0);
        match res {
            Some( ( Symbol::DotGreaterEqual(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_greater() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '>', ' ', 0);
        match res {
            Some( ( Symbol::DotGreater(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_less_equal() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '<', '=', 0);
        match res {
            Some( ( Symbol::DotLessEqual(0, 2), 3 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_less() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '<', ' ', 0);
        match res {
            Some( ( Symbol::DotLess(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_upto() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '.', ' ', 0);
        match res {
            Some( ( Symbol::Upto(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_star() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '*', ' ', 0);
        match res {
            Some( ( Symbol::DotTimes(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_slash() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '/', ' ', 0);
        match res {
            Some( ( Symbol::DotSlash(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_equal() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '=', ' ', 0);
        match res {
            Some( ( Symbol::DotEqual(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_unequal() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', '#', ' ', 0);
        match res {
            Some( ( Symbol::DotUnequal(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_dot_period() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('.', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Period(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_less_equal() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('<', '=', ' ', 0);
        match res {
            Some( ( Symbol::LessEqual(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_less() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('<', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Less(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_greater_equal() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('>', '=', ' ', 0);
        match res {
            Some( ( Symbol::GreaterEqual(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_greater() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('>', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Greater(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_becomes() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator(':', '=', ' ', 0);
        match res {
            Some( ( Symbol::Becomes(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_colon() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator(':', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Colon(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_plus_times() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('+', '*', ' ', 0);
        match res {
            Some( ( Symbol::PlusTimes(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_plus() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('+', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Plus(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_times_times() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('*', '*', ' ', 0);
        match res {
            Some( ( Symbol::TimesTimes(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_times() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('*', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Times(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_question_marks() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('?', '?', ' ', 0);
        match res {
            Some( ( Symbol::QuestionMarks(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_question_mark() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('?', ' ', ' ', 0);
        match res {
            Some( ( Symbol::QuestionMark(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_exclaim_marks() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('!', '!', ' ', 0);
        match res {
            Some( ( Symbol::ExclaimMarks(0, 1), 2 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_exclaim_mark() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('!', ' ', ' ', 0);
        match res {
            Some( ( Symbol::ExclaimMark(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_back_slash() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('\\', ' ', ' ', 0);
        match res {
            Some( ( Symbol::BackSlash(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_left_paren() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('(', ' ', ' ', 0);
        match res {
            Some( ( Symbol::LeftParen(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_right_paren() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator(')', ' ', ' ', 0);
        match res {
            Some( ( Symbol::RightParen(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_left_bracket() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('[', ' ', ' ', 0);
        match res {
            Some( ( Symbol::LeftBracket(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_right_bracket() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator(']', ' ', ' ', 0);
        match res {
            Some( ( Symbol::RightBracket(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_left_brace() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('{', ' ', ' ', 0);
        match res {
            Some( ( Symbol::LeftBrace(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_right_curly() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('}', ' ', ' ', 0);
        match res {
            Some( ( Symbol::RightBrace(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_arrow() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('^', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Arrow(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_bar() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('|', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Bar(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_not() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('~', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Not(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn operator_transpose() {
        let scanner = ActiveOberonScanner::new();
        let res = scanner.is_operator('`', ' ', ' ', 0);
        match res {
            Some( ( Symbol::Transpose(0, 0), 1 ) ) => {
                assert!(true)
            },
            _ => assert!(false)
        }
    }
}