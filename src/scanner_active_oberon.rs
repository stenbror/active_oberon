

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
    fn new(text: &'static str) -> Self;
    fn get_next_symbol(&mut self) -> Result<Symbol, String>;
    fn is_reserved_keyword_or_literal(&self, buf: Vec<char>, line: u32, col: u32) -> Result<Symbol, String>;
    fn get_char(&mut self) -> (char, u32, u32);
    fn peek_char(&self) -> char;

}

pub struct ActiveOberonScanner {
    buffer: Vec<char>,
    position: u32,
    line: u32,
    col: u32
}

impl ActiveOberonScannerMethods for ActiveOberonScanner {
    fn new(text: &'static str) -> Self {
        ActiveOberonScanner {
            buffer: text.chars().collect(),
            position: 0,
            line: 1,
            col: 1
        }
    }

    /// This is the main entry for parser to get next valid symbol (Token) with line and col position.
    fn get_next_symbol(&mut self) -> Result<Symbol, String> {
        let (mut ch, mut line, mut col) = self.get_char();

        loop {
            match ch {
                ' ' | '\t' | '\n' => (ch, line, col) = self.get_char(),
                _ => break
            }
        }

        match ch {
            '\0' => Ok(Symbol::EOF),
            '#' => Ok(Symbol::NotEqual(line, col)),
            '&' => Ok(Symbol::And(line, col)),
            '(' => {
                let mut ch2 = self.peek_char();
                match ch2 {
                    '*' => {
                        let mut level = 1;
                        ( _ , _ , _ ) = self.get_char();
                        loop {
                            ch2 = self.peek_char();
                            match ch2 {
                                '*' => {
                                    ( _ , _ , _ ) = self.get_char();
                                    ch2 = self.peek_char();
                                    match ch2 {
                                        ')' => {
                                            level = level - 1;
                                            ( _ , _ , _ ) = self.get_char();
                                            if level <= 0 { break }
                                            ( _ , _ , _ ) = self.get_char()
                                        },
                                        _ => ( _ , _ , _ ) = self.get_char()
                                    }
                                },
                                '(' => {
                                    ch2 = self.peek_char();
                                    match ch2 {
                                        '*' => {
                                            ( _ , _ , _ ) = self.get_char();
                                            level = level + 1
                                        },
                                        _ => ( _ , _ , _ ) = self.get_char()
                                    }
                                },
                                '\0' => return Err(format!("Lexical Error => Line: {}, Col: {} - Unterminated comment!", line, col )),
                                _ => ( _ , _ , _ ) = self.get_char()
                            }
                        }
                        self.get_next_symbol()
                    },
                    _ => Ok(Symbol::LeftParen(line, col))
                }
            },
            ')' => Ok(Symbol::RightParen(line, col)),
            '*' => {
                let ch2 = self.peek_char();
                match ch2 {
                    '*' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::TimesTimes(line, col))
                    },
                    _ => Ok(Symbol::Times(line, col))
                }
            },
            '+' => {
                let ch2 = self.peek_char();
                match ch2 {
                    '*' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::PlusTimes(line, col))
                    },
                    _ => Ok(Symbol::Plus(line, col))
                }
            },
            ',' => Ok(Symbol::Comma(line, col)),
            '-' => Ok(Symbol::Minus(line, col)),
            '.' => {
                let ch2 = self.peek_char();
                match ch2 {
                    '.' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::Upto(line, col))
                    },
                    '*' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::DotTimes(line, col))
                    },
                    '/' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::DotSlash(line, col))
                    },
                    '=' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::DotEqual(line, col))
                    },
                    '#' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::DotUnequal(line, col))
                    },
                    '>' => {
                        ( _ , _ , _ ) = self.get_char();
                        let ch3 = self.peek_char();
                        match ch3 {
                            '=' => {
                                ( _ , _ , _ ) = self.get_char();
                                Ok(Symbol::DotGreaterEqual(line, col))
                            },
                            _ => Ok(Symbol::DotGreater(line, col))
                        }
                    },
                    '<' => {
                        ( _ , _ , _ ) = self.get_char();
                        let ch3 = self.peek_char();
                        match ch3 {
                            '=' => {
                                ( _ , _ , _ ) = self.get_char();
                                Ok(Symbol::DotLessEqual(line, col))
                            },
                            _ => Ok(Symbol::DotLess(line, col))
                        }
                    },
                    _ => Ok(Symbol::Period(line, col))
                }
            },
            '/' => Ok(Symbol::Slash(line, col)),
            ':' => {
                let ch2 = self.peek_char();
                match ch2 {
                    '=' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::Becomes(line, col))
                    },
                    _ => Ok(Symbol::Colon(line, col))
                }
            }
            ';' => Ok(Symbol::Semicolon(line, col)),
            '<' => {
                let ch2 = self.peek_char();
                match ch2 {
                    '=' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::LessEqual(line, col))
                    },
                    '<' => {
                        let ch3 = self.peek_char();
                        match ch3 {
                            '?' => {
                                ( _ , _ , _ ) = self.get_char();
                                Ok(Symbol::LessLessQ(line, col))
                            },
                            _ => Ok(Symbol::LessLess(line, col))
                        }
                    },
                    _ => Ok(Symbol::Less(line, col))
                }
            },
            '=' => Ok(Symbol::Equal(line, col)),
            '>' => {
                let ch2 = self.peek_char();
                match ch2 {
                    '=' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::GreaterEqual(line, col))
                    },
                    '>' => {
                        let ch3 = self.peek_char();
                        match ch3 {
                            '?' => {
                                ( _ , _ , _ ) = self.get_char();
                                Ok(Symbol::GreaterGreaterQ(line, col))
                            },
                            _ => Ok(Symbol::GreaterGreater(line, col))
                        }
                    },
                    _ => Ok(Symbol::Greater(line, col))
                }
            },
            '[' => Ok(Symbol::LeftBracket(line, col)),
            ']' => Ok(Symbol::RightBracket(line, col)),
            '{' => Ok(Symbol::LeftBrace(line, col)),
            '|' => Ok(Symbol::Bar(line, col)),
            '}' => Ok(Symbol::LeftBracket(line, col)),
            '`' => Ok(Symbol::Transpose(line, col)),
            '?' => {
                let ch2 = self.peek_char();
                match ch2 {
                    '?' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::QuestionMarks(line, col))
                    },
                    _ => Ok(Symbol::QuestionMark(line, col))
                }
            },
            '!' => {
                let ch2 = self.peek_char();
                match ch2 {
                    '!' => {
                        ( _ , _ , _ ) = self.get_char();
                        Ok(Symbol::ExclaimMarks(line, col))
                    },
                    _ => Ok(Symbol::ExclaimMark(line, col))
                }
            },
            'A'..='Z' => {
                let mut text : Vec<char> = Vec::new();
                text.push(ch);
                loop {
                    match self.peek_char() {
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                            ( ch, _ , _ ) = self.get_char();
                            text.push(ch)
                        },
                        _ => break
                    }
                }
                self.is_reserved_keyword_or_literal(text, line, col)
            },
            'a'..='z' | '_' => {
                let mut text : Vec<char> = Vec::new();
                text.push(ch);
                loop {
                    match self.peek_char() {
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                            ( ch, _ , _ ) = self.get_char();
                            text.push(ch)
                        },
                        _ => break
                    }
                }
                Ok(Symbol::Ident(line, col, String::from_iter(text)))
            },
            _ => Err(format!("Lexical Error => Line: {}, Col: {} - Unknown character '{}' found in source code!", line, col, ch ))
        }
    }

    fn is_reserved_keyword_or_literal(&self, buf: Vec<char>, line: u32, col: u32) -> Result<Symbol, String> {
        let text = String::from_iter(buf);
        match text.as_str() {
            "AWAIT" => Ok(Symbol::Await(line, col)),
            "BEGIN" => Ok(Symbol::Begin(line, col)),
            "BY" => Ok(Symbol::By(line, col)),
            "CONST" => Ok(Symbol::Const(line, col)),
            "CASE" => Ok(Symbol::Case(line, col)),
            "CELL" => Ok(Symbol::Cell(line, col)),
            "CELLNET" => Ok(Symbol::Cellnet(line, col)),
            "CODE" => Ok(Symbol::Code(line, col)),
            "DEFINITION" => Ok(Symbol::Definition(line, col)),
            "DO" => Ok(Symbol::Do(line, col)),
            "DIV" => Ok(Symbol::Div(line, col)),
            "END" => Ok(Symbol::End(line, col)),
            "ENUM" => Ok(Symbol::Enum(line, col)),
            "ELSE" => Ok(Symbol::Else(line, col)),
            "ELSIF" => Ok(Symbol::Elsif(line, col)),
            "EXIT" => Ok(Symbol::Exit(line, col)),
            "EXTERN" => Ok(Symbol::Extern(line, col)),
            "FALSE" => Ok(Symbol::False(line, col)),
            "FOR" => Ok(Symbol::For(line, col)),
            "FINALLY" => Ok(Symbol::Finally(line, col)),
            "IF" => Ok(Symbol::If(line, col)),
            "IGNORE" => Ok(Symbol::Ignore(line, col)),
            "IMAG" => Ok(Symbol::Imag(line, col)),
            "IN" => Ok(Symbol::In(line, col)),
            "IS" => Ok(Symbol::Is(line, col)),
            "IMPORT" => Ok(Symbol::Import(line, col)),
            "LOOP" => Ok(Symbol::Loop(line, col)),
            "MODULE" => Ok(Symbol::Module(line, col)),
            "MOD" => Ok(Symbol::Mod(line, col)),
            "NIL" => Ok(Symbol::Nil(line, col)),
            "OF" => Ok(Symbol::Of(line, col)),
            "OR" => Ok(Symbol::Or(line, col)),
            "OUT" => Ok(Symbol::Out(line, col)),
            "OPERATOR" => Ok(Symbol::Operator(line, col)),
            "PROCEDURE" => Ok(Symbol::Procedure(line, col)),
            "PORT" => Ok(Symbol::Port(line, col)),
            "REPEAT" => Ok(Symbol::Repeat(line, col)),
            "RETURN" => Ok(Symbol::Return(line, col)),
            "SELF" => Ok(Symbol::Self_(line, col)),
            "NEW" => Ok(Symbol::New(line, col)),
            "RESULT" => Ok(Symbol::Result(line, col)),
            "THEN" => Ok(Symbol::Then(line, col)),
            "TRUE" => Ok(Symbol::True(line, col)),
            "TO" => Ok(Symbol::To(line, col)),
            "TYPE" => Ok(Symbol::Type(line, col)),
            "UNTIL" => Ok(Symbol::Until(line, col)),
            "VAR" => Ok(Symbol::Var(line, col)),
            "WHILE" => Ok(Symbol::While(line, col)),
            "WITH" => Ok(Symbol::With(line, col)),

            "ANY" => Ok(Symbol::Any(line, col)),
            "ARRAY" => Ok(Symbol::Array(line, col)),
            "OBJECT" => Ok(Symbol::Object(line, col)),
            "POINTER" => Ok(Symbol::Pointer(line, col)),
            "RECORD" => Ok(Symbol::Record(line, col)),
            "ADDRESS" => Ok(Symbol::Address(line, col)),
            "SIZE" => Ok(Symbol::Size(line, col)),
            "ALIAS" => Ok(Symbol::Alias(line, col)),

            _ => Ok(Symbol::Ident(line, col, text))
        }
    }

    fn get_char(&mut self) -> (char, u32, u32) {
        let mut res = '\0';
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                match x {
                    '\r' => {
                        self.position = self.position + 1;
                        match self.buffer.get(self.position as usize) {
                            Some('\n') => {
                                self.position = self.position + 1;
                                res = '\n'
                            },
                            _ => res = '\n'
                        }
                        self.line = self.line + 1;
                        self.col = 1;
                    },
                    '\n' => {
                        self.position = self.position + 1;
                        self.line = self.line + 1;
                        self.col = 1;
                        res = '\n'
                    },
                    _ => {
                        self.position = self.position + 1;
                        res = x.clone();
                        self.col = self.col + 1
                    }
                }

            },
            _ => ()
        }
        (res, self.line, self.col - 1)
    }

    fn peek_char(&self) -> char {
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                return x.clone()
            },
            _ => '\0'
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scanner_operator_unequal() {
        let mut scanner = ActiveOberonScanner::new("#");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::NotEqual(1,1)))
    }

    #[test]
    fn scanner_operator_and() {
        let mut scanner = ActiveOberonScanner::new("&");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::And(1,1)))
    }

    #[test]
    fn scanner_operator_left_paren() {
        let mut scanner = ActiveOberonScanner::new("(");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::LeftParen(1,1)))
    }

    #[test]
    fn scanner_operator_right_paren() {
        let mut scanner = ActiveOberonScanner::new(")");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::RightParen(1,1)))
    }

    #[test]
    fn scanner_operator_times() {
        let mut scanner = ActiveOberonScanner::new("*");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Times(1,1)))
    }

    #[test]
    fn scanner_operator_times_times() {
        let mut scanner = ActiveOberonScanner::new("**");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::TimesTimes(1,1)))
    }

    #[test]
    fn scanner_operator_plus() {
        let mut scanner = ActiveOberonScanner::new("+");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Plus(1,1)))
    }

    #[test]
    fn scanner_operator_plus_times() {
        let mut scanner = ActiveOberonScanner::new("+*");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::PlusTimes(1,1)))
    }

    #[test]
    fn scanner_operator_comma() {
        let mut scanner = ActiveOberonScanner::new(",");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Comma(1,1)))
    }

    #[test]
    fn scanner_operator_minus() {
        let mut scanner = ActiveOberonScanner::new("-");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Minus(1,1)))
    }

    #[test]
    fn scanner_operator_period() {
        let mut scanner = ActiveOberonScanner::new(".");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Period(1,1)))
    }

    #[test]
    fn scanner_operator_upto() {
        let mut scanner = ActiveOberonScanner::new("..");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Upto(1,1)))
    }

    #[test]
    fn scanner_operator_dot_times() {
        let mut scanner = ActiveOberonScanner::new(".*");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::DotTimes(1,1)))
    }



    #[test]
    fn scanner_comment_single() {
        let mut scanner = ActiveOberonScanner::new("(* This is a single comment *)");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::EOF))
    }





    #[test]
    fn scanner_keyword_await() {
        let mut scanner = ActiveOberonScanner::new("AWAIT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Await(1,1)))
    }

    #[test]
    fn scanner_keyword_await_whitespace() {
        let mut scanner = ActiveOberonScanner::new("  AWAIT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Await(1,3)))
    }

    #[test]
    fn scanner_indent_await__whitespace() {
        let mut scanner = ActiveOberonScanner::new("  AWAIT_");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Ident(1,3, "AWAIT_".to_string())))
    }

    #[test]
    fn scanner_indent_lowercase() {
        let mut scanner = ActiveOberonScanner::new("  __init__");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Ident(1,3, "__init__".to_string())))
    }

    #[test]
    fn scanner_indent_lowercase_non_special_character() {
        let mut scanner = ActiveOberonScanner::new("  to_Test123");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Ident(1,3, "to_Test123".to_string())))
    }

    #[test]
    fn scanner_keyword_begin() {
        let mut scanner = ActiveOberonScanner::new("BEGIN");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Begin(1,1)))
    }

    #[test]
    fn scanner_keyword_by() {
        let mut scanner = ActiveOberonScanner::new("BY");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::By(1,1)))
    }

    #[test]
    fn scanner_keyword_const() {
        let mut scanner = ActiveOberonScanner::new("CONST");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Const(1,1)))
    }

    #[test]
    fn scanner_keyword_case() {
        let mut scanner = ActiveOberonScanner::new("CASE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Case(1,1)))
    }

    #[test]
    fn scanner_keyword_cell() {
        let mut scanner = ActiveOberonScanner::new("CELL");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Cell(1,1)))
    }

    #[test]
    fn scanner_keyword_cellnet() {
        let mut scanner = ActiveOberonScanner::new("CELLNET");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Cellnet(1,1)))
    }

    #[test]
    fn scanner_keyword_code() {
        let mut scanner = ActiveOberonScanner::new("CODE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Code(1,1)))
    }

    #[test]
    fn scanner_keyword_definition() {
        let mut scanner = ActiveOberonScanner::new("DEFINITION");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Definition(1,1)))
    }

    #[test]
    fn scanner_keyword_do() {
        let mut scanner = ActiveOberonScanner::new("DO");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Do(1,1)))
    }

    #[test]
    fn scanner_keyword_div() {
        let mut scanner = ActiveOberonScanner::new("DIV");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Div(1,1)))
    }

    #[test]
    fn scanner_keyword_end() {
        let mut scanner = ActiveOberonScanner::new("END");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::End(1,1)))
    }

    #[test]
    fn scanner_keyword_enum() {
        let mut scanner = ActiveOberonScanner::new("ENUM");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Enum(1,1)))
    }

    #[test]
    fn scanner_keyword_else() {
        let mut scanner = ActiveOberonScanner::new("ELSE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Else(1,1)))
    }

    #[test]
    fn scanner_keyword_elsif() {
        let mut scanner = ActiveOberonScanner::new("ELSIF");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Elsif(1,1)))
    }

    #[test]
    fn scanner_keyword_exit() {
        let mut scanner = ActiveOberonScanner::new("EXIT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Exit(1,1)))
    }

    #[test]
    fn scanner_keyword_extern() {
        let mut scanner = ActiveOberonScanner::new("EXTERN");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Extern(1,1)))
    }

    #[test]
    fn scanner_keyword_false() {
        let mut scanner = ActiveOberonScanner::new("FALSE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::False(1,1)))
    }

    #[test]
    fn scanner_keyword_for() {
        let mut scanner = ActiveOberonScanner::new("FOR");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::For(1,1)))
    }

    #[test]
    fn scanner_keyword_finally() {
        let mut scanner = ActiveOberonScanner::new("FINALLY");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Finally(1,1)))
    }

    #[test]
    fn scanner_keyword_if() {
        let mut scanner = ActiveOberonScanner::new("IF");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::If(1,1)))
    }

    #[test]
    fn scanner_keyword_ignore() {
        let mut scanner = ActiveOberonScanner::new("IGNORE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Ignore(1,1)))
    }

    #[test]
    fn scanner_keyword_imag() {
        let mut scanner = ActiveOberonScanner::new("IMAG");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Imag(1,1)))
    }

    #[test]
    fn scanner_keyword_in() {
        let mut scanner = ActiveOberonScanner::new("IN");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::In(1,1)))
    }

    #[test]
    fn scanner_keyword_is() {
        let mut scanner = ActiveOberonScanner::new("IS");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Is(1,1)))
    }

    #[test]
    fn scanner_keyword_import() {
        let mut scanner = ActiveOberonScanner::new("IMPORT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Import(1,1)))
    }

    #[test]
    fn scanner_keyword_loop() {
        let mut scanner = ActiveOberonScanner::new("LOOP");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Loop(1,1)))
    }

    #[test]
    fn scanner_keyword_module() {
        let mut scanner = ActiveOberonScanner::new("MODULE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Module(1,1)))
    }

    #[test]
    fn scanner_keyword_mod() {
        let mut scanner = ActiveOberonScanner::new("MOD");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Mod(1,1)))
    }

    #[test]
    fn scanner_keyword_nil() {
        let mut scanner = ActiveOberonScanner::new("NIL");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Nil(1,1)))
    }

    #[test]
    fn scanner_keyword_of() {
        let mut scanner = ActiveOberonScanner::new("OF");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Of(1,1)))
    }

    #[test]
    fn scanner_keyword_or() {
        let mut scanner = ActiveOberonScanner::new("OR");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Or(1,1)))
    }

    #[test]
    fn scanner_keyword_out() {
        let mut scanner = ActiveOberonScanner::new("OUT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Out(1,1)))
    }

    #[test]
    fn scanner_keyword_operator() {
        let mut scanner = ActiveOberonScanner::new("OPERATOR");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Operator(1,1)))
    }

    #[test]
    fn scanner_keyword_procedure() {
        let mut scanner = ActiveOberonScanner::new("PROCEDURE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Procedure(1,1)))
    }

    #[test]
    fn scanner_keyword_port() {
        let mut scanner = ActiveOberonScanner::new("PORT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Port(1,1)))
    }

    #[test]
    fn scanner_keyword_repeat() {
        let mut scanner = ActiveOberonScanner::new("REPEAT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Repeat(1,1)))
    }

    #[test]
    fn scanner_keyword_return() {
        let mut scanner = ActiveOberonScanner::new("RETURN");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Return(1,1)))
    }

    #[test]
    fn scanner_keyword_self() {
        let mut scanner = ActiveOberonScanner::new("SELF");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Self_(1,1)))
    }

    #[test]
    fn scanner_keyword_new() {
        let mut scanner = ActiveOberonScanner::new("NEW");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::New(1,1)))
    }

    #[test]
    fn scanner_keyword_result() {
        let mut scanner = ActiveOberonScanner::new("RESULT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Result(1,1)))
    }

    #[test]
    fn scanner_keyword_then() {
        let mut scanner = ActiveOberonScanner::new("THEN");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Then(1,1)))
    }

    #[test]
    fn scanner_keyword_true() {
        let mut scanner = ActiveOberonScanner::new("TRUE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::True(1,1)))
    }

    #[test]
    fn scanner_keyword_to() {
        let mut scanner = ActiveOberonScanner::new("TO");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::To(1,1)))
    }

    #[test]
    fn scanner_keyword_type() {
        let mut scanner = ActiveOberonScanner::new("TYPE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Type(1,1)))
    }

    #[test]
    fn scanner_keyword_until() {
        let mut scanner = ActiveOberonScanner::new("UNTIL");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Until(1,1)))
    }

    #[test]
    fn scanner_keyword_var() {
        let mut scanner = ActiveOberonScanner::new("VAR");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Var(1,1)))
    }

    #[test]
    fn scanner_keyword_while() {
        let mut scanner = ActiveOberonScanner::new("WHILE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::While(1,1)))
    }

    #[test]
    fn scanner_keyword_with() {
        let mut scanner = ActiveOberonScanner::new("WITH");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::With(1,1)))
    }

    #[test]
    fn scanner_keyword_any() {
        let mut scanner = ActiveOberonScanner::new("ANY");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Any(1,1)))
    }

    #[test]
    fn scanner_keyword_array() {
        let mut scanner = ActiveOberonScanner::new("ARRAY");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Array(1,1)))
    }

    #[test]
    fn scanner_keyword_object() {
        let mut scanner = ActiveOberonScanner::new("OBJECT");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Object(1,1)))
    }

    #[test]
    fn scanner_keyword_pointer() {
        let mut scanner = ActiveOberonScanner::new("POINTER");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Pointer(1,1)))
    }

    #[test]
    fn scanner_keyword_record() {
        let mut scanner = ActiveOberonScanner::new("RECORD");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Record(1,1)))
    }

    #[test]
    fn scanner_keyword_address() {
        let mut scanner = ActiveOberonScanner::new("ADDRESS");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Address(1,1)))
    }

    #[test]
    fn scanner_keyword_size() {
        let mut scanner = ActiveOberonScanner::new("SIZE");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Size(1,1)))
    }

    #[test]
    fn scanner_keyword_alias() {
        let mut scanner = ActiveOberonScanner::new("ALIAS");
        assert_eq!(scanner.get_next_symbol(), Ok(Symbol::Alias(1,1)))
    }

}