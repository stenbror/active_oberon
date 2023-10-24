

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
                        (ch, line, col) = self.get_char();
                        loop {
                            ch2 = self.peek_char();
                            match ch2 {
                                '*' => {
                                    (ch, line, col) = self.get_char();
                                    ch2 = self.peek_char();
                                    match ch2 {
                                        ')' => {
                                            level = level - 1;
                                            if level <= 0 { break }
                                            (ch, line, col) = self.get_char();
                                        },
                                        _ => (ch, line, col) = self.get_char()
                                    }
                                },
                                '(' => {
                                    ch2 = self.peek_char();
                                    match ch2 {
                                        '*' => {
                                            (ch, line, col) = self.get_char();
                                            level = level + 1
                                        },
                                        _ => (ch, line, col) = self.get_char()
                                    }
                                },
                                '\0' => return Err(format!("Lexical Error => Line: {}, Col: {} - Unterminated comment!", line, col )),
                                _ => (ch, line, col) = self.get_char()
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
            _ => Err(format!("Lexical Error => Line: {}, Col: {} - Unknown character '{}' found in source code!", line, col, ch ))
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
}