
use crate::scanner::Symbol;


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