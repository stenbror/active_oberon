
use crate::scanner::Symbol;


pub trait ActiveOberonScannerMethods {
    fn get_next_symbol(&mut self) -> Result<Symbol, String>;
    fn get_char(&self) -> (char, u32, u32);
    fn peek_char(&self) -> char;
}

pub struct ActiveOberonScanner {

}

impl ActiveOberonScannerMethods for ActiveOberonScanner {

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
            _ => Err(format!("Lexical Error => Line: {}, Col: {} - Unknown character '{}' found in source code!", line, col, ch ))
        }
    }

    fn get_char(&self) -> (char, u32, u32) {
        todo!()
    }

    fn peek_char(&self) -> char {
        todo!()
    }
}