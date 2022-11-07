#![allow(unused)]

use staticsort::staticsort;
use std::borrow::Cow;
use std::io::prelude::*;
use std::io::{BufRead, BufReader};

pub struct Token<'a> {
    pub ttype: TokenType,
    pub text: Cow<'a, [u8]>,
    pub line: u32,
    pub col: u32,
}

impl core::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("ttype", &self.ttype)
            .field("text", &String::from_utf8_lossy(&self.text))
            .field("position", &format!("{}:{}", self.line, self.col))
            .finish()
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenTag {
    None,
    /// Indicates that this is a partial token that must be concatenated with the previous token
    /// (and any subsequent adjacent [`TokenTag::Continuation`] tokens) to form a full token.
    Continuation,
}

pub struct Tokenizer<'a> {
    input: &'a [u8],
    /// A stack of states
    state: Vec<TokenizerState>,
    index: usize,
    line: u32,
    col: u32,
    /// Tracks the number of opened brackets `[` (only in contexts where they are for indexing)
    bracket_count: u32,
    /// Tracks the number of opened parentheses `(` (only in contexts where they are for subshells)
    subshell_count: u32,
    /// Tracks the number of opened braces `(` (only in contexts where they are for expansion)
    braces_count: u32,
    return_token: Option<TokenType>,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Text,
    DoubleQuote,
    SingleQuote,
    Dollar,
    IndexStart,
    SubshellStart,
    OpeningBrace,
    IndexEnd,
    SubshellEnd,
    ClosingBrace,
    Semicolon,
    /// Coalesced `\n` or `\r\n`. Never a `\r` by itself.
    EndOfLine,
    /// The literal `|` but not in the case of `||` (see [`TokenType::Or`]).
    Pipe,
    Redirection,
    /// Coalesced whitespace. Currently just spaces.
    Whitespace,
    Comment,
    Backgrounding,
    /// Either a literal `&&` or a literal `and`.
    LogicalAnd,
    /// Either a literal `||` or a literal `or`. See also [`TokenType::Pipe`].
    LogicalOr,
}

#[derive(Debug)]
pub enum TokenizerError {
    Io(std::io::Error),
    EndOfStream,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenizerState {
    None,
    Quote(u8),
    VariableName,
}

trait BufReaderExt {
    /// Reads until any of the bytes (or ascii characters) in `any` are found.
    ///
    /// The sequence of read bytes (including the stop character) is written to `buf`, and the
    /// number of bytes read plus the stop character (or `None` in case of EOF) are returned.
    fn read_until_any(
        &mut self,
        buf: &mut Vec<u8>,
        any: &[u8],
    ) -> std::io::Result<(usize, Option<u8>)>;
}

impl<R: Read> BufReaderExt for BufReader<R> {
    fn read_until_any(
        &mut self,
        buf: &mut Vec<u8>,
        any: &[u8],
    ) -> std::io::Result<(usize, Option<u8>)> {
        // TODO: Optimize this somehow so we're not reading a single byte at a time.
        let mut idx = 0;
        loop {
            match self.read_exact(&mut buf[idx..][..1]) {
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok((idx, None)),
                Err(e) => return Err(e.into()),
                Ok(_) => {}
            };
            match any.binary_search(&buf[idx]) {
                Err(_) => {
                    idx += 1;
                    continue;
                }
                Ok(n) => {
                    idx += 1;
                    return Ok((idx, Some(any[n])));
                }
            }
        }
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a [u8]) -> Tokenizer<'a> {
        Tokenizer {
            input: source,
            state: Vec::new(),
            index: 0,
            line: 1,
            col: 1,
            bracket_count: 0,
            subshell_count: 0,
            braces_count: 0,
            return_token: None,
        }
    }

    fn tokenizer(input: &[u8]) -> impl Iterator<Item = Result<Token, TokenizerError>> {
        let tokenizer = Tokenizer::new(input);
        tokenizer.into_iter()
    }

    /// Peeks the current state from the top of [`TokenizerState::state`] or returns
    /// [`TokenizerState::None`].
    fn state(&self) -> TokenizerState {
        match self.state.last() {
            Some(s) => *s,
            None => TokenizerState::None,
        }
    }

    #[inline]
    fn peek(&self) -> Option<u8> {
        if self.index + 1 >= self.input.len() {
            None
        } else {
            Some(self.input[self.index + 1])
        }
    }

    fn read_next(&mut self) -> Result<Token<'a>, TokenizerError> {
        let mut loc = (self.line, self.col);
        let mut start = self.index;
        let mut is_escape = false;

        macro_rules! make_token {
            ($ttype:expr) => {{
                eprintln!("Returning token from state {:?}", self.state());
                let token = Token {
                    ttype: $ttype,
                    text: Cow::Borrowed(&self.input[start..self.index]),
                    line: loc.0,
                    col: loc.1,
                };
                eprintln!("Returning token of type {:?}", token.ttype);
                start = self.index;
                token
            }};
        }

        if self.index < self.input.len() {
            eprintln!("Starting/resuming in state {:?} on c == '{}' ({}:{})", self.state(), self.input[self.index] as char, loc.0, loc.1);
        }
        let mut skip_char = false;
        loop {
            if self.index == self.input.len() {
                if start == self.index {
                    return Err(TokenizerError::EndOfStream);
                } else {
                    return Ok(make_token!(TokenType::Text));
                }
            }

            /// read_next!() is a macro that hides the fact that we skip over non-significant
            /// whitespace and return a partially completed token before resuming parsing.
            macro_rules! read {
                ($pat:pat) => {
                    if (self.index + 1) >= self.input.len() {
                        Err(None)
                    } else if matches!(self.input[(self.index + 1)], $pat) {
                        self.index += 1;
                        self.col += 1;
                        let c = self.input[self.index];
                        // We expect the first predicate below to be a compile-time constant
                        if matches!(b'\n', $pat) && c == b'\n' {
                            self.line += 1;
                            self.col = 1;
                        }
                        Ok(c)
                    } else {
                        Err(Some(self.input[self.index + 1]))
                    }
                };
                (not: $pat:pat) => {
                    if (self.index + 1) >= self.input.len() {
                        Err(None)
                    } else if !matches!(self.input[(self.index + 1)], $pat) {
                        self.index += 1;
                        self.col += 1;
                        let c = self.input[self.index];
                        // We expect the first predicate below to be a compile-time constant
                        if !matches!(b'\n', $pat) && c == b'\n' {
                            self.line += 1;
                            self.col = 1;
                        }
                        Ok(c)
                    } else {
                        Err(Some(self.input[self.index + 1]))
                    }
                };
            }

            let c = self.input[self.index];
            eprintln!("c: {}, state: {:?}", c as char, self.state());
            match (c, self.state()) {
                (b'\\', _) => {
                    todo!("handle escapes!");
                }
                (c, TokenizerState::Quote(q)) if c == q => {
                    if self.index != start {
                        return Ok(make_token!(TokenType::Text)); // previous or text
                    }
                    self.state.pop();
                    self.index += 1;
                    self.col += 1;
                    let ttype = match c {
                        b'"' => TokenType::DoubleQuote,
                        b'\'' => TokenType::SingleQuote,
                        _ => unreachable!(),
                    };
                    return Ok(make_token!(ttype));
                },
                (_, TokenizerState::Quote(b'\'')) => {
                    // Just keep going
                },
                (b'$', _) => {
                    if self.index != start {
                        return Ok(make_token!(TokenType::Text));
                    }
                    self.state.push(TokenizerState::VariableName);
                    self.index += 1;
                    self.col += 1;
                    return Ok(make_token!(TokenType::Dollar));
                }
                (_, TokenizerState::Quote(b'"')) => {
                    // Just keep going
                },
                (b'(' | b'{' | b'[', _) => {
                    if self.index != start {
                        return Ok(make_token!(TokenType::Text)); // previous or text
                    }
                    self.index += 1;
                    self.col += 1;
                    let ttype = match c {
                        b'(' => TokenType::SubshellStart,
                        b'{' => TokenType::OpeningBrace,
                        b'[' => TokenType::IndexStart,
                        _ => unreachable!(),
                    };
                    return Ok(make_token!(ttype));
                },
                (b')' | b'}' | b']', _) => {
                    if self.index != start {
                        return Ok(make_token!(TokenType::Text)); // previous or text
                    }
                    self.index += 1;
                    self.col += 1;
                    let ttype = match c {
                        b')' => TokenType::SubshellEnd,
                        b'}' => TokenType::ClosingBrace,
                        b']' => TokenType::IndexEnd,
                        _ => unreachable!(),
                    };
                    return Ok(make_token!(ttype));
                },
                (b' ', _) => {
                    if self.index != start {
                        return Ok(make_token!(TokenType::Text)); // previous or start
                    }
                    self.state.pop();
                    while read!(b' ').is_ok() {
                    }
                    self.index += 1;
                    self.col += 1;
                    return Ok(make_token!(TokenType::Whitespace));
                }
                (b'\r', _) => {
                    if self.peek() == Some(b'\n') {
                        if self.index != start {
                            return Ok(make_token!(TokenType::Text));
                        }

                        // We can merge the \r into the \n and treat as TT::EndOfLine
                        while read!(b'\r' | b'\n').is_ok() {
                        }
                        self.index += 1;
                        self.col += 1;
                        return Ok(make_token!(TokenType::EndOfLine));
                    }
                    // Otherwise append the \r to whatever text comes next
                }
                (b'\n', _) => {
                    if self.index != start {
                        return Ok(make_token!(TokenType::Text)); // previous or start
                    }
                    self.line += 1;
                    self.col = 1;
                    self.state.pop();
                    while read!(b'\n' | b'\n').is_ok() {
                    }
                    self.index += 1;
                    return Ok(make_token!(TokenType::EndOfLine));
                }
                (b';', _) => {
                    if self.index != start {
                        return Ok(make_token!(TokenType::Text)); // previous or start
                    }
                    self.state.pop();
                    self.index += 1;
                    self.col += 1;
                    return Ok(make_token!(TokenType::Semicolon));
                }
                (b'"' | b'"', _) => {
                    if self.index != start {
                        return Ok(make_token!(TokenType::Text)); // previous or text
                    }
                    self.state.push(TokenizerState::Quote(c));
                    self.index += 1;
                    self.col += 1;
                    let ttype = match c {
                        b'"' => TokenType::DoubleQuote,
                        b'\'' => TokenType::SingleQuote,
                        _ => unreachable!(),
                    };
                    return Ok(make_token!(ttype));
                },
                (b'&', _) => {
                    if self.peek() == Some(b'&') {
                        if self.index != start {
                            return Ok(make_token!(TokenType::Text)); // previous or start
                        }
                        _ = read!(b'&');
                        self.index += 1;
                        self.col += 1;
                        return Ok(make_token!(TokenType::LogicalAnd));
                    }
                    if matches!(self.peek(), Some(b' ' | b'\n' | b'|' | b';') | None) {
                        if self.index != start {
                            return Ok(make_token!(TokenType::Text)); // previous or text
                        }
                        _ = read!(_);
                        return Ok(make_token!(TokenType::Backgrounding));
                    }
                    // It is to be considered regular text and returned appended to what
                    // came before and/or comes after.
                },
                _ => {
                }
            }

            assert_ne!(c, b'\n');

            self.index += 1;
            self.col += 1;


            // TODO: Handle UTF-8 fragments as incomplete tokens
        }
    }
}

// #[test]
// fn basic_syntax_test() {
//     let input = format!(r#"
// set x "foo"[1]
// echo "hello world"$goodbye[2]" friend"
// echo "hi "(echo from a (echo nested)[1] subshell)
// echo this&is a string{nl}
// echo th{nl}is is all text
// echo this is backgrounded&
// "#, nl = '\r');
//     let tokenizer = Tokenizer::new(input.as_bytes());
//     let tokens: Vec<_> = tokenizer.collect();
//
//     panic!("{:#?}", tokens);
// }

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_next() {
            Err(TokenizerError::EndOfStream) => None,
            x => Some(x),
        }
    }
}

impl std::fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenizerError::Io(e) => e.fmt(f),
            TokenizerError::EndOfStream => write!(f, "End-of-stream"),
        }
    }
}

impl std::error::Error for TokenizerError {}

impl From<std::io::Error> for TokenizerError {
    fn from(value: std::io::Error) -> Self {
        TokenizerError::Io(value)
    }
}
