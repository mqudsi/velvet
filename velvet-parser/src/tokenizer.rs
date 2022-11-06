#![allow(unused)]

use staticsort::staticsort;
use std::borrow::Cow;
use std::io::prelude::*;
use std::io::{BufRead, BufReader};

pub struct Token<'a> {
    pub ttype: TokenType,
    pub tag: TokenTag,
    pub text: Cow<'a, [u8]>,
    pub line: u32,
    pub col: u32,
}

impl core::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("ttype", &self.ttype)
            .field("tag", &self.tag)
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
    state: TokenizerState,
    continuation: bool,
    index: usize,
    line: u32,
    col: u32,
    /// Tracks the number of opened brackets `[` (only in contexts where they are for indexing)
    bracket_count: u32,
    /// Tracks the number of opened parentheses `(` (only in contexts where they are for subshells)
    subshell_count: u32,
    /// Tracks the number of opened braces `(` (only in contexts where they are for expansion)
    braces_count: u32,
}

#[derive(Debug)]
pub enum TokenType {
    Text,
    Variable,
    Index,
    Subshell,
    /// Represents either a literal `\n` or a semicolon `;`
    EndOfLine,
    Pipe,
    Redirection,
    /// Significant whitespace, e.g. one or more leading spaces at the start of a command
    Whitespace,
    Comment,
}

#[derive(Debug)]
pub enum TokenizerError {
    Io(std::io::Error),
    EndOfStream,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenizerState {
    None,
    PendingChar(u8),
    VariableName,
    Bracket,
    Subshell,
    Braces,
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

impl TokenizerState {
    #[inline]
    fn terminates_on(&self, c: u8) -> bool {
        match (c, self) {
            // Handles ending single-quoted/double-quoted contexts
            (c, TokenizerState::PendingChar(c2)) => c == *c2,
            (b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', TokenizerState::VariableName) => false,
            (_, TokenizerState::VariableName) => true,
            (b' ' | b'\n' | b';', TokenizerState::None) => true,
            _ => false,
        }
    }

    /// This is called after [`TokenizerState::terminates_on()`] and thus elides states that would
    /// have matched there.
    #[inline]
    fn ignores(&self, c: u8) -> bool {
        match (c, self) {
            (b'"', TokenizerState::None) => false,
            (b'\'', TokenizerState::None) => false,
            (b'$', TokenizerState::PendingChar(b'"')) => false,
            (b'(', TokenizerState::None) => false,
            (b'[', TokenizerState::None) => false,
            (b'{', TokenizerState::None) => false,
            _ => true,
        }
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a [u8]) -> Tokenizer<'a> {
        Tokenizer {
            input: source,
            state: TokenizerState::None,
            continuation: false,
            index: 0,
            line: 1,
            col: 1,
            bracket_count: 0,
            subshell_count: 0,
            braces_count: 0,
        }
    }

    fn read_next(&mut self) -> Result<Token<'a>, TokenizerError> {
        let mut loc = (self.line, self.col);
        let mut start = self.index;
        let mut is_escape = false;

        macro_rules! make_token {
            () => {
                make_token!(next_state: self.state, continues: self.continuation)
            };
            (next_state: $next_state:expr) => {
                make_token!(next_state: $next_state, continues: self.continuation)
            };
            (next_state: $next_state:expr, continues: $continues:expr) => {{
                let token = Token {
                    ttype: if is_escape {
                        TokenType::Text
                    } else if self.state == TokenizerState::Subshell {
                        TokenType::Subshell
                    } else if self.state == TokenizerState::Bracket {
                        TokenType::Index
                    } else if self.state == TokenizerState::VariableName {
                        TokenType::Variable
                    } else {
                        TokenType::Text
                    },
                    tag: if self.continuation {
                        TokenTag::Continuation
                    } else {
                        TokenTag::None
                    },
                    text: Cow::Borrowed(&self.input[start..self.index]),
                    line: loc.0,
                    col: loc.1,
                };
                self.state = $next_state;
                self.continuation = $continues;
                token
            }};
        }

        if self.index < self.input.len() {
            eprintln!("Starting/resuming in state {:?} on c == '{}' ({}:{})", self.state, self.input[self.index] as char, loc.0, loc.1);
        }
        let mut skip_char = false;
        loop {
            if self.index == self.input.len() {
                if start == self.index {
                    return Err(TokenizerError::EndOfStream);
                } else {
                    return Ok(make_token!());
                }
            }

            let c = self.input[self.index];

            if is_escape {
                todo!("Handle remainder of escape as buffered string!");
            }

            if self.state.terminates_on(c) {
                let next_state = if self.state != TokenizerState::VariableName { self.state } else { TokenizerState::None };
                if self.index != start {
                    // End tokenization here, restarting at this same symbol next time
                    return Ok(make_token!(next_state: next_state));
                }
                self.state = TokenizerState::None;
                self.continuation = !matches!(c, b' ' | b';' | b'\n');
                skip_char = true;
            } else if self.terminate_after(c) {
                self.index += 1;
                self.col += 1;
                return Ok(make_token!(next_state: TokenizerState::None, continues: true));
            } else if self.state.ignores(c) {
                if self.state == TokenizerState::Bracket && c == b'[' {
                    self.bracket_count += 1;
                } else if self.state == TokenizerState::Subshell && c == b'(' {
                    self.subshell_count += 1;
                } else if self.state == TokenizerState::Braces && c == b'{' {
                    self.braces_count += 1;
                }
            } else if c == b'\\' {
                is_escape = true;
                if self.index != start {
                    return Ok(make_token!());
                }
            } else if c == b'"' {
                if self.index != start {
                    return Ok(make_token!());
                }
                self.state = TokenizerState::PendingChar(b'"');
                skip_char = true;
            } else if c == b'\'' {
                if self.index != start {
                    return Ok(make_token!());
                }
                self.state = TokenizerState::PendingChar(b'\'');
                skip_char = true;
            } else if c == b'[' {
                if self.index != start {
                    return Ok(make_token!());
                }
                eprintln!("brackets mode");
                self.state = TokenizerState::Bracket;
                self.bracket_count += 1;
            } else if c == b'(' {
                if self.index != start {
                    return Ok(make_token!());
                }
                self.state = TokenizerState::Subshell;
                self.subshell_count += 1;
            } else if c == b'{' {
                if self.index != start {
                    return Ok(make_token!());
                }
                self.state = TokenizerState::Braces;
                self.braces_count += 1;
            } else if c == b'$' {
                if self.index != start {
                    return Ok(make_token!());
                }
                self.state = TokenizerState::VariableName;
            }

            self.index += 1;
            self.col += 1;
            if c == b'\n' {
                if !skip_char {
                    eprintln!("Not skipping new line in state {:?}", self.state);
                }
                self.line += 1;
                self.col = 1;
            }
            if skip_char {
                start = self.index;
                loc = (self.line, self.col);
            }
            skip_char = false;

            // TODO: Handle UTF-8 fragments as incomplete tokens
        }
    }

    #[inline]
    fn terminate_after(&mut self, c: u8) -> bool {
        match (c, &self.state) {
            (b']', &TokenizerState::Bracket) => {
                self.bracket_count -= 1;
                self.bracket_count == 0
            },
            (b')', &TokenizerState::Subshell) => {
                self.subshell_count -= 1;
                self.subshell_count == 0
            }
            (b'}', &TokenizerState::Braces) => {
                self.braces_count -= 1;
                self.braces_count == 0
            }
            _ => false,
        }
    }
}

#[test]
fn basic_syntax_test() {
    let input = r#"
set x "foo"[1]
echo "hello world"$goodbye[2]" friend"
echo "hi "(echo from a (echo nested)[1] subshell)
"#;
    let tokenizer = Tokenizer::new(input.as_bytes());
    let tokens: Vec<_> = tokenizer.collect();

    panic!("{:#?}", tokens);
}

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
