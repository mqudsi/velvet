#![allow(unused)]

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
    cached_token: Option<Token<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Text,
    DoubleQuote,
    SingleQuote,
    Dollar,
    IndexStart,
    SubshellStart,
    BraceStart,
    IndexEnd,
    SubshellEnd,
    BraceEnd,
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
    /// A literal `,` in the context of a brace expansion
    BraceSeparator,
    VariableName,
}

#[derive(Debug)]
pub enum TokenizerError {
    Io(std::io::Error),
    EndOfStream,
    UnexpectedSymbol { symbol: u8, line: u32, col: u32 },
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenizerState {
    None,
    SingleQuote,
    DoubleQuote,
    Subshell,
    Brace,
    Index,
    /// We need a separate state for variable name so we can properly tokenize an index after a
    /// variable name in a quoted context.
    ///
    /// We can handle `echo "$(echo foo)[1]"` just fine, but `echo "$foo[1]"` is context-sensitive
    /// as here we have an index but in `echo "foo[1]"` we have no index.
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

pub fn tokenize(input: &[u8]) -> impl Iterator<Item = Result<Token, TokenizerError>> {
    let tokenizer = Tokenizer::new(input);
    tokenizer.into_iter()
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
            cached_token: None,
        }
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
    fn read(&self) -> Option<u8> {
        if self.index >= self.input.len() {
            None
        } else {
            Some(self.input[self.index])
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

    #[inline]
    fn prev(&self) -> Option<u8> {
        if self.index > 1 {
            Some(self.input[self.index - 1])
        } else {
            None
        }
    }

    fn read_next(&mut self) -> Result<Token<'a>, TokenizerError> {
        if let Some(token) = self.cached_token.take() {
            return Ok(token);
        }

        let mut loc = (self.line, self.col);
        let mut start = self.index;
        let mut is_escape = false;

        macro_rules! make_token {
            () => {{
                let ttype = match self.state() {
                    TokenizerState::VariableName => {
                        self.state.pop();
                        TokenType::VariableName
                    }
                    _ => TokenType::Text,
                };
                make_token!(ttype)
            }};
            ($ttype:expr) => {{
                let token = Token {
                    ttype: $ttype,
                    text: Cow::Borrowed(&self.input[start..self.index]),
                    line: loc.0,
                    col: loc.1,
                };
                eprintln!(
                    "Returning token {:?} of type {:?} from state {:?}",
                    &String::from_utf8_lossy(&token.text),
                    token.ttype,
                    self.state()
                );
                assert_ne!(self.state(), TokenizerState::VariableName);
                start = self.index;
                token
            }};
        }

        /// A macro to modify `self.index` (and associated position values) so that the semantics of
        /// the operation and the intention behind it are clear.
        macro_rules! consume_char {
            () => {{
                self.index += 1;
                self.col += 1;
            }};
            (check_line) => {{
                match self.read() {
                    Some(b'\n') => {
                        self.index += 1;
                        self.line += 1;
                        self.col = 1;
                    }
                    _ => consume_char!(),
                }
            }};
        }

        if self.index < self.input.len() {
            eprintln!(
                "Starting/resuming in state {:?} on c == '{}' ({}:{})",
                self.state(),
                self.input[self.index] as char,
                loc.0,
                loc.1
            );
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

            /// A pull-based read that consumes the current character only if it is a match.
            macro_rules! read {
                ($pat:pat) => {
                    match self.read() {
                        None => Err(None),
                        Some(c @ $pat) => {
                            consume_char!();
                            // We expect the first predicate below to be a compile-time constant
                            if matches!(b'\n', $pat) && c == b'\n' {
                                self.line += 1;
                                self.col = 1;
                            }
                            Ok(c)
                        }
                        Some(c) => Err(Some(c)),
                    }
                };
            }

            /// Determines if we need to return what we have in the buffer before returning a new
            /// token type. The magic is actually in the `false` branch where we need to end certain
            /// contex-dependent tokenizer states before continuing.
            macro_rules! have_fragment {
                () => {{
                    if start != self.index {
                        true
                    } else {
                        // In order to correctly tokenize a quoted variable name with an index like
                        // `echo $foo[1]` we need to push a `VariableName` state into the stack so we
                        // can detect and handle the index as non-text. We don't need it to handle
                        // something like `echo "$(foo)[1]" because a simple peek will suffice.
                        if self.state() == TokenizerState::VariableName {
                            eprintln!("Reverting from VariableName state without returning a token.");
                            self.state.pop();
                        }
                        false
                    }
                }}
            };

            let c = self.input[self.index];
            eprintln!("c: {}, state: {:?}", c as char, self.state());
            match (c, self.state()) {
                (b'\\', _) => {
                    todo!("handle escapes!");
                }
                (b'"', TokenizerState::DoubleQuote) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    consume_char!();
                    self.state.pop();
                    return Ok(make_token!(TokenType::DoubleQuote));
                }
                (b'\'', TokenizerState::SingleQuote) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    consume_char!();
                    self.state.pop();
                    return Ok(make_token!(TokenType::SingleQuote));
                }
                (_, TokenizerState::SingleQuote) => {
                    // Just keep going
                }
                (b'$', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    consume_char!();
                    let dollar = make_token!(TokenType::Dollar);
                    // Make sure we don't lose out on a subshell start in a quoted context
                    if read!(b'(').is_ok() {
                        self.state.push(TokenizerState::Subshell);
                        self.cached_token = Some(make_token!(TokenType::SubshellStart));
                    } else {
                        self.state.push(TokenizerState::VariableName);
                    }
                    return Ok(dollar);
                }
                (_, TokenizerState::DoubleQuote) => {
                    // Just keep going
                }
                // We need to handle this directly to prevent the state from reverting to
                // DoubleQuote after terminating the variable name.
                (b'[', TokenizerState::VariableName) => {
                    if have_fragment!() {
                        let token = make_token!();
                        // We need to explicitly reset the VariableName state because the default
                        // make_token! macro pops it off our stack.
                        self.state.push(TokenizerState::VariableName);
                        return Ok(token);
                    }
                    self.state.push(TokenizerState::Index);
                    consume_char!();
                    return Ok(make_token!(TokenType::IndexStart));
                }
                (b'(' | b'{' | b'[', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    consume_char!();
                    let (ttype, state) = match c {
                        b'(' => (TokenType::SubshellStart, TokenizerState::Subshell),
                        b'{' => (TokenType::BraceStart, TokenizerState::Brace),
                        b'[' => (TokenType::IndexStart, TokenizerState::Index),
                        _ => unreachable!(),
                    };
                    self.state.push(state);
                    return Ok(make_token!(ttype));
                }
                (b')', TokenizerState::Subshell)
                | (b'}', TokenizerState::Brace)
                | (b']', TokenizerState::Index) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    consume_char!();
                    self.state.pop();
                    let ttype = match c {
                        b')' => TokenType::SubshellEnd,
                        b'}' => TokenType::BraceEnd,
                        b']' => TokenType::IndexEnd,
                        _ => unreachable!(),
                    };
                    let closing_symbol = make_token!(ttype);
                    // Make sure we don't lose out on an index start in a quoted context
                    if c == b')' && read!(b'[').is_ok() {
                        self.state.push(TokenizerState::Index);
                        self.cached_token = Some(make_token!(TokenType::IndexStart));
                    }
                    return Ok(closing_symbol);
                }
                // fish treats an unmatched `]` differently from an unmatched `}` or `]`;
                // specifically, it is not an error but rather regular text.
                (b')' | b'}', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    let error = TokenizerError::UnexpectedSymbol {
                        symbol: c,
                        line: self.line,
                        col: self.col,
                    };
                    consume_char!();
                    return Err(error);
                }
                (b',', TokenizerState::Brace) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    consume_char!();
                    return Ok(make_token!(TokenType::BraceSeparator));
                }
                (b' ', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    while read!(b' ').is_ok() {}
                    return Ok(make_token!(TokenType::Whitespace));
                }
                (b'\r', _) => {
                    if self.peek() == Some(b'\n') {
                        if have_fragment!() {
                            return Ok(make_token!(TokenType::Text));
                        }
                        // We can merge the \r into the \n and treat as TT::EndOfLine
                        while read!(b'\r' | b'\n').is_ok() {}
                        return Ok(make_token!(TokenType::EndOfLine));
                    }
                    // Otherwise append the \r to whatever text comes next
                }
                (b'\n', _) => {
                    if have_fragment!() {
                        eprintln!("Returning before \\n pos: ({}, {})", self.line, self.col);
                        return Ok(make_token!());
                    }
                    eprintln!("Before \\n pos: ({}, {})", self.line, self.col);
                    while read!(b'\r' | b'\n').is_ok() {}
                    eprintln!("After \\n pos: ({}, {})", self.line, self.col);
                    return Ok(make_token!(TokenType::EndOfLine));
                }
                (b';', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    consume_char!();
                    return Ok(make_token!(TokenType::Semicolon));
                }
                (b'"', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.state.push(TokenizerState::DoubleQuote);
                    consume_char!();
                    return Ok(make_token!(TokenType::DoubleQuote));
                }
                (b'\'', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.state.push(TokenizerState::SingleQuote);
                    consume_char!();
                    return Ok(make_token!(TokenType::SingleQuote));
                }
                (b'&', _) => {
                    if self.peek() == Some(b'&') {
                        // Peek before returning a fragment so we can return the & appended to the
                        // previous text if it's not going to be treated as a special symbol.
                        if have_fragment!() {
                            return Ok(make_token!());
                        }
                        consume_char!();
                        consume_char!();
                        return Ok(make_token!(TokenType::LogicalAnd));
                    }
                    if matches!(self.peek(), Some(b' ' | b'\r' | b'\n' | b'|' | b';') | None) {
                        if have_fragment!() {
                            return Ok(make_token!());
                        }
                        consume_char!();
                        return Ok(make_token!(TokenType::Backgrounding));
                    }
                    // Else it is to be considered regular text and returned appended to what came
                    // before and/or comes after.
                }
                (b'|', _) => {
                    // Unlike the case with `&`, `|` is always a special symbol.
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    consume_char!();
                    if read!(b'|').is_ok() {
                        return Ok(make_token!(TokenType::LogicalOr));
                    }
                    return Ok(make_token!(TokenType::Pipe));
                }
                _ => {}
            }

            consume_char!(check_line);

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
            TokenizerError::UnexpectedSymbol { symbol, line, col } => {
                write!(f, "Unexpected symbol '{symbol}' at {line}:{col}")
            }
        }
    }
}

impl std::error::Error for TokenizerError {}

impl From<std::io::Error> for TokenizerError {
    fn from(value: std::io::Error) -> Self {
        TokenizerError::Io(value)
    }
}
