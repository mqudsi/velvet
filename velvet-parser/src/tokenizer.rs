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
pub struct TokenizerError {
    pub kind: ErrorKind,
    pub line: u32,
    pub col: u32,
    pub len: u8,
}

#[derive(Debug)]
pub enum ErrorKind {
    EndOfStream,
    UnexpectedSymbol {
        symbol: u8,
    },
    /// A dangling `\\` without any following characters. Can be considered a for of
    /// [`ErrorKind::EndOfStream`] and isn't necessarily an error if the input isn't complete.
    UnterminatedEscape,
    /// A hex-based escape but followed by a non-hex character.
    InvalidEscape,
    /// A properly-formed `\u` or `\U` escape was provided but evaluated to an illegal value for a
    /// Unicode Scalar Value (i.e. was a high- or low-surrogate codepoint).
    InvalidCodepoint,
    /// An octal escape was used to form an extended ASCII value, which is not supported.
    InvalidAscii,
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

    /// Returns an error pointing to the current line and column, with the column offset by the
    /// provided `offset`.
    fn error<T>(&self, kind: ErrorKind, len: usize, offset: i32) -> Result<T, TokenizerError> {
        Err(TokenizerError {
            kind,
            line: self.line,
            col: (TryInto::<i32>::try_into(self.col).unwrap() + TryInto::<i32>::try_into(offset).unwrap()).try_into().unwrap(),
            len: len.try_into().unwrap(),
        })
    }

    #[inline]
    /// Bump the current read index and column. The line count isn't checked and should be handled
    /// separately!
    fn consume_char(&mut self) {
        self.index += 1;
        self.col += 1;
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
                    return self.error(ErrorKind::EndOfStream, 0, 0);
                } else {
                    return Ok(make_token!());
                }
            }

            /// A pull-based read that consumes the current character only if it is a match.
            macro_rules! read {
                (hex) => {
                    read!(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
                };
                ($pat:pat) => {
                    match self.read() {
                        None => Err(None),
                        Some(c @ $pat) => {
                            self.consume_char();
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
                            self.state.pop();
                        }
                        false
                    }
                }}
            };

            let c = self.input[self.index];
            eprintln!("c: {}, state: {:?}", c as char, self.state());
            match (c, self.state()) {
                // TODO: move escape handling to a function so we can keep it in cold storage
                (b'\\', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    if self.peek().is_none() {
                        return self.error(ErrorKind::UnterminatedEscape, 1, 0);
                    }
                    self.consume_char();
                    // Safe to unwrap because we've already used `peek()` above successfully.
                    let byte = match read!(_).unwrap() {
                        b'a' => 0x07, // alert
                        b'e' => 0x1b, // escape
                        b'f' => 0x0c, // form feed
                        b'v' => 0x0b, // vertical tab
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'x' | b'X' => {
                            // Read a one or two-digit hex sequence
                            let hex1 = match read!(hex) {
                                Ok(hex) => hex,
                                Err(None) => {
                                    return self.error(ErrorKind::InvalidEscape, 2, -2);
                                }
                                Err(_) => {
                                    return self.error(ErrorKind::InvalidEscape, 3, -2);
                                }
                            };
                            let hex2 = match read!(hex) {
                                Ok(hex) => Some(hex),
                                Err(None) => None,
                                Err(_) => None,
                            };

                            return Ok(Token {
                                ttype: TokenType::Text,
                                text: match (hex1, hex2) {
                                    (hex1, Some(hex2)) => {
                                        let src = &self.input[self.index - 2..][..2];
                                        let src = std::str::from_utf8(src).unwrap();
                                        let value = u8::from_str_radix(src, 16).expect(
                                            "We've already verified it's a valid hex value",
                                        );
                                        Cow::Owned(vec![value])
                                    }
                                    (hex1, None) => {
                                        let src = &self.input[self.index - 1..][..1];
                                        let src = std::str::from_utf8(src).unwrap();
                                        let value = u8::from_str_radix(src, 16).expect(
                                            "We've already verified it's a valid hex value",
                                        );
                                        Cow::Owned(vec![value])
                                    }
                                },
                                line: loc.0,
                                col: loc.1,
                                // TODO: store the slice containing the undecoded representation
                            });
                        }
                        b'u' => {
                            // Read 1-4 hex digits into a 16-bit Unicode codepoint
                            let mut hex_count = 0;
                            let mut hex_chars = [0u8; 4];
                            let hex_iter = std::iter::from_fn(|| read!(hex).ok()).take(4);
                            for hex in hex_iter {
                                hex_chars[hex_count] = hex;
                                hex_count += 1;
                            }

                            // Confine results to what we actually were able to read
                            let hex_chars = &hex_chars[0..hex_count];
                            if hex_count == 0 {
                                return match self.read() {
                                    None => {
                                        self.error(ErrorKind::UnterminatedEscape, 2, -2)
                                    }
                                    _ => self.error(ErrorKind::InvalidEscape, 3, -2)
                                };
                            }
                            // Convert from hex to a 16-bit value. Safe to unwrap because we've
                            // already validated that it's all valid hex characters and that we're
                            // only reading as many hex digits as would fit.
                            let hex_chars = std::str::from_utf8(hex_chars).unwrap();
                            let codepoint = u16::from_str_radix(hex_chars, 16).unwrap();
                            // Validate codepoint is a valid Unicode Scalar Value (anything other
                            // than a surrogate codepoint).
                            let codepoint = match char::from_u32(codepoint as u32) {
                                Some(c) => c,
                                None => {
                                    return self.error(ErrorKind::InvalidCodepoint, 2 + hex_count, -2 - hex_count as i32);
                                }
                            };

                            return Ok(Token {
                                ttype: TokenType::Text,
                                text: Cow::Owned(String::from(codepoint).into_bytes()),
                                line: loc.0,
                                col: loc.1,
                                // TODO: store the slice containing the undecoded representation
                            });
                        }
                        b'U' => {
                            // Read 1-8 hex digits into a 32-bit Unicode codepoint
                            let mut hex_count = 0;
                            let mut hex_chars = [0u8; 8];
                            let hex_iter = std::iter::from_fn(|| read!(hex).ok()).take(8);
                            for hex in hex_iter {
                                hex_chars[hex_count] = hex;
                                hex_count += 1;
                            }

                            // Confine results to what we actually were able to read
                            let hex_chars = &hex_chars[0..hex_count];
                            if hex_count == 0 {
                                return match self.read() {
                                    None => {
                                        self.error(ErrorKind::UnterminatedEscape, 2, -2)
                                    }
                                    _ => self.error(ErrorKind::InvalidEscape, 3, -2),
                                };
                            }
                            // Convert from hex to a 32-bit value. Safe to unwrap because we've
                            // already validated that it's all valid hex characters and that we're
                            // only reading as many hex digits as would fit.
                            let hex_chars = std::str::from_utf8(hex_chars).unwrap();
                            let codepoint = u32::from_str_radix(hex_chars, 16).unwrap();
                            // Validate codepoint is a valid Unicode Scalar Value (anything other
                            // than a surrogate codepoint).
                            let codepoint = match char::from_u32(codepoint) {
                                Some(c) => c,
                                None => self.error(ErrorKind::InvalidCodepoint, 2 + hex_count, -2 - hex_count as i32)?,
                            };

                            return Ok(Token {
                                ttype: TokenType::Text,
                                text: Cow::Owned(String::from(codepoint).into_bytes()),
                                line: loc.0,
                                col: loc.1,
                                // TODO: store the slice containing the undecoded representation
                            });
                        }
                        b'c' => {
                            // Generate the keycode for ctrl + whatever char comes next.
                            // A control code is the last five bits of the ASCII code, for the 32
                            // ASCII characters from @ to _ (mapping 0x40..=0x5F to 0x00..=0x1F)
                            // and repeated again (for the lowercase values) for the next 31 codes
                            // (from ` to ~ or 0x60 to 0x7E). The 32nd lowercase value is DEL (which
                            // isn't printable) so it isn't used.
                            let code = match read!(b'@'..=b'~') {
                                Ok(c) => c,
                                Err(None) => {
                                    return self.error(ErrorKind::UnterminatedEscape, 2, -2)
                                }
                                _ => return self.error(ErrorKind::InvalidEscape, 3, -2),
                            };
                            code & 0b00011111
                        }
                        o @ b'0'..=b'7' => {
                            // Read up to 3 octal digits and return them as a single ASCII character
                            let mut octal_count = 1;
                            let mut octal_chars = [o, 0, 0];
                            if let Ok(o) = read!(b'0'..=b'7') {
                                octal_chars[octal_count] = o;
                                octal_count = 2;
                                if let Ok(o) = read!(b'0'..=b'7') {
                                    octal_chars[octal_count] = o;
                                    octal_count = 3;
                                }
                            }
                            let octal_chars = &octal_chars[0..octal_count];
                            // Safe to unwrap because we've already guaranteed they're ASCII values
                            let octal_chars = std::str::from_utf8(octal_chars).unwrap();
                            // Maximum base-10 value that can be expressed in 3 octal digits is 511,
                            // but fish octal escapes are documented as returning an ASCII value.
                            // The maximum value that can fit into a u8 is o377 (255) but fish
                            // further caps the octal input to a max of o177 (128).

                            // Safe to unwrap since a 3-digit octal number has a max of 0x1FF
                            let value = u16::from_str_radix(octal_chars, 8).unwrap();
                            if value > 0o177 {
                                return
                                    self.error(ErrorKind::InvalidAscii, 1 + octal_count, -1 - octal_count as i32);
                            }
                            value as u8
                        }
                        b'\n' => {
                            // A literal new line after an escape is a continuation and should be
                            // interpreted as ignoring the new line altogether (though incrementing
                            // our inner line count).
                            start = self.index;
                            loc = (self.line, self.col);
                            continue;
                        }
                        b'\r' => {
                            // Treat like an escape of a literal \n if followed by one
                            if read!(b'\n').is_ok() {
                                start = self.index;
                                loc = (self.line, self.col);
                            } else {
                                // Otherwise continuing below will just map an escaped \r to itself,
                                // which is fine.
                            }
                            continue;
                        }
                        c => {
                            // The escape evaluates to the next character itself.
                            // Instead of returning a single character as a text token, just consume
                            // the character and progress the loop. The character will be the start
                            // of a new text token.
                            continue;
                        }
                    };

                    return Ok(Token {
                        ttype: TokenType::Text,
                        text: Cow::Owned(vec![byte]),
                        line: loc.0,
                        col: loc.1,
                        // TODO: return the input slice mapping to this text
                    });
                }
                (b'"', TokenizerState::DoubleQuote) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.consume_char();
                    self.state.pop();
                    return Ok(make_token!(TokenType::DoubleQuote));
                }
                (b'\'', TokenizerState::SingleQuote) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.consume_char();
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
                    self.consume_char();
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
                    self.consume_char();
                    return Ok(make_token!(TokenType::IndexStart));
                }
                (b'(' | b'{' | b'[', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.consume_char();
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
                    self.consume_char();
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
                    let error = self.error(ErrorKind::UnexpectedSymbol { symbol: c }, 1, 0);
                    self.consume_char();
                    return error;
                }
                (b',', TokenizerState::Brace) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.consume_char();
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
                        return Ok(make_token!());
                    }
                    while read!(b'\r' | b'\n').is_ok() {}
                    return Ok(make_token!(TokenType::EndOfLine));
                }
                (b';', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.consume_char();
                    return Ok(make_token!(TokenType::Semicolon));
                }
                (b'"', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.state.push(TokenizerState::DoubleQuote);
                    self.consume_char();
                    return Ok(make_token!(TokenType::DoubleQuote));
                }
                (b'\'', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.state.push(TokenizerState::SingleQuote);
                    self.consume_char();
                    return Ok(make_token!(TokenType::SingleQuote));
                }
                (b'&', _) => {
                    if self.peek() == Some(b'&') {
                        // Peek before returning a fragment so we can return the & appended to the
                        // previous text if it's not going to be treated as a special symbol.
                        if have_fragment!() {
                            return Ok(make_token!());
                        }
                        self.consume_char();
                        self.consume_char();
                        return Ok(make_token!(TokenType::LogicalAnd));
                    }
                    if matches!(self.peek(), Some(b' ' | b'\r' | b'\n' | b'|' | b';') | None) {
                        if have_fragment!() {
                            return Ok(make_token!());
                        }
                        self.consume_char();
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
                    self.consume_char();
                    if read!(b'|').is_ok() {
                        return Ok(make_token!(TokenType::LogicalOr));
                    }
                    return Ok(make_token!(TokenType::Pipe));
                }
                _ => {}
            }

            match self.read() {
                Some(b'\n') => {
                    self.index += 1;
                    self.line += 1;
                    self.col = 1;
                },
                _ => {
                    self.index += 1;
                    self.col += 1;
                },
            }

            // TODO: Handle UTF-8 fragments as incomplete tokens
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_next() {
            Err(TokenizerError {
                kind: ErrorKind::EndOfStream,
                ..
            }) => None,
            x => Some(x),
        }
    }
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedSymbol { symbol } => {
                write!(f, "Unexpected symbol '{}'", *symbol as char)
            }
            ErrorKind::UnterminatedEscape => write!(f, "Unterminated escape"),
            ErrorKind::EndOfStream => write!(f, "End-of-stream"),
            ErrorKind::InvalidEscape => write!(f, "Invalid escape"),
            ErrorKind::InvalidCodepoint => {
                write!(f, "Unicode escape evaluated to an invalid codepoint")
            }
            ErrorKind::InvalidAscii => {
                write!(f, "Octal escape evaluated to an invalid ASCII character")
            }
        }
    }
}

impl std::fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}:{}", self.kind, self.line, self.col)
    }
}

impl std::error::Error for TokenizerError {}
