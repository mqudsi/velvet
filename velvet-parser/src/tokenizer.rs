use std::borrow::Cow;
use std::io::prelude::*;
use std::io::BufReader;

/// The path separator to recognize and use for tokenization purposes.
const PATH_SEPARATOR: u8 = b'/';

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

pub struct Tokenizer<'a> {
    input: &'a [u8],
    /// A stack of states. Should only be used with states that can stack and last for more than one
    /// word at a time. Otherwise [`Tokenizer::temp_state`] should be assigned to instead.
    state: Vec<TokenizerState>,
    /// A state that doesn't last more than one "word" and, when present, supercedes
    /// [`Tokenizer::state`].
    ///
    /// It is unset every time `have_fragment!()` is called when `have_fragment!()` returns false,
    /// or when a bare `make_token!()` without a specific [`TokenType`] is provided.
    temp_state: Option<TokenizerState>,
    /// The token to be returned the next time around, before the main tokenizer loop is called.
    /// Always unset when used.
    cached_token: Option<Token<'a>>,
    /// Set in [`Tokenier::read_next()`] each time [`Tokenizer::read_next_inner()`] returns a valid
    /// token.
    last_token_type: Option<TokenType>,
    /// A buffer that's used if we need to return multiple contiguous ranges coalesced into one
    /// token. It is reset to `None` on each invocation of [`Tokenizer::read_next()`].
    buffer: Option<Cow<'a, [u8]>>,
    index: usize,
    line: u32,
    col: u32,
    /// A debug sanity check to ensure we never fall through the switch after calling
    /// [`Tokenizer::consume_char()`].
    char_consumed: bool,
}

#[derive(Debug)]
pub struct TokenizerError {
    pub kind: ErrorKind,
    pub line: u32,
    pub col: u32,
    pub len: u8,
}

#[derive(Debug, PartialEq)]
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
    /// We need a separate (temporary) state for variable name so we can properly tokenize an index
    /// after a variable name in a quoted context.
    ///
    /// We can handle `echo "$(echo foo)[1]"` just fine, but `echo "$foo[1]"` is context-sensitive
    /// as here we have an index but in `echo "foo[1]"` we have no index.
    VariableName,
    /// A temporary state, used to determine if a sequence of numbers is a file descriptor (such as
    /// in the case of `echo foo 1> /dev/null`) or a regular text token.
    FileDescriptor,
    /// A temporary state, used to track the fd portion of a fd redirect, as in `echo foo>&21`
    /// (starting at the `2`).
    FdRedirect,
    /// A temporary state for marking a word containing special characters triggering expansion
    /// and/or substitution, such as `*`.
    Glob,
    /// A temporary state indicating we are possibly completing a home directory expansion. If a
    /// tilde is followed by plain text, the entire thing is plain text but if it is followed by a
    /// character that indicates the end of a sequence/token then it is an expansion.
    HomeExpansion,
}

impl TokenizerState {
    #[inline]
    /// There are states that should never be pushed to [`Tokenizer::state`] and should instead only
    /// be assigned to [`Tokenizer::temp_state`]. This fn is only used for debug assertion purposes.
    fn is_temp_state(&self) -> bool {
        matches!(
            self,
            TokenizerState::VariableName
                | TokenizerState::FileDescriptor
                | TokenizerState::FdRedirect
                | TokenizerState::Glob
                | TokenizerState::HomeExpansion
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Text,
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
    /// A file descriptor such as `&2`
    FileDescriptor,
    /// The bare redirection operator `>`
    Redirection,
    /// The append redirection operator `>>`
    Append,
    /// The bare redirection operator `<`
    StdinRedirection,
    /// Coalesced whitespace. Currently just spaces.
    Whitespace,
    /// The comment, starting with the literal `#` and continuing until the end of the line.
    Comment,
    Backgrounding,
    /// Either a literal `&&` or a literal `and`.
    LogicalAnd,
    /// Either a literal `||` or a literal `or`. See also [`TokenType::Pipe`].
    LogicalOr,
    /// A literal `,` in the context of a brace expansion
    BraceSeparator,
    VariableName,
    /// The `&` before a target/source fd
    FdRedirection,
    /// A `~` at the start of a token in a context that indicates it should be expanded to the
    /// current user's home directory.
    HomeDirExpansion,
    /// A combination of one or more of any of the characters that trigger expansion within a token.
    /// May indicate the presence of `*` or `**`.
    Glob,
    /// A username following a `~` for home directory expansion.
    Username,
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

pub fn tokenize<'a>(input: &'a [u8]) -> Tokenizer<'a> {
    let tokenizer = Tokenizer::new(input);
    tokenizer
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
                Err(e) => return Err(e),
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
            temp_state: None,
            buffer: None,
            char_consumed: false,
            index: 0,
            line: 1,
            col: 1,
            cached_token: None,
            last_token_type: None,
        }
    }

    #[inline]
    /// Peeks the current state from the top of [`TokenizerState::state`] or returns
    /// [`TokenizerState::None`].
    fn state(&self) -> TokenizerState {
        self.temp_state
            .unwrap_or_else(|| self.state.last().cloned().unwrap_or(TokenizerState::None))
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

    #[inline]
    /// Appends a range of the current input to the internal buffer. If the buffer has not yet been
    /// initialized (i.e. this is the first specially-crafted input this loop), the passed input is
    /// copied to a new `Vec<u8>`, otherwise it is appended to whatever is already in the buffer.
    fn append(&mut self, input: &'a [u8]) {
        self.buffer = Some(match self.buffer.take() {
            Some(b) => {
                let mut vec = b.into_owned();
                vec.extend_from_slice(input);
                Cow::Owned(vec)
            }
            None => Cow::Borrowed(input),
        })
    }

    #[inline(never)]
    /// Returns an error pointing to the current line and column, with the column offset by the
    /// provided `offset`.
    fn error<T>(&self, kind: ErrorKind, len: usize, offset: i32) -> Result<T, TokenizerError> {
        Err(TokenizerError {
            kind,
            line: self.line,
            col: (TryInto::<i32>::try_into(self.col).unwrap()
                + TryInto::<i32>::try_into(offset).unwrap())
            .try_into()
            .unwrap(),
            len: len.try_into().unwrap(),
        })
    }

    #[inline]
    /// Bump the current read index and column. The line count isn't checked and should be handled
    /// separately!
    fn consume_char(&mut self) {
        self.index += 1;
        self.col += 1;
        self.char_consumed = true;
    }

    /// A wrapper around [`Tokenizer::read_next()`] that updates the internal state with the latest
    /// returned token type.
    fn read_next(&mut self) -> Result<Token<'a>, TokenizerError> {
        let result = self.read_next_inner();
        if let &Ok(ref token) = &result {
            self.last_token_type = Some(token.ttype.clone());
        }
        result
    }

    #[inline(always)]
    fn read_next_inner(&mut self) -> Result<Token<'a>, TokenizerError> {
        if let Some(token) = self.cached_token.take() {
            return Ok(token);
        }

        let mut loc = (self.line, self.col);
        let mut start = self.index;

        macro_rules! make_token {
            () => {{
                let ttype = match self.temp_state.take() {
                    Some(TokenizerState::VariableName) => TokenType::VariableName,
                    // TokenizerState::FileDescriptor only ends on > directly so it's not handled
                    // here, whereas TokenizerState::FdRedirect ends on whitespace so it is.
                    Some(TokenizerState::FdRedirect) => TokenType::FileDescriptor,
                    Some(TokenizerState::Glob) => TokenType::Glob,
                    Some(TokenizerState::HomeExpansion) => TokenType::Username,
                    _ => TokenType::Text,
                };
                make_token!(ttype)
            }};
            ($ttype:expr) => {{
                let text = match (self.buffer.take(), &self.input[start..self.index]) {
                    (Some(old), b"") => old,
                    (Some(old), new) => {
                        let mut text = old.into_owned();
                        text.extend_from_slice(new);
                        Cow::Owned(text)
                    }
                    (None, new) => Cow::Borrowed(new),
                };
                let token = Token {
                    ttype: $ttype,
                    text: text,
                    line: loc.0,
                    col: loc.1,
                };
                eprintln!(
                    "Returning token {:?} of type {:?} from state {:?}",
                    &String::from_utf8_lossy(&token.text),
                    token.ttype,
                    self.state()
                );
                // Make sure we never have a temporary state pushed on to the actual state stack.
                debug_assert!(
                    self.state.last().is_none() || !self.state.last().unwrap().is_temp_state()
                );
                {
                    #![allow(unused_assignments)]
                    start = self.index;
                }
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

        loop {
            if self.index == self.input.len() {
                if start == self.index && self.buffer.is_none() {
                    return self.error(ErrorKind::EndOfStream, 0, 0);
                } else {
                    return Ok(make_token!());
                }
            }
            self.char_consumed = false;

            /// A pull-based read that consumes the current character only if it is a match.
            macro_rules! read {
                (hex) => {
                    read!(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
                };
                ($pat:pat) => {
                    match self.read() {
                        None => Err(None),
                        #[allow(clippy::redundant_pattern)]
                        Some(c @ $pat) => {
                            self.consume_char();
                            // We expect the first predicate below to be a compile-time constant
                            if matches!(b'\n', $pat) && c == b'\n' {
                                self.line += 1;
                                self.col = 1;
                            }
                            Ok(c)
                        }
                        #[allow(unreachable_patterns)]
                        Some(c) => Err(Some(c)),
                    }
                };
            }

            /// Determines if we need to return what we have in the buffer before returning a new
            /// token type. The magic is actually in the `false` branch where we need to end certain
            /// contex-dependent tokenizer states before continuing; as such, every time
            /// `have_fragment!()` is called and returns false, [`TokenizerState::temp_state`] is
            /// reset.
            macro_rules! have_fragment {
                () => {{
                    if start != self.index || self.buffer.is_some() {
                        true
                    } else {
                        self.temp_state = None;
                        false
                    }
                }};
            }

            let c = self.input[self.index];
            eprintln!("c: {}, state: {:?}", c as char, self.state());
            match (c, self.state()) {
                // TODO: move escape handling to a function so we can keep it in cold storage
                (b'\\', _) => {
                    if self.peek().is_none() {
                        return self.error(ErrorKind::UnterminatedEscape, 1, 0);
                    }
                    // Unlike other handlers, the escape handler doesn't start a new state but
                    // rather appends to the old one.
                    if start != self.index {
                        self.append(&self.input[start..self.index]);
                    }
                    self.consume_char();
                    start = self.index;
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
                            let _hex1 = match read!(hex) {
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

                            match hex2 {
                                Some(_) => {
                                    let src = &self.input[self.index - 2..][..2];
                                    let src = std::str::from_utf8(src).unwrap();
                                    // We've already verified it's a valid hex value
                                    u8::from_str_radix(src, 16).unwrap()
                                }
                                None => {
                                    let src = &self.input[self.index - 1..][..1];
                                    let src = std::str::from_utf8(src).unwrap();
                                    // We've already verified it's a valid hex value
                                    u8::from_str_radix(src, 16).unwrap()
                                }
                            }
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
                                    None => self.error(ErrorKind::UnterminatedEscape, 2, -2),
                                    _ => self.error(ErrorKind::InvalidEscape, 3, -2),
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
                                    return self.error(
                                        ErrorKind::InvalidCodepoint,
                                        2 + hex_count,
                                        -2 - hex_count as i32,
                                    );
                                }
                            };

                            let mut bytes = String::from(codepoint).into_bytes();
                            self.buffer = Some(match self.buffer.take() {
                                Some(b) => {
                                    let mut text = b.into_owned();
                                    text.append(&mut bytes);
                                    Cow::Owned(text)
                                }
                                None => Cow::Owned(bytes),
                            });
                            start = self.index;
                            continue;
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
                                    None => self.error(ErrorKind::UnterminatedEscape, 2, -2),
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
                                None => self.error(
                                    ErrorKind::InvalidCodepoint,
                                    2 + hex_count,
                                    -2 - hex_count as i32,
                                )?,
                            };

                            let mut bytes = String::from(codepoint).into_bytes();
                            self.buffer = Some(match self.buffer.take() {
                                Some(b) => {
                                    let mut text = b.into_owned();
                                    text.append(&mut bytes);
                                    Cow::Owned(text)
                                }
                                None => Cow::Owned(bytes),
                            });
                            start = self.index;
                            continue;
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
                                return self.error(
                                    ErrorKind::InvalidAscii,
                                    1 + octal_count,
                                    -1 - octal_count as i32,
                                );
                            }
                            value as u8
                        }
                        b'\n' => {
                            // A literal new line after an escape is a continuation and should be
                            // interpreted as ignoring the new line altogether (though incrementing
                            // our inner line count).
                            if self.buffer.is_some() {
                                self.index -= 1;
                                let token = make_token!();
                                self.index += 1;
                                return Ok(token);
                            }
                            start = self.index;
                            loc = (self.line, self.col);
                            continue;
                        }
                        b'\r' => {
                            // Treat like an escape of a literal \n if followed by one
                            let mut token_start = self.index - 1;
                            if read!(b'\n').is_ok() {
                                if self.buffer.is_some() {
                                    std::mem::swap(&mut self.index, &mut token_start);
                                    let token = make_token!();
                                    std::mem::swap(&mut self.index, &mut token_start);
                                    return Ok(token);
                                }
                                start = self.index;
                                loc = (self.line, self.col);
                            } else {
                                // Otherwise continuing below will just map an escaped \r to itself,
                                // which is fine.
                            }
                            continue;
                        }
                        _ => {
                            // The escape evaluates to the next character itself.
                            // Instead of allocating, just consume the character and progress
                            // the loop. The character will be the start of a new token.
                            continue;
                        }
                    };

                    self.buffer = Some(match self.buffer.take() {
                        Some(b) => {
                            let mut text = b.into_owned();
                            text.push(byte);
                            Cow::Owned(text)
                        }
                        None => Cow::Owned(vec![byte]),
                    });
                    start = self.index;
                    continue;
                }
                (b'"', TokenizerState::DoubleQuote) => {
                    if have_fragment!() {
                        self.append(&self.input[start..self.index]);
                    }
                    self.consume_char();
                    self.state.pop();
                    start = self.index;
                    continue;
                }
                (b'\'', TokenizerState::SingleQuote) => {
                    if have_fragment!() {
                        self.append(&self.input[start..self.index]);
                    }
                    self.consume_char();
                    self.state.pop();
                    start = self.index;
                    continue;
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
                        self.temp_state = Some(TokenizerState::VariableName);
                    }
                    return Ok(dollar);
                }
                // `[` is only a special character immediately after a variable name or a subshell.
                (b'[', TokenizerState::VariableName) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    // TS::VariableName is automatically unset, so we just repeat instead of
                    // consuming the char.
                    continue;
                }
                (b'[', _) => {
                    if !have_fragment!()
                        && matches!(
                            self.last_token_type,
                            Some(TokenType::VariableName | TokenType::SubshellEnd)
                        )
                    {
                        self.consume_char();
                        self.state.push(TokenizerState::Index);
                        return Ok(make_token!(TokenType::IndexStart));
                    }
                }
                // These states have token types that must end on encountering punctuation.
                (c, TokenizerState::VariableName) if c.is_ascii_punctuation() => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                }
                // These states have token types that must end on encountering a path separator.
                (PATH_SEPARATOR, TokenizerState::VariableName | TokenizerState::HomeExpansion) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                }
                // Make sure we've handled everything to do with TS::VariableName before we reach
                // the ignore rule for TS::DoubleQuote.
                (_, TokenizerState::DoubleQuote) => {
                    // Just keep going
                }
                (b')', TokenizerState::Subshell) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.consume_char();
                    self.state.pop();
                    return Ok(make_token!(TokenType::SubshellEnd));
                }
                (b'}', TokenizerState::Brace) | (b']', TokenizerState::Index) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.consume_char();
                    self.state.pop();
                    let ttype = match c {
                        b'}' => TokenType::BraceEnd,
                        b']' => TokenType::IndexEnd,
                        _ => unreachable!(),
                    };
                    return Ok(make_token!(ttype));
                }
                (b'(' | b'{', _) => {
                    if have_fragment!() {
                        return Ok(make_token!());
                    }
                    self.consume_char();
                    let (ttype, state) = match c {
                        b'(' => (TokenType::SubshellStart, TokenizerState::Subshell),
                        b'{' => (TokenType::BraceStart, TokenizerState::Brace),
                        _ => unreachable!(),
                    };
                    self.state.push(state);
                    return Ok(make_token!(ttype));
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
                            return Ok(make_token!());
                        }
                        // We can merge the \r into the \n and treat as TT::EndOfLine
                        while read!(b'\r' | b'\n').is_ok() {}
                        return Ok(make_token!(TokenType::EndOfLine));
                    }
                    // Otherwise concatenate the \r to whatever text comes before/after
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
                        self.append(&self.input[start..self.index]);
                    }
                    self.state.push(TokenizerState::DoubleQuote);
                    self.consume_char();
                    start = self.index;
                    continue;
                }
                (b'\'', _) => {
                    if have_fragment!() {
                        self.append(&self.input[start..self.index]);
                    }
                    self.state.push(TokenizerState::SingleQuote);
                    self.consume_char();
                    start = self.index;
                    continue;
                }
                (b'&', _) => {
                    // The ampersand is heavily overloaded and can be a backgrounding symbol, a
                    // "logical and" operator, plain text, or a file descriptor prefix.
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
                    if matches!(
                        self.peek(),
                        Some(b' ' | b'\r' | b'\n' | b'|' | b';' | b'<' | b'>') | None
                    ) {
                        if have_fragment!() {
                            return Ok(make_token!());
                        }
                        self.consume_char();
                        return Ok(make_token!(TokenType::Backgrounding));
                    }
                    if !have_fragment!() && !matches!(self.last_token_type, Some(TokenType::Text)) {
                        self.consume_char();
                        let token = make_token!(TokenType::FdRedirection);
                        // Set us up to read more numbers as an fd (e.g. `cat <&0`)
                        self.temp_state = Some(TokenizerState::FdRedirect);
                        return Ok(token);
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
                (b'0'..=b'9', TokenizerState::None) => {
                    // This is possibly a file descriptor like in the case of the `1` in `echo foo 1>&2`
                    // It's only the case if the number is the start of the token.
                    if !have_fragment!() && !matches!(self.last_token_type, Some(TokenType::Text)) {
                        self.temp_state = Some(TokenizerState::FileDescriptor);
                    }
                    // Otherwise just append to the existing fragment and treat as whatever
                    // (probably text).
                }
                (b'>', TokenizerState::FileDescriptor) => {
                    // We have an actual file descriptor after entering TS::FileDescriptor upon
                    // encountering a numeric sequence above.
                    let token = make_token!(TokenType::FileDescriptor);
                    self.temp_state = None;
                    return Ok(token);
                    // We'll resume on '>' in TokenizerState::None
                }
                (b'>', _) => {
                    // This can be appended directly to the text, e.g. `echo foo>/dev/null`
                    if have_fragment!() {
                        return Ok(make_token!());
                    }

                    self.consume_char();
                    let ttype = match read!(b'>') {
                        // This is actually an append redirection
                        Ok(_) => TokenType::Append,
                        _ => TokenType::Redirection,
                    };
                    return Ok(make_token!(ttype));
                }
                (b'<', _) => {
                    // This can be only be an stdin redirection because fish doesn't support <<
                    if have_fragment!() {
                        return Ok(make_token!());
                    }

                    self.consume_char();
                    return Ok(make_token!(TokenType::StdinRedirection));
                }
                (b'#', _) => {
                    // A comment only starts if we're not in the middle of a token and continues
                    // until EOL or EOF, without supporting continuations. Since we have no facility
                    // for comment contents to be retrieved, we also won't handle escapes, \r, etc.
                    if have_fragment!() || matches!(self.last_token_type, Some(TokenType::Text)) {
                        // Comment comes in the middle of text, so we're going to consider this to
                        // be regular text appended/prepended to whatever was before/comes after.
                        self.consume_char();
                        continue;
                    }

                    while !matches!(self.read(), None | Some(b'\n')) {
                        self.consume_char();
                    }
                    return Ok(make_token!(TokenType::Comment));
                }
                (b'~', _) => {
                    // Fish overloads ~ in a way that it's impossible to determine at tokenization
                    // if it's going to be a special symbol or not, as it may expand to a different
                    // user's home directory or be kept as plain text. We aren't going to support
                    // that and will be classifying it as an expansion or plain text definitively.
                    // See https://github.com/fish-shell/fish-shell/issues/9340
                    if have_fragment!() || matches!(self.last_token_type, Some(TokenType::Text)) {
                        // If it's not at the start of a string, treat it as plain text.
                        self.consume_char();
                        continue;
                    }

                    self.consume_char();
                    let token = make_token!(TokenType::HomeDirExpansion);
                    self.temp_state = Some(TokenizerState::HomeExpansion);
                    return Ok(token);
                }
                (b'*', _) => {
                    // This isn't just regular text, it's a glob/expansion. Don't start a new token,
                    // but convert the entirety of the current token to a glob.
                    self.temp_state = Some(TokenizerState::Glob);
                    // Don't end the token here; keep reading until the natural end of the sequence,
                    // when/where we'll return a TT::Glob instead.
                }
                _ => {
                    // No special treatment.
                }
            }

            // We aren't allowed to ever call `consume_char()` (which a successful `read!()` calls)
            // and also fall through the switch to here.
            assert!(!self.char_consumed, "self.consume_char() was called!");

            match self.read() {
                Some(b'\n') => {
                    self.index += 1;
                    self.line += 1;
                    self.col = 1;
                }
                c => {
                    self.index += 1;
                    self.col += 1;
                    if let Some(c) = c {
                        eprintln!("Consuming default token {} ({})", c as char, c);
                    }
                }
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

impl Token<'_> {
    #[inline]
    pub fn matches_text(&self, text: &[u8]) -> bool {
        self.ttype == TokenType::Text && &*self.text == text
    }

    #[inline]
    pub fn is_text(&self) -> bool {
        self.ttype == TokenType::Text
    }

    #[inline]
    pub fn is_space(&self) -> bool {
        self.ttype == TokenType::Whitespace
    }
}
