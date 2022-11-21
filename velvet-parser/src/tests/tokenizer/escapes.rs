use crate::tokenizer::*;

#[test]
/// Verify the behavior of general (single-char) escapes that transform the input into something
/// else (i.e. don't return the same character).
fn escape_basic() {
    let mut tokens = tokenize(br#"\a"#).map(Result::unwrap);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, &[0x07], "alert escape failure");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);
    let token = tokens.next();
    assert!(
        token.is_none(),
        "Expected no other token, received {:#?}",
        token.unwrap()
    );

    let mut tokens = tokenize(br#" \f\e \n"#).map(Result::unwrap);
    assert_eq!(tokens.next().unwrap().ttype, TokenType::Whitespace);
    // form feed and escape
    let token = tokens.next().unwrap();
    assert!(matches!(
        token,
        Token {
            ttype: TokenType::Text,
            text: _,
            line: 1,
            col: 2
        }
    ));
    assert_eq!(&*token.text, &[0x0c, 0x1b]);
    // space
    assert_eq!(tokens.next().unwrap().ttype, TokenType::Whitespace);
    // new line
    let token = tokens.next().unwrap();
    assert!(matches!(
        token,
        Token {
            ttype: TokenType::Text,
            text: _,
            line: 1,
            col: 7
        }
    ));
    assert_eq!(&*token.text, &[b'\n']);
    // end
    assert!(tokens.next().is_none());

    for escape in [
        (br#"\r"#, b'\r', "carriage return"),
        (br#"\t"#, b'\t', "tab"),
        (br#"\v"#, b'\x0b', "vertical tab"),
    ] {
        let token = tokenize(escape.0).next().unwrap().unwrap();
        assert_eq!(
            token.ttype,
            TokenType::Text,
            "Unexpected TokenType for {}",
            escape.2
        );
        assert_eq!(
            &*token.text,
            &[escape.1],
            "Incorrect escape value for {}",
            escape.2
        );
        assert_eq!(token.line, 1, "Incorrect line for {}", escape.2);
        assert_eq!(token.col, 1, "Incorrect column for {}", escape.2);
    }
}

#[test]
/// Validate hex escape decoding
fn escape_hex() {
    let mut tokens = tokenize(br#"\x42\XFF \x1"#).map(Result::unwrap);

    // Lowercase \x escape
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&token.text[..1], b"\x42");
    // Uppercase \X escape
    assert_eq!(&token.text[1..], b"\xFF");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    // Whitespace
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 9);

    // Single-character escape
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"\x01");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 10);
}

#[test]
/// Validate hex escape decoding with adjacent non-escape characters.
fn escape_hex_adjacent() {
    let mut tokens = tokenize(br#"\X4z\x1Fbanana"#).map(Result::unwrap);

    // Uppercase \X escape + non-hex second digit
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"\x04z\x1Fbanana");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    assert!(tokens.next().is_none());
}

#[test]
fn escape_dangling() {
    let mut tokens = tokenize(b" \\");
    assert_eq!(tokens.next().unwrap().unwrap().ttype, TokenType::Whitespace);
    let error = tokens
        .next()
        .unwrap()
        .expect_err("Expected an error due to bare escape!");
    assert!(matches!(error.kind, ErrorKind::UnterminatedEscape));
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 2);

    // TODO: Support continuing or halting after an error rather than returning the same
    // assert!(tokens.next().is_none());
}

#[test]
fn escape_hex_invalid() {
    let mut tokens = tokenize(b"\\xz");
    let error = tokens
        .next()
        .unwrap()
        .expect_err("Expected an error due to invalid hex value!");
    assert!(matches!(error.kind, ErrorKind::InvalidEscape));
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
fn escape_octal() {
    let token = tokenize(br#"\123"#).next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, &[0o123]);
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);
}

#[test]
fn escape_octal_adjacent() {
    let mut tokens = tokenize(br#"\1234"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(token.text[0], 0o123);
    assert_eq!(&token.text[1..], b"4");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    assert!(tokens.next().is_none());
}

#[test]
fn escape_octal_short() {
    let mut tokens = tokenize(br#"\129"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(token.text[0], 0o12);
    assert_eq!(&token.text[1..], b"9");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    assert!(tokens.next().is_none());
}

#[test]
fn escape_octal_ascii_range_min() {
    let token = tokenize(br#"\000"#)
        .next()
        .unwrap()
        .expect("No error because octal is in ASCII range");
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, &[0o000]);
}

#[test]
fn escape_octal_ascii_range_max() {
    let token = tokenize(br#"\177"#)
        .next()
        .unwrap()
        .expect("No error because octal is in ASCII range");
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, &[0o177]);
}

#[test]
fn escape_octal_ascii_range_exceeded() {
    let error = tokenize(br#"\200"#)
        .next()
        .unwrap()
        .expect_err("Error because octal is out of ASCII range");
    assert!(matches!(error.kind, ErrorKind::InvalidAscii));
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify that we don't panic within the code itself when the range for a u8 is exceeded when
/// decoding an octal escape. The error should otherwise be the same as in
/// [`escape_octal_ascii_range_exceeded()'].
fn escape_octal_u8_range_exceeded() {
    // Max octal value for a u8 is o377
    let error = tokenize(br#"\400"#)
        .next()
        .unwrap()
        .expect_err("Error because octal is out of u8 range");
    assert!(matches!(error.kind, ErrorKind::InvalidAscii));
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify basic 4-digit unicode escape support
fn escape_unicode_four() {
    let token = tokenize(br#"\u1234"#).next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, "\u{1234}".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);
}

#[test]
/// Verify 4-digit unicode with hex-like characters after
fn escape_unicode_four_adjacent() {
    let mut tokens = tokenize(br#"\u123456"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, "\u{1234}56".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);
}

#[test]
/// Verify 4-digit unicode with less than four hex-like characters
fn escape_unicode_four_short() {
    let mut tokens = tokenize(br#"\u123go"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, "\u{123}go".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);
}

#[test]
/// Verify 4-digit unicode with no valid hex characters
fn escape_unicode_four_invalid_hex() {
    let mut tokens = tokenize(br#"\ug123"#);

    let error = tokens
        .next()
        .unwrap()
        .expect_err("An invalid hex value was inserted");
    assert_eq!(error.kind, ErrorKind::InvalidEscape);
    assert_eq!(error.len, 3);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify basic 4-digit unicode with no payload
fn escape_unicode_four_incomplete() {
    let mut tokens = tokenize(br#"\u"#);

    let error = tokens
        .next()
        .unwrap()
        .expect_err("No codepoint was provided");
    assert_eq!(error.kind, ErrorKind::UnterminatedEscape);
    assert_eq!(error.len, 2);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify basic 8-digit unicode escape support
fn escape_unicode_eight() {
    let token = tokenize(br#"\U10FFFF"#).next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, "\u{10FFFF}".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);
}

#[test]
/// Verify 8-digit unicode with hex-like characters after
fn escape_unicode_eight_adjacent() {
    let mut tokens = tokenize(br#"\U0010FFFFABC"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);
    assert_eq!(&*token.text, "\u{10FFFF}ABC".as_bytes());
}

#[test]
/// Verify 8-digit unicode with less than eight hex-like characters
fn escape_unicode_eight_short() {
    let mut tokens = tokenize(br#"\U12345go"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, "\u{12345}go".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);
}

#[test]
/// Verify 8-digit unicode with no valid hex characters
fn escape_unicode_eight_invalid_hex() {
    let mut tokens = tokenize(br#"\Ug123"#);

    let error = tokens
        .next()
        .unwrap()
        .expect_err("An invalid hex value was inserted");
    assert_eq!(error.kind, ErrorKind::InvalidEscape);
    assert_eq!(error.len, 3);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify basic 8-digit unicode with no payload
fn escape_unicode_eight_incomplete() {
    let mut tokens = tokenize(br#"\U"#);

    let error = tokens
        .next()
        .unwrap()
        .expect_err("No codepoint was provided");
    assert_eq!(error.kind, ErrorKind::UnterminatedEscape);
    assert_eq!(error.len, 2);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify illegal values for Unicode Scalar Values raise an error
fn escape_unicode_illegal_value() {
    let error = tokenize(br#"\uD800"#)
        .next()
        .unwrap()
        .expect_err("USV range is 0x0..=0xD7FF and 0xE000..=10FFFF!");
    assert_eq!(error.kind, ErrorKind::InvalidCodepoint);
    assert_eq!(error.len, 6);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify illegal values for Unicode Scalar Values raise an error
fn escape_unicode_out_of_range() {
    let error = tokenize(br#"\U110000"#)
        .next()
        .unwrap()
        .expect_err("USV range is 0x0..=0xD7FF and 0xE000..=10FFFF!");
    assert_eq!(error.kind, ErrorKind::InvalidCodepoint);
    assert_eq!(error.len, 8);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify EOL slash is treated as a continuation
fn escape_line_continuation() {
    let mut tokens = tokenize(b"hello cat\\\natonic").map(Result::unwrap);

    tokens.next().unwrap();
    tokens.next().unwrap();
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"cat");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"atonic");
    assert_eq!(token.line, 2);
    assert_eq!(token.col, 1);
}

#[test]
/// Verify EOL slash is treated as a continuation but doesn't eat subsequent whitespace.
fn escape_line_continuation_whitespace() {
    let mut tokens = tokenize(b"hello cat\\\n  dog").map(Result::unwrap);

    tokens.next().unwrap();
    tokens.next().unwrap();
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"cat");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);
    assert_eq!(token.line, 2);
    assert_eq!(token.col, 1);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"dog");
    assert_eq!(token.line, 2);
    assert_eq!(token.col, 3);
}

#[test]
/// Verify escaped CR,NL is treated as a continuation
fn escape_cr_line_continuation() {
    let mut tokens = tokenize(b"hello cat\\\r\natonic").map(Result::unwrap);

    tokens.next().unwrap();
    tokens.next().unwrap();
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"cat");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"atonic");
    assert_eq!(token.line, 2);
    assert_eq!(token.col, 1);
}

#[test]
/// Verify escaped CR not followed by NL is treated as a plain CR, and that the CR is prepended to
/// the same token as whatever text follows.
fn escape_cr() {
    let mut tokens = tokenize(b"\\\rhello").map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"\rhello");
}

#[test]
/// Verify default escapes evaluate to the escape character itself, prepended to whatever text
/// follows (if any).
fn escape_default() {
    let token = tokenize(br#"\\"#).next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, br#"\"#);
    assert_eq!(token.col, 1);

    let token = tokenize(br#"\kombat"#).next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, br#"kombat"#);
    assert_eq!(token.col, 1);
}
