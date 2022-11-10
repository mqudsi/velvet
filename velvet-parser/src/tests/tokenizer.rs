use crate::find;
use crate::tokenizer::*;

#[test]
/// Verify how and when/where new lines are counted.
fn count_lines() {
    let input = b"hello\nworld\n\ngoodbye";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let new_line = tokens
        .find(|t| t.ttype == TokenType::EndOfLine)
        .expect("Could not locate first literal EOL");
    assert_eq!(
        new_line.line, 1,
        "Line no. should be incremented *after* a \\n, not on it!"
    );
    assert_eq!(new_line.col, 6, "Incorrect \\n column!");

    let world = tokens
        .find(|t| t.ttype == TokenType::Text && matches!(&*t.text, b"world"))
        .expect("Could not find a match for literal text \"world\"");
    assert_eq!(world.line, 2);
    assert_eq!(world.col, 1, "Column count not reset at new line!");

    let goodbye = tokens
        .find(|t| t.ttype == TokenType::Text && matches!(&*t.text, b"goodbye"))
        .expect("Could not find a match for literal text \"goodbye\"");
    assert_eq!(goodbye.line, 4, "Adjacent new lines not counted!");
}

#[test]
/// Assert that a quoted subshell has its dollar, subshell, and index symbols reported correctly.
fn quoted_subshell_parentheses() {
    let input = br#"set str "hello $(echo "world")[1]""#;
    let mut tokens = tokenize(input).map(|t| t.unwrap());

    let tok = tokens
        .find(|t| t.ttype == TokenType::Dollar)
        .expect("Could not find token for dollar-prefixed subshell!");
    assert!(matches!(&*tok.text, b"$"), "Dollar symbol not a literal $");
    let tok = tokens
        .find(|t| t.ttype == TokenType::SubshellStart)
        .expect("Could not find token for opening parenthesis in quoted subshell!");
    assert!(matches!(&*tok.text, b"("), "SubshellStart not a ( symbol!");
    let tok = tokens
        .find(|t| t.ttype == TokenType::SubshellEnd)
        .expect("Could not find token for closing parenthesis in quoted subshell!");
    assert!(matches!(&*tok.text, b")"), "SubshellEnd not a ) symbol!");
    let tok = tokens
        .find(|t| t.ttype == TokenType::IndexStart)
        .expect("Could not find token for opening index in quoted subshell!");
    assert!(matches!(&*tok.text, b"["), "IndexStart not a [ symbol!");
    let tok = tokens
        .find(|t| t.ttype == TokenType::IndexEnd)
        .expect("Could not find token for closing index in quoted subshell!");
    assert!(matches!(&*tok.text, b"]"), "IndexEnd not a ] symbol!");
}

fn unbalanced_closing_symbol<const C: u8>() {
    let mut input = "echo ".to_owned().into_bytes();
    input.push(C);
    let mut tokens = tokenize(&input);

    while let Some(x) = tokens.next() {
        match x {
            Ok(_) => continue,
            // Matching against a const parameter directly as a structural member is not yet
            // supported. See https://rust-lang.github.io/rfcs/1445-restrict-constants-in-patterns.html
            Err(TokenizerError {
                kind: ErrorKind::UnexpectedSymbol { symbol },
                line: 1,
                col: 6,
                ..
            }) if symbol == C => return,
            Err(e) => panic!("Unexpected tokenizer error {e:?}"),
        }
    }
    panic!("Did not find expected UnexpectedSymbol error!");
}

#[test]
fn brace_unbalanced_closing() {
    unbalanced_closing_symbol::<b'}'>();
}

#[test]
fn subshell_unbalanced_closing() {
    unbalanced_closing_symbol::<b')'>();
}

#[test]
/// Assert that an unbalanced closing index is not a tokenizer error.
/// fish does not treat it the same way it does an unbalanced closing brace or parenthesis.
fn index_unbalanced_closing() {
    let input = b"echo ]";
    let mut tokens = tokenize(input);

    // It should be returned as text, not a symbol.
    let tok = tokens
        .last()
        .unwrap()
        .expect("We should not get a TokenizerError from a standalone closing ]");
    assert_eq!(tok.ttype, TokenType::Text);
    assert_eq!(&*tok.text, b"]");
}

#[test]
/// A `&` is a backgrounding token if it isn't followed by another `&` and is the last token in the
/// input or comes before whitespace, a pipe, or a semicolon.
fn backgrounding_symbol() {
    // These should all be detected as backgrounding symbols
    for input in [b"echo &".as_slice(), b"echo &; ..", b"echo &|"] {
        let tok = tokenize(input).nth(2).unwrap().unwrap();
        assert_eq!(tok.ttype, TokenType::Backgrounding);
        assert_eq!(&*tok.text, b"&", "Unexpected text in backgrounding symbol!");
    }

    // While none of these should be detected as background symbols
    for input in [b"echo &&".as_slice(), b"echo hello&not"] {
        let tok = tokenize(input)
            .map(Result::unwrap)
            .find(|tok| tok.ttype == TokenType::Backgrounding);
        assert!(
            matches!(tok, None),
            "Backgrounding symbol should not have been detected!"
        );
    }
}

#[test]
fn variable_name() {
    let input = b"echo $foo";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let tok = tokens.nth(2).unwrap();
    assert_eq!(tok.ttype, TokenType::Dollar);

    let tok = tokens.next().unwrap();
    assert_eq!(tok.ttype, TokenType::VariableName);
    assert_eq!(&*tok.text, b"foo");
}

#[test]
fn variable_index() {
    let input = b"echo $foo[1]";
    let mut tokens = tokenize(input).map(Result::unwrap);

    tokens
        .find(|t| t.ttype == TokenType::VariableName)
        .expect("Couldn't find a VariableName token!");

    let tok = tokens.next().unwrap();
    assert_eq!(tok.ttype, TokenType::IndexStart);
    assert_eq!(&*tok.text, b"[");

    let tok = tokens.next().unwrap();
    assert_eq!(tok.ttype, TokenType::Text);
    assert_eq!(&*tok.text, b"1");

    let tok = tokens.next().unwrap();
    assert_eq!(tok.ttype, TokenType::IndexEnd);
    assert_eq!(&*tok.text, b"]");
}

#[test]
/// Ensure variable names are found in a quoted context.
fn quoted_variable_name() {
    let input = br#"echo "hello $world""#;
    let mut tokens = tokenize(input).map(Result::unwrap);

    tokens
        .find(|t| t.ttype == TokenType::Dollar)
        .expect("Couldn't find dollar symbol in a quoted context!");
    let tok = tokens.next().expect("Expected a variable name!");
    assert_eq!(tok.ttype, TokenType::VariableName);
    assert_eq!(&*tok.text, b"world");
}

#[test]
/// Ensure variable names and indexes in a quoted context are found.
fn quoted_variable_index() {
    let input = br#"echo "hello $world[1]""#;
    let mut tokens = tokenize(input).map(Result::unwrap);

    tokens
        .find(|t| t.ttype == TokenType::VariableName)
        .expect("Couldn't find variable name in a quoted context!");
    let tok = tokens.next().unwrap();
    assert_eq!(tok.ttype, TokenType::IndexStart);
    assert_eq!(&*tok.text, b"[");

    let tok = tokens.next().unwrap();
    assert_eq!(tok.ttype, TokenType::Text);
    assert_eq!(&*tok.text, b"1");

    let tok = tokens.next().unwrap();
    assert_eq!(tok.ttype, TokenType::IndexEnd);
    assert_eq!(&*tok.text, b"]");
}

#[test]
/// Assert that no empty variable name is returned when it's not present.
fn dollar_no_variable_name() {
    assert!(
        tokenize(b"echo $ not_a_var_name")
            .map(Result::unwrap)
            .find(|t| t.ttype == TokenType::VariableName)
            .is_none(),
        "Detected a variable name after a space"
    );

    assert!(
        tokenize(b"echo $(subshell is not a var)")
            .map(Result::unwrap)
            .find(|t| t.ttype == TokenType::VariableName)
            .is_none(),
        "Detected a variable name after a quoted subshell"
    );
}

#[test]
/// Ensure a `$(subshell)` is detected outside of a quoted context, too.
fn dollar_subshell() {
    let mut tokens = tokenize(b"echo $(echo hi)").map(Result::unwrap);

    let tok = tokens
        .find(|t| t.ttype == TokenType::Dollar)
        .expect("Couldn't find the start of the $ expression");

    let tok = tokens.next().unwrap();
    assert_eq!(tok.ttype, TokenType::SubshellStart);
    assert_eq!(&*tok.text, b"(");

    let tok = tokens.last().unwrap();
    assert_eq!(tok.ttype, TokenType::SubshellEnd);
    assert_eq!(&*tok.text, b")");
}

#[test]
fn cr_in_text() {
    let input = b"echo hello\rworld";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let tok = tokens.last().unwrap();
    assert_eq!(tok.ttype, TokenType::Text);
    assert_eq!(&*tok.text, b"hello\rworld");
}

#[test]
fn cr_before_nl() {
    let input = b"echo hello\r\n";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let tok = tokens.last().unwrap();
    assert_eq!(tok.ttype, TokenType::EndOfLine);
    assert_eq!(&*tok.text, b"\r\n");
}

#[test]
fn pipe_simple() {
    let input = b"echo hello | cat";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let tok = tokens
        .find(|t| t.ttype == TokenType::Pipe)
        .expect("Did not parse a pipe symbol!");
    assert_eq!(&*tok.text, b"|");

    tokenize(b"echo hello|cat")
        .map(Result::unwrap)
        .find(|t| t.ttype == TokenType::Pipe)
        .unwrap();

    tokenize(b"echo hello| cat")
        .map(Result::unwrap)
        .find(|t| t.ttype == TokenType::Pipe)
        .unwrap();

    tokenize(b"echo hello|;")
        .map(Result::unwrap)
        .find(|t| t.ttype == TokenType::Pipe)
        .unwrap();
}

#[test]
fn logical_or() {
    let input = b"true || false";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let tok = tokens
        .find(|t| t.ttype == TokenType::LogicalOr)
        .expect("Did not parse a logical or symbol!");
    assert_eq!(&*tok.text, b"||");

    tokenize(b"true||false")
        .map(Result::unwrap)
        .find(|t| t.ttype == TokenType::LogicalOr)
        .unwrap();
}

#[test]
/// Assert leading whitespace is not stripped or elided from the tokenization.
fn whitespace_leading() {
    let input = b" echo exclude this from history";
    let tok = tokenize(input).map(Result::unwrap).nth(0).unwrap();

    assert_eq!(tok.ttype, TokenType::Whitespace);
}

#[test]
/// Assert whitespace is always coalesced
fn whitespace_coalesce_spaces() {
    let input = b" echo  exclude this from history   ";
    let mut tokens = tokenize(input).map(Result::unwrap);

    // Count maximum consecutive `TokenType::Whitespace` tokens
    let (max_consecutive, _) = tokens.fold((0, 0), |(max, acc), tok| match tok.ttype {
        TokenType::Whitespace => (max.max(acc + 1), acc + 1),
        _ => (max, 0),
    });
    assert_eq!(max_consecutive, 1);
}

#[test]
/// Assert whitespace is always coalesced
fn whitespace_coalesce_lines() {
    let input = b"echo alpha\n\necho bravo\necho charlie";
    let mut tokens = tokenize(input).map(Result::unwrap);

    // Count maximum consecutive `TokenType::EndOfLine` tokens
    let (max_consecutive, _) = tokens.fold((0, 0), |(max, acc), tok| match tok.ttype {
        TokenType::EndOfLine => (max.max(acc + 1), acc + 1),
        _ => (max, 0),
    });
    assert_eq!(max_consecutive, 1);
}

#[test]
/// Verify the behavior of general (single-char) escapes that transform the input into something
/// else (i.e. don't return the same character).
fn escape_basic() {
    let mut tokens = tokenize(b"\\a").map(Result::unwrap);
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
    // form feed
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
    assert_eq!(&*token.text, &[0x0c]);
    // escape
    let token = tokens.next().unwrap();
    assert!(matches!(
        token,
        Token {
            ttype: TokenType::Text,
            text: _,
            line: 1,
            col: 4
        }
    ));
    assert_eq!(&*token.text, &[0x1b]);
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
    assert_eq!(&*token.text, b"\x42");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    // Uppercase \X escape
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"\xFF");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 5);

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
    assert_eq!(&*token.text, b"\x04");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    // The z in between the two hex escapes
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"z");
    assert_eq!(token.col, 4);

    // Lowercase \x escape + hex-like third digit
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"\x1F");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 5);

    // Trailing text
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"banana");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 9);

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
    assert_eq!(&*token.text, &[0o123]);
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"4");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 5);

    assert!(tokens.next().is_none());
}

#[test]
fn escape_octal_short() {
    let mut tokens = tokenize(br#"\129"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, &[0o12]);
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"9");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 4);

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
    assert_eq!(&*token.text, "\u{1234}".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"56");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 7);
}

#[test]
/// Verify 4-digit unicode with less than four hex-like characters
fn escape_unicode_four_short() {
    let mut tokens = tokenize(br#"\u123go"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, "\u{123}".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"go");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 6);
}

#[test]
/// Verify 4-digit unicode with no valid hex characters
fn escape_unicode_four_invalid_hex() {
    let mut tokens = tokenize(br#"\ug123"#);

    let error = tokens.next().unwrap()
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

    let error = tokens.next().unwrap()
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
    assert_eq!(&*token.text, "\u{10FFFF}".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"ABC");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 11);
}

#[test]
/// Verify 8-digit unicode with less than eight hex-like characters
fn escape_unicode_eight_short() {
    let mut tokens = tokenize(br#"\U12345go"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, "\u{12345}".as_bytes());
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 1);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"go");
    assert_eq!(token.line, 1);
    assert_eq!(token.col, 8);
}

#[test]
/// Verify 8-digit unicode with no valid hex characters
fn escape_unicode_eight_invalid_hex() {
    let mut tokens = tokenize(br#"\Ug123"#);

    let error = tokens.next().unwrap()
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

    let error = tokens.next().unwrap()
        .expect_err("No codepoint was provided");
    assert_eq!(error.kind, ErrorKind::UnterminatedEscape);
    assert_eq!(error.len, 2);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify illegal values for Unicode Scalar Values raise an error
fn escape_unicode_illegal_value() {
    let error = tokenize(br#"\uD800"#).next().unwrap()
        .expect_err("USV range is 0x0..=0xD7FF and 0xE000..=10FFFF!");
    assert_eq!(error.kind, ErrorKind::InvalidCodepoint);
    assert_eq!(error.len, 6);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}

#[test]
/// Verify illegal values for Unicode Scalar Values raise an error
fn escape_unicode_out_of_range() {
    let error = tokenize(br#"\U110000"#).next().unwrap()
        .expect_err("USV range is 0x0..=0xD7FF and 0xE000..=10FFFF!");
    assert_eq!(error.kind, ErrorKind::InvalidCodepoint);
    assert_eq!(error.len, 8);
    assert_eq!(error.line, 1);
    assert_eq!(error.col, 1);
}
