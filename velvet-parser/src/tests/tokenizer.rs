use crate::find;
use crate::tokenizer::*;

#[test]
/// Verify how and when/where new lines are counted.
fn new_line_counting() {
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
            Err(TokenizerError::UnexpectedSymbol {
                symbol: c,
                line: 1,
                col: 6,
            }) if c == C => return,
            Err(e) => panic!("Unexpected tokenizer error {e:?}"),
        }
    }
    panic!("Did not find expected UnexpectedSymbol error!");
}

#[test]
fn unbalanced_closing_brace() {
    unbalanced_closing_symbol::<b'}'>();
}

#[test]
fn unbalanced_closing_subshell() {
    unbalanced_closing_symbol::<b')'>();
}

#[test]
/// Assert that an unbalanced closing index is not a tokenizer error.
/// fish does not treat it the same way it does an unbalanced closing brace or parenthesis.
fn unbalanced_closing_index() {
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
fn basic_variable_name() {
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
fn no_variable_name() {
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
fn cr_new_lines() {
    let input = b"echo hello\r\n";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let tok = tokens.last().unwrap();
    assert_eq!(tok.ttype, TokenType::EndOfLine);
    assert_eq!(&*tok.text, b"\r\n");
}

#[test]
fn simple_pipe() {
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
fn leading_whitespace() {
    let input = b" echo exclude this from history";
    let tok = tokenize(input).map(Result::unwrap).nth(0).unwrap();

    assert_eq!(tok.ttype, TokenType::Whitespace);
}

#[test]
/// Assert whitespace is always coalesced
fn coalesced_whitespace() {
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
fn coalesced_new_lines() {
    let input = b"echo alpha\n\necho bravo\necho charlie";
    let mut tokens = tokenize(input).map(Result::unwrap);

    // Count maximum consecutive `TokenType::EndOfLine` tokens
    let (max_consecutive, _) = tokens.fold((0, 0), |(max, acc), tok| match tok.ttype {
        TokenType::EndOfLine => (max.max(acc + 1), acc + 1),
        _ => (max, 0),
    });
    assert_eq!(max_consecutive, 1);
}
