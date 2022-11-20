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
    let tokens = tokenize(input);

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
fn ampersand_backgrounding() {
    // These should all be detected as backgrounding symbols
    for input in [b"echo &".as_slice(), b"echo &; ..", b"echo &|"] {
        let tok = tokenize(input).nth(2).unwrap().unwrap();
        assert_eq!(tok.ttype, TokenType::Backgrounding);
        assert_eq!(&*tok.text, b"&", "Unexpected text in backgrounding symbol!");
    }
}

#[test]
fn ampersand_plaintext() {
    let input = b"echo hello&not";
    let mut tokens = tokenize(input).skip(2).map(Result::unwrap);

    // Depending on whether we stick to one way of tokenizing or the other, it is fair game for the
    // three text components "hello", "&", and "not" to be returned as one or multiple tokens.
    while let Some(t) = tokens.next() {
        assert_eq!(
            t.ttype,
            TokenType::Text,
            "{}: {} tokenized as {:?} not Text!",
            std::str::from_utf8(input).unwrap(),
            std::str::from_utf8(&*t.text).unwrap(),
            t.ttype
        );
    }
}

#[test]
fn ampersand_plaintext_after_escape() {
    let input = br#"echo hell\o&friend"#;
    let tokens = tokenize(input).skip(2).map(Result::unwrap);

    let text = tokens
        .take_while(|t| t.ttype == TokenType::Text)
        .fold(Vec::new(), |mut acc, t| {
            acc.extend_from_slice(&*t.text);
            acc
        });
    assert_eq!(text, b"hello&friend");
}

#[test]
fn ampersand_plaintext_after_hex_escape() {
    let input = br#"echo hell\x6F&friend"#;
    let tokens = tokenize(input).skip(2).map(Result::unwrap);

    let text = tokens
        .take_while(|t| t.ttype == TokenType::Text)
        .fold(Vec::new(), |mut acc, t| {
            acc.extend_from_slice(&*t.text);
            acc
        });
    assert_eq!(text, b"hello&friend");
}

#[test]
/// `&&` is always a logical and operator, no matter if it is at the beginning, middle, or end of a
/// token.
fn ampersand_logical_and() {
    // These should all be detected as logical and operators
    for input in [b"echo &&".as_slice(), b"echo a&&b; ..", b"true&&false"] {
        let tok = tokenize(input)
            .map(Result::unwrap)
            .find(|t| t.ttype == TokenType::LogicalAnd);
        assert!(
            tok.is_some(),
            "Did not parse && in statement `{}` as logical and!",
            std::str::from_utf8(input).unwrap()
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
/// Verify variables are ended at certain symbols.
fn variable_path_separated() {
    let mut tokens = tokenize(b"foo/$var1/$var2/$bar.txt").map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"foo/");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Dollar);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::VariableName);
    assert_eq!(&*token.text, b"var1");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"/");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Dollar);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::VariableName);
    assert_eq!(&*token.text, b"var2");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"/");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Dollar);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::VariableName);
    assert_eq!(&*token.text, b"bar");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b".txt");
}

#[test]
/// Verify variables are ended at certain symbols.
fn variable_quote_interpolation() {
    let mut tokens = tokenize(br#"$foo"quoted$var"$bar"#).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Dollar);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::VariableName);
    assert_eq!(&*token.text, b"foo");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::DoubleQuote);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"quoted");
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Dollar);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::VariableName);
    assert_eq!(&*token.text, b"var");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::DoubleQuote);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Dollar);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::VariableName);
    assert_eq!(&*token.text, b"bar");
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

    tokens.find(|t| t.ttype == TokenType::Dollar)
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
    let tokens = tokenize(input).map(Result::unwrap);

    let tok = tokens.last().unwrap();
    assert_eq!(tok.ttype, TokenType::Text);
    assert_eq!(&*tok.text, b"hello\rworld");
}

#[test]
fn cr_before_nl() {
    let input = b"echo hello\r\n";
    let tokens = tokenize(input).map(Result::unwrap);

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
    let tokens = tokenize(input).map(Result::unwrap);

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
    let tokens = tokenize(input).map(Result::unwrap);

    // Count maximum consecutive `TokenType::EndOfLine` tokens
    let (max_consecutive, _) = tokens.fold((0, 0), |(max, acc), tok| match tok.ttype {
        TokenType::EndOfLine => (max.max(acc + 1), acc + 1),
        _ => (max, 0),
    });
    assert_eq!(max_consecutive, 1);
}

#[test]
/// Verify that tokens containing glob-match characters are classified as globs, not text.
fn expansion_glob_token() {
    let token = tokenize(b"a*c").next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Glob);
    assert_eq!(&*token.text, b"a*c");
}

#[test]
/// Verify that tokens with multiple glob characters are not broken up into separate tokens.
fn expansion_glob_token_multi() {
    let token = tokenize(b"a*c*e").next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Glob);
    assert_eq!(&*token.text, b"a*c*e");
}

#[test]
/// Verify globs are not split up on path separators
fn expansion_glob_multiple_paths() {
    let token = tokenize(b"ab*/foo*.txt").next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Glob);
    assert_eq!(&*token.text, b"ab*/foo*.txt");
}

#[test]
/// Verify that a tilde at the start of a string is considered a home directory expansion.
fn expansion_tilde_home_dir() {
    let token = tokenize(b"~").next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::HomeDirExpansion);
    assert_eq!(&*token.text, b"~");
}

#[test]
/// Verify that a tilde followed by certain special characters is still considered to be a home
/// directory expansion.
fn expansion_tilde_home_dir_redirect() {
    let mut tokens = tokenize(b"echo ~>tmp.txt").map(Result::unwrap);

    assert_eq!(tokens.next().unwrap().ttype, TokenType::Text);
    assert_eq!(tokens.next().unwrap().ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::HomeDirExpansion);
    assert_eq!(&*token.text, b"~");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");
}

#[test]
/// Verify that a tilde followed by certain special characters is still considered to be a home
/// directory expansion.
fn expansion_tilde_home_dir_pipe() {
    let mut tokens = tokenize(b"echo ~|cat").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::HomeDirExpansion);
    assert_eq!(&*token.text, b"~");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Pipe);
    assert_eq!(&*token.text, b"|");
}

#[test]
/// Verify that a tilde followed by a username is treated as an expansion and a username.
fn expansion_tilde_home_dir_username() {
    let mut tokens = tokenize(b"echo ~alice").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::HomeDirExpansion);
    assert_eq!(&*token.text, b"~");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Username);
    assert_eq!(&*token.text, b"alice");
}

#[test]
/// Verify that a tilde followed by a username and path is treated as such.
fn expansion_tilde_home_dir_username_path() {
    let mut tokens = tokenize(b"echo ~alice/foo").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::HomeDirExpansion);
    assert_eq!(&*token.text, b"~");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Username);
    assert_eq!(&*token.text, b"alice");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"/foo");
}

#[test]
/// Verify that a tilde followed by certain special characters is still considered to be a home
/// directory expansion.
fn expansion_tilde_home_dir_backgrounded() {
    let mut tokens = tokenize(b"echo ~&").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::HomeDirExpansion);
    assert_eq!(&*token.text, b"~");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Backgrounding);
    assert_eq!(&*token.text, b"&");
}

#[test]
/// Ensure a tilde followed by the path separator is considered a home directory expansion followed
/// by text representing the path (separately).
fn expansion_tilde_home_dir_path() {
    let mut tokens = tokenize(b"~/").map(Result::unwrap);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::HomeDirExpansion);
    assert_eq!(&*token.text, b"~");
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"/");
}

#[test]
/// Verify tilde isn't considered to be special text in other circumstances.
fn expansion_tilde_not_home_dir() {
    let token = tokenize(b"hello~").next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"hello~");

    let token = tokenize(b"he~llo").next().unwrap().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"he~llo");
}

#[test]
/// Test the functionality of [`UnifiedTokenIterator`] and ensure it correctly concatenates adjacent
/// text tokens not separated by whitespace.
fn unified_token_iterator() {
    let input = br#"he\llo"#;
    let tokens = tokenize(input);
    // "he" should be one token and the unnecessarily-escaped "l" should be prepended to the
    // remainder of the letters.
    assert_eq!(tokens.count(), 2);

    // Now verify that unification works and returns the expected results.
    let mut tokens = tokenize(input).unified().map(Result::unwrap);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"hello");

    assert!(tokens.next().is_none());
}

#[test]
fn comment_trailing_regular() {
    let input = b"hello # world";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"hello");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Comment);
    assert_eq!(&*token.text, b"# world");
}

#[test]
fn comment_trailing_newline() {
    let input = b"hello # world\n";
    let mut tokens = tokenize(input).map(Result::unwrap);

    let token = tokens.find(|t| t.ttype == TokenType::Comment)
        .expect("Expected to find a comment!");
    assert_eq!(&*token.text, b"# world");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::EndOfLine);
    assert_eq!(&*token.text, b"\n");
}

#[test]
fn comment_not_mid_word() {
    let input = b"echo hello#world";
    let token = tokenize(input).map(Result::unwrap)
        .last().unwrap();

    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"hello#world");
}

#[test]
fn comment_not_in_quotes() {
    let token = tokenize(br#"echo "hello world #""#)
        .map(Result::unwrap)
        .skip(1)
        .find(|t| t.ttype == TokenType::Text)
        .unwrap();
    assert_eq!(&*token.text, b"hello world #");
}

#[test]
fn comment_after_operator() {
    let token = tokenize(b"echo foo;#hello")
        .map(Result::unwrap)
        .last()
        .unwrap();
    assert_eq!(token.ttype, TokenType::Comment);
    assert_eq!(&*token.text, b"#hello");
}

#[test]
fn comment_after_implicit_continuation() {
    let mut tokens = tokenize(b"echo foo |\n # comment \ncat -")
        .map(Result::unwrap)
        .skip(4);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Pipe);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::EndOfLine);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Comment);
    assert_eq!(&*token.text, b"# comment ");
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::EndOfLine);
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"cat");
}

#[test]
fn comment_not_when_escaped() {
    let token = tokenize(br#"echo \#foo"#)
        .map(Result::unwrap)
        .skip(2)
        .next()
        .unwrap();

    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"#foo");
}
