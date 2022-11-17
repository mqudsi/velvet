use crate::tokenizer::*;

#[test]
fn redirect_basic() {
    let mut tokens = tokenize(b"echo foo > file1")
        .map(Result::unwrap)
        .skip_while(|t| t.ttype != TokenType::Redirection);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_append_basic() {
    let mut tokens = tokenize(b"echo foo >> file1")
        .map(Result::unwrap)
        .skip_while(|t| t.ttype != TokenType::Append);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Append);
    assert_eq!(&*token.text, b">>");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_no_space_right() {
    let mut tokens = tokenize(b"echo foo >file1")
        .map(Result::unwrap)
        .skip_while(|t| t.ttype != TokenType::Redirection);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_append_no_space_right() {
    let mut tokens = tokenize(b"echo foo >>file1")
        .map(Result::unwrap)
        .skip_while(|t| t.ttype != TokenType::Append);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Append);
    assert_eq!(&*token.text, b">>");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_no_space_left() {
    let mut tokens = tokenize(b"echo foo> file1")
        .map(Result::unwrap)
        .skip_while(|t| t.ttype != TokenType::Redirection);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_append_no_space_left() {
    let mut tokens = tokenize(b"echo foo>> file1")
        .map(Result::unwrap)
        .skip_while(|t| t.ttype != TokenType::Append);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Append);
    assert_eq!(&*token.text, b">>");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_no_space() {
    let mut tokens = tokenize(b"echo foo>file1")
        .map(Result::unwrap)
        .skip_while(|t| t.ttype != TokenType::Redirection);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_append_no_space() {
    let mut tokens = tokenize(b"echo foo>>file1")
        .map(Result::unwrap)
        .skip_while(|t| t.ttype != TokenType::Append);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Append);
    assert_eq!(&*token.text, b">>");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_from_fd() {
    let mut tokens = tokenize(b"echo 21>file1").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"21");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_append_from_fd() {
    let mut tokens = tokenize(b"echo 21>>file1").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"21");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Append);
    assert_eq!(&*token.text, b">>");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"file1");
}

#[test]
fn redirect_to_fd() {
    let mut tokens = tokenize(b"echo 21>&file").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"21");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FdRedirection);
    assert_eq!(&*token.text, b"&");

    // This is not a valid FD but it should still be classified as one.
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"file");
}

#[test]
/// Fish handles a space-prefixed `&foo` as a file descriptor reference, even if it's not in a
/// position where a file descriptor reference can be accepted. Verify that we have the same
/// behavior.
fn redirect_to_floating_fd() {
    let mut tokens = tokenize(b"echo 21> &file").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"21");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FdRedirection);
    assert_eq!(&*token.text, b"&");

    // This is not a valid FD but it should still be classified as one.
    // The parser will error out on it, not us.
    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"file");
}

#[test]
fn redirect_not_an_fd() {
    let mut tokens = tokenize(b"hello 21 > 22").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"21");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"22");
}

#[test]
fn redirect_not_from_fd() {
    let mut tokens = tokenize(b"echo e&2>/dev/tty")
        .map(Result::unwrap)
        .skip(2)
        .peekable();

    // Depending on how we are tokenizing, we may get "e&2" as one text token or as two or three.
    let mut text = Vec::with_capacity(3);
    while matches!(
        tokens.peek(),
        Some(Token {
            ttype: TokenType::Text,
            ..
        })
    ) {
        let t = tokens.next().unwrap();
        text.extend_from_slice(&*t.text);
    }
    assert_eq!(&*text, b"e&2");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Redirection);
    assert_eq!(&*token.text, b">");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Text);
    assert_eq!(&*token.text, b"/dev/tty");
}

#[test]
fn redirect_stdin_basic() {
    let mut tokens = tokenize(b"cat <&1").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::StdinRedirection);
    assert_eq!(&*token.text, b"<");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FdRedirection);
    assert_eq!(&*token.text, b"&");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"1");
}

#[test]
fn redirect_stdin_no_spaces() {
    let mut tokens = tokenize(b"cat<&1").map(Result::unwrap).skip(1);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::StdinRedirection);
    assert_eq!(&*token.text, b"<");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FdRedirection);
    assert_eq!(&*token.text, b"&");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"1");
}

#[test]
/// Fish treats a space-prefixed `&foo` as a file descriptor reference, even if it's not in a
/// position where a file descriptor is expected (e.g. for redirects, an fd reference must be glued
/// to the redirection operator and not separated by a space). Verify that we match fish's behavior
/// here.
fn redirect_stdin_space_from_floating_fd() {
    let mut tokens = tokenize(b"cat < &foo").map(Result::unwrap).skip(2);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::StdinRedirection);
    assert_eq!(&*token.text, b"<");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::Whitespace);

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FdRedirection);
    assert_eq!(&*token.text, b"&");

    let token = tokens.next().unwrap();
    assert_eq!(token.ttype, TokenType::FileDescriptor);
    assert_eq!(&*token.text, b"foo");
}
