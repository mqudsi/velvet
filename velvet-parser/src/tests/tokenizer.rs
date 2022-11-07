use crate::tokenizer::*;

#[test]
fn new_line_counting() {
    let input = format!("hello\nworld\n\ngoodbye");
    let tokenizer = Tokenizer::new(input.as_bytes());
    let mut tokens = tokenizer.into_iter();

    let world = tokens.find(|t| {
        let t = t.as_ref().unwrap();
        t.ttype == TokenType::Text && matches!(&*t.text, b"world")
    }).unwrap().unwrap();
    assert_eq!(world.line, 2);
    assert_eq!(world.col, 1, "Column count not reset at new line!");

    let goodbye = tokens.find(|t| {
        let t = t.as_ref().unwrap();
        t.ttype == TokenType::Text && matches!(&*t.text, b"goodbye")
    }).unwrap().unwrap();
    assert_eq!(goodbye.line, 4, "Adjacent new lines not counted!");
}
