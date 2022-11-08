mod tokenizer;

#[macro_export]
macro_rules! find {
    ($iter:ident, $pat:pat) => {{
        $iter.find(|i| matches!(i, $pat))
    }};
}
