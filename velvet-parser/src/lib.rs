#![allow(unused)]

use ast::*;
use std::io::{BufRead, BufReader};

#[cfg(test)]
mod tests;
mod tokenizer;

pub fn parse<'a>(input: &'a [u8]) -> Result<Ast<'a>, ParserError> {
    let line = 0;
    let ast = Vec::<BlockOrComment>::new();

    return Ok(Ast {
        bytes: input,
        tree: ast,
    });
}

pub struct Source<'a> {
    source: &'a [u8],
    line: usize,
    col: usize,
}

pub struct ParserError {}
