#![allow(unused_imports)]
#![allow(unused_assignments)]
#![allow(unused_variables)]

use crate::peekable::{Peekable, PeekableQueue};
use ast::*;
use std::cell::RefCell;

use crate::tokenizer::{Token, TokenType, Tokenizer, TokenizerError};

pub struct Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, ParserError>>,
{
    tokens: PeekableQueue<I>,
}

pub fn parse<'a>(src: &'a [u8]) {
    let tokenizer = Tokenizer::new(src);
    let parser = Parser {
        tokens: tokenizer
            .map(|result| result.map_err(|err| err.into()))
            .into_peekable(),
    };
}

pub enum ParserError {
    TokenizerError(TokenizerError),
    UnexpectedEnd,
}

impl From<TokenizerError> for ParserError {
    fn from(value: TokenizerError) -> Self {
        ParserError::TokenizerError(value)
    }
}

pub enum ParserState {
    /// Either starting from scratch or a job has been fully parsed and returned.
    None,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, ParserError>>,
{
    /*
    /// Consumes and returns the current token if it is a match to the requested `TokenType`.
    ///
    /// If `std::ops::Try` were stabilized, this would return an `enum` of `None` `Match(T)`,
    /// `Mismatch(T)`, and `Error(E)`. Instead we have to use this ugly nesting and probably rely on
    /// a macro at the call site to sift through the result.
    fn read(&mut self, ttype: TokenType) -> Result<Option<Token<'a>>, Option<TokenizerError>> {
        match self.tokens.peek() {
            Some(Ok(t)) if t.ttype == ttype => Ok(Some(self.tokens.next().unwrap().unwrap())),
            Some(Ok(_)) => Ok(None),
            Some(Err(_)) => Err(Some(self.tokens.next().unwrap().unwrap_err())),
            None => Err(None),
        }
    }*/

    pub fn parse(&'a mut self) -> Result<(), ParserError> {
        self.parse_job()
    }

    /*#[inline(always)]
    fn peek<'p>(tokens: &'p mut Peekable<T>) -> Result<&'a Token<'p>, ParserError<'a>>
    where 'a: 'p {
        match tokens.peek().map(|x| x.as_ref()) {
            None => Err(ParserError::UnexpectedEnd),
            Some(Err(err)) => Err(ParserError::from(err)),
            Some(Ok(token)) => Ok(token),
        }
    }*/

    fn parse_job(&'a mut self) -> Result<(), ParserError> {
        let mut whitespace_prefixed = false;
        for _ in self
            .tokens
            .map_while2(|t| t.as_ref().map(|t| t.ttype == TokenType::Whitespace).ok())
        {
            whitespace_prefixed = true;
        }

        let logical_decorator = self.read_logical_decorator();
        let decorator = self.read_decorator();

        Ok(())
    }

    fn read_time(&mut self) -> bool {
        self.tokens
            .next_if(|next| {
                next.as_ref()
                    .map(|t| t.matches_text(b"time"))
                    .unwrap_or(false)
            })
            .is_some()
    }

    fn read_logical_decorator(&mut self) -> Option<LogicalDecorator> {
        self.tokens
            .map_while2(|next| match next {
                Ok(token) if token.ttype == TokenType::Text => match &*token.text {
                    b"or" => Some(LogicalDecorator::Or),
                    b"and" => Some(LogicalDecorator::And),
                    b"not" => Some(LogicalDecorator::Not),
                    _ => None,
                },
                _ => None,
            })
            .next()
    }

    fn read_decorator(&mut self) -> Option<Decorator> {
        self.tokens
            .map_while2(|next| match next {
                Ok(token) if token.ttype == TokenType::Text => match &*token.text {
                    b"command" => Some(Decorator::Command),
                    b"builtin" => Some(Decorator::Builtin),
                    b"function" => Some(Decorator::Function),
                    _ => None,
                },
                _ => None,
            })
            .next()
    }
}

trait TokenStreamUtils {
    fn consume_spaces(&mut self) -> bool;
}

impl<'a, I> TokenStreamUtils for PeekableQueue<I>
where
    I: Iterator<Item = Result<Token<'a>, ParserError>>,
{
    fn consume_spaces(&mut self) -> bool {
        let mut result = false;
        while self
            .next_if(|t| t.as_ref().map(Token::is_space).unwrap_or(false))
            .is_some()
        {
            result = true;
        }
        result
    }
}
