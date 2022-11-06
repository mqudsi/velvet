use super::Ast;
use super::Block;
use super::Literal;
use super::Pipeline;
use super::Text;
use super::TextOrComment;

pub struct BeginBlock<'a> {
    pub literal: Literal<'a>,
    pub body: Ast<'a>,
}

pub struct ForBlock<'a> {
    pub literal: Literal<'a>,
    pub var: Text<'a>,
    pub source: Block<'a>,
    pub body: Ast<'a>,
}

pub struct IfBlock<'a> {
    pub literal: Literal<'a>,
    pub condition: Pipeline<'a>,
    pub body: Ast<'a>,
}

pub struct SwitchBlock<'a> {
    pub literal: Literal<'a>,
    pub cases: Vec<CaseBlock<'a>>,
}

pub struct WhileBlock<'a> {
    pub literal: Literal<'a>,
    pub condition: Pipeline<'a>,
    pub body: Ast<'a>,
}

/// The definition of a function
pub struct FunctionBlock<'a> {
    pub literal: Literal<'a>,
    pub name: Text<'a>,
    pub condition: Block<'a>,
    pub body: Ast<'a>,
}

pub struct CaseBlock<'a> {
    pub literal: Literal<'a>,
    pub cases: Vec<TextOrComment<'a>>,
    pub body: Ast<'a>,
}
