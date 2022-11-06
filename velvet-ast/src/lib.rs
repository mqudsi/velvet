mod blocks;
mod operators;

use blocks::*;
use std::path::Path;
use std::rc::Rc;

pub struct Ast<'a> {
    pub bytes: &'a [u8],
    pub tree: Vec<BlockOrComment<'a>>,
}

pub struct Literal<'a> {
    pub literal: &'a [u8],
    pub line: usize,
    pub column: usize,
}

pub struct Comment<'a> {
    /// The comment, including the comment sign and any leading/trailing whitespace.
    pub literal: Literal<'a>,
    /// The text of the comment, excluding the comment sign and any leading/trailing whitespace.
    pub text: &'a [u8],
}

/// The smallest unit of execution, composed of one or more [`Block`]s chained together.
pub struct Pipeline<'a> {
    /// The statement, as encountered in the input.
    pub literal: Literal<'a>,
    pub pipeline: Vec<BlockOrComment<'a>>,
}

pub enum BlockOrComment<'a> {
    Block(Block<'a>),
    Comment(Comment<'a>),
}

pub enum TextOrComment<'a> {
    Text(Text<'a>),
    Comment(Comment<'a>),
}

/// A block, which is any construct that understands the concept of stdin, stdout, and stderr.
///
/// This can be something as simple as a single [`Command`], where the fds can be easily assigned
/// then it can be left to its own devices, to something as complex as nested blocks, each with
/// their own input and output that we have to marshall.
pub struct Block<'a> {
    /// The literal block, as encountered in the input
    pub literal: Literal<'a>,
    pub block: BlockType<'a>,
    pub stdin: Redirection<'a>,
    pub stdout: Redirection<'a>,
    pub stderr: Redirection<'a>,
}

/// A subshell, operating as an asynchronous block with its own stdin, stdout, and stderr.
pub struct Subshell<'a> {
    /// The literal text of the subshell, as encountered in the input.
    pub literal: Literal<'a>,
    pub ast: Ast<'a>,
}

/// The smallest unit of anything that is text or can be substituted in place of text.
///
/// Made up of one or more tokens.
pub struct Text<'a> {
    pub literal: Literal<'a>,
    /// The concatenated tokens that make up this text
    pub tokens: OneOrMore<Token<'a>>,
}

pub enum OneOrMore<T>
{
    Single(T),
    Multiple(Vec<T>),
}

pub struct Token<'a> {
    pub literal: Literal<'a>,
    pub source: TextSourceIndex,
    /// Whether or not the value of [`source`] should be split on $IFS or new lines.
    pub split: bool,
    /// How much of the text source to consume.
    pub range: Range,
}

pub enum Range {
    All,
    N(RangeIndex),
    StartEnd(Option<RangeIndex>, Option<RangeIndex>),
}

/// An index that can count from the start or the end of an array.
pub struct RangeIndex(pub i32);

/// The index of the [`TextSource`] whose output should be used.
pub struct TextSourceIndex(pub u32);

pub enum TextSource<'a> {
    PlainText(&'a [u8]),
    Subshell(Subshell<'a>),
    Variable { name: &'a [u8] },
    Glob { glob: &'a [u8] },
}

/// An abstraction over anything that has an stdin, stdout, and stderr.
pub enum BlockType<'a> {
    /// A command, which can end up evaluating to an executable, function, builtin, etc and is
    /// executed asynchronously.
    Command(Command<'a>),
    /// A subshell, which is executed asynchronously.
    Subshell(Subshell<'a>),
    Begin(Rc<BeginBlock<'a>>),
    For(Rc<ForBlock<'a>>),
    If(Rc<IfBlock<'a>>),
    Switch(Rc<SwitchBlock<'a>>),
    While(Rc<WhileBlock<'a>>),
}

/// A single command, which is an invocation of some executable token and arguments.
///
/// At this point, we have no idea if this command is going to resolve to an executable, a builtin,
/// a function, an alias, etc.
pub struct Command<'a> {
    /// The literal command invocation, as encountered in the input.
    pub literal: Literal<'a>,
    /// The decorator attached to the command, such as `builtin` or `command`.
    pub decorator: Option<Decorator>,
    /// The head as `args[0]` and any additional arguments to pass as `$argv`.
    pub args: Vec<TextOrComment<'a>>,
}

pub enum Decorator {
    Command,
    Builtin,
}

/// Where/how stdin, stdout, stderr are redirected.
pub enum Redirection<'a> {
    /// The default redirection
    ///
    /// stdin is connected to the tty for the first command in a pipeline, each stdout is connected
    /// to the stdin of the subsequent command, except for the last command which has its stdout
    /// redirected to the tty. All stderr is redirected to the tty.
    Default,
    /// To the terminal emulator. Only valid as stdin for the first process.
    Tty,
    /// Redirection to a numbered file descriptor.
    FileDescriptor(i32),
    /// Redirection to a file,
    File(&'a Path),
    /// Redirection from /dev/zero, where input is all zeros
    Zero,
    /// Redirection to /dev/null, where output is ignored
    Null,
    /// Redirection to/from a closed fd,
    Closed,
}

