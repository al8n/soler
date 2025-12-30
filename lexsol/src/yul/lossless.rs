use derive_more::{Display, IsVariant, TryUnwrap, Unwrap};
use token::token;
#[cfg(feature = "evm")]
use tokit::Require;
use tokit::{
  lexer::{IdentifierToken, KeywordToken, LitToken, OperatorToken, PunctuatorToken},
  utils::{cmp::Equivalent, tracker::LimitExceeded},
};

use super::Lit;

use crate::{
  error::yul as error,
  types::{LitBool, LitNumber},
};

mod bytes;
mod str;
mod token;

/// The syntactic lexer for Yul.
pub type Lexer<'a, S = &'a str> = tokit::lexer::LogosLexer<'a, Token<S>>;

/// The char type used for the syntactic token.
pub type Char<'a, S> = <<<Lexer<'a, S> as tokit::Lexer<'a>>::Source as tokit::Source<usize>>::Slice<
  'a,
> as tokit::lexer::source::Slice<'a>>::Char;
/// The error type for lexing based on lossless [`Token`].
pub type Error<'a, S> = error::Error<Char<'a, S>, LimitExceeded>;
/// A collection of errors for lossless [`Token`].
pub type Errors<'a, S> = error::Errors<Char<'a, S>, LimitExceeded>;

/// The lossless token of Yul
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Token<S> {
  /// ' '
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Space,
  /// '\t'
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Tab,
  /// '\n'
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  NewLine,
  /// '\r'
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  CarriageReturn,
  /// '\r\n'
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  CarriageReturnNewLine,
  /// '\f'
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  FormFeed,

  /// ":="
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  ColonAssign,
  /// "->"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  ThinArrow,
  /// "{"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  LBrace,
  /// "}"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  RBrace,
  /// "("
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  LParen,
  /// ")"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  RParen,
  /// "."
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Dot,
  /// ","
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Comma,
  /// "leave"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Leave,
  /// "continue"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Continue,
  /// "break"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Break,
  /// "switch"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Switch,
  /// "case"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Case,
  /// "default"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Default,
  /// "function"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Function,
  /// "let"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Let,
  /// "if"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  If,
  /// "for"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  For,

  /// Yul line comment.
  LineComment(S),

  /// Yul multi-line comment.
  MultiLineComment(S),

  /// Yul identifier
  ///
  /// Spec: [Yul identifier](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulIdentifier)
  Identifier(S),
  /// Yul literal
  ///
  /// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
  Lit(Lit<S>),

  /// Yul EVM built-in function
  ///
  /// Spec: [Yul evm built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltin(super::EvmBuiltinFunction),
}

/// The kind of Yul lossless token
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
#[non_exhaustive]
pub enum TokenKind {
  /// ' '
  Space,
  /// '\t'
  Tab,
  /// '\n'
  NewLine,
  /// '\r'
  CarriageReturn,
  /// '\r\n'
  CarriageReturnNewLine,
  /// '\f'
  FormFeed,

  /// ":="
  ColonAssign,
  /// "->"
  ThinArrow,
  /// "{"
  LBrace,
  /// "}"
  RBrace,
  /// "("
  LParen,
  /// ")"
  RParen,
  /// "."
  Dot,
  /// ","
  Comma,
  /// "leave"
  Leave,
  /// "continue"
  Continue,
  /// "break"
  Break,
  /// "switch"
  Switch,
  /// "case"
  Case,
  /// "default"
  Default,
  /// "function"
  Function,
  /// "let"
  Let,
  /// "if"
  If,
  /// "for"
  For,

  /// Yul line comment.
  LineComment,

  /// Yul multi-line comment.
  MultiLineComment,

  /// Yul identifier
  ///
  /// Spec: [Yul identifier](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulIdentifier)
  Identifier,

  /// Yul literal
  ///
  /// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
  Lit(Lit),

  /// Yul EVM built-in function
  ///
  /// Spec: [Yul evm built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltin(super::EvmBuiltinFunction),
}

impl<S> Token<S> {
  /// Get the kind of this token
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> TokenKind {
    match self {
      Self::FormFeed => TokenKind::FormFeed,
      Self::Space => TokenKind::Space,
      Self::Tab => TokenKind::Tab,
      Self::NewLine => TokenKind::NewLine,
      Self::CarriageReturn => TokenKind::CarriageReturn,
      Self::CarriageReturnNewLine => TokenKind::CarriageReturnNewLine,
      Self::ColonAssign => TokenKind::ColonAssign,
      Self::ThinArrow => TokenKind::ThinArrow,
      Self::LBrace => TokenKind::LBrace,
      Self::RBrace => TokenKind::RBrace,
      Self::LParen => TokenKind::LParen,
      Self::RParen => TokenKind::RParen,
      Self::Dot => TokenKind::Dot,
      Self::Comma => TokenKind::Comma,
      Self::Leave => TokenKind::Leave,
      Self::Continue => TokenKind::Continue,
      Self::Break => TokenKind::Break,
      Self::Switch => TokenKind::Switch,
      Self::Case => TokenKind::Case,
      Self::Default => TokenKind::Default,
      Self::Function => TokenKind::Function,
      Self::Let => TokenKind::Let,
      Self::If => TokenKind::If,
      Self::For => TokenKind::For,
      Self::LineComment(_) => TokenKind::LineComment,
      Self::MultiLineComment(_) => TokenKind::MultiLineComment,
      Self::Identifier(_) => TokenKind::Identifier,
      Self::Lit(lit) => TokenKind::Lit(lit.unit()),
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(f) => TokenKind::EvmBuiltin(f.unit()),
    }
  }
}

impl<'a, S: 'a> PunctuatorToken<'a> for Token<S>
where
  Token<S>: tokit::Token<'a>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_open_brace(&self) -> bool {
    matches!(self, Self::LBrace)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_close_brace(&self) -> bool {
    matches!(self, Self::RBrace)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_open_paren(&self) -> bool {
    matches!(self, Self::LParen)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_close_paren(&self) -> bool {
    matches!(self, Self::RParen)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_dot(&self) -> bool {
    matches!(self, Self::Dot)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_comma(&self) -> bool {
    matches!(self, Self::Comma)
  }
}

impl<'a, S: 'a> OperatorToken<'a> for Token<S>
where
  Token<S>: tokit::Token<'a>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_colon_eq_assign(&self) -> bool {
    matches!(self, Self::ColonAssign)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_arrow(&self) -> bool {
    matches!(self, Self::ThinArrow)
  }
}

impl<'a, S: 'a> LitToken<'a> for Token<S>
where
  Token<S>: tokit::Token<'a>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_literal(&self) -> bool {
    matches!(self, Self::Lit(_))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_decimal_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Number(LitNumber::Decimal(_))))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_hexadecimal_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Number(LitNumber::Hexadecimal(_))))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_boolean_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Boolean(_)))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_true_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Boolean(LitBool::True(_))))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_false_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Boolean(LitBool::False(_))))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_string_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::String(_)))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_inline_string_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::String(_)))
  }
}

impl<'a, S: 'a> IdentifierToken<'a, S> for Token<S>
where
  Token<S>: tokit::Token<'a>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_identifier(&self) -> bool {
    matches!(self, Self::Identifier(_))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn identifier(&self) -> Option<&S> {
    match self {
      Self::Identifier(s) => Some(s),
      _ => None,
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn try_into_identifier(self) -> Result<S, Self>
  where
    Self: Sized,
  {
    self.try_unwrap_identifier().map_err(|e| e.input)
  }
}

impl<'a, S: 'a> KeywordToken<'a> for Token<S>
where
  Token<S>: tokit::Token<'a>,
  str: Equivalent<S>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn keyword(&self) -> Option<&'static str> {
    match self {
      Self::Leave => Some("leave"),
      Self::Continue => Some("continue"),
      Self::Break => Some("break"),
      Self::Switch => Some("switch"),
      Self::Case => Some("case"),
      Self::Default => Some("default"),
      Self::Function => Some("function"),
      Self::Let => Some("let"),
      Self::If => Some("if"),
      Self::For => Some("for"),
      _ => None,
    }
  }
}

impl<S> Equivalent<Token<S>> for str
where
  str: Equivalent<S>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn equivalent(&self, other: &Token<S>) -> bool {
    match other {
      Token::Leave => self.eq("leave"),
      Token::Continue => self.eq("continue"),
      Token::Break => self.eq("break"),
      Token::Switch => self.eq("switch"),
      Token::Case => self.eq("case"),
      Token::Default => self.eq("default"),
      Token::Function => self.eq("function"),
      Token::Let => self.eq("let"),
      Token::If => self.eq("if"),
      Token::For => self.eq("for"),
      Token::LBrace => self.eq("{"),
      Token::RBrace => self.eq("}"),
      Token::LParen => self.eq("("),
      Token::RParen => self.eq(")"),
      Token::Dot => self.eq("."),
      Token::Comma => self.eq(","),
      Token::ColonAssign => self.eq(":="),
      Token::ThinArrow => self.eq("->"),
      Token::Identifier(s) => self.equivalent(s),
      #[cfg(feature = "evm")]
      Token::EvmBuiltin(b) => self.eq(b.as_str()),
      _ => false,
    }
  }
}

#[cfg(feature = "evm")]
#[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
impl<S> Require<super::EvmBuiltinFunction> for Token<S> {
  type Err = Self;

  fn require(self) -> Result<super::EvmBuiltinFunction, Self::Err>
  where
    Self: Sized,
  {
    self.try_unwrap_evm_builtin().map_err(|e| e.input)
  }
}

super::syntax_kind!(
  /// The syntax kinds for Yul
  enum SyntaxKind {
    @evm: [
      "stop",
      "add",
      "sub",
      "mul",
      "div",
      "sdiv",
      "mod",
      "smod",
      "exp",
      "not",
      "lt",
      "gt",
      "slt",
      "sgt",
      "eq",
      "iszero",
      "and",
      "or",
      "xor",
      "byte",
      "shl",
      "shr",
      "sar",
      "clz",
      "addmod",
      "mulmod",
      "signextend",
      "keccak256",
      "pop",
      "mload",
      "mstore",
      "mstore8",
      "sload",
      "sstore",
      "tload",
      "tstore",
      "msize",
      "gas",
      "address",
      "balance",
      "selfbalance",
      "caller",
      "callvalue",
      "calldataload",
      "calldatasize",
      "calldatacopy",
      "extcodesize",
      "extcodecopy",
      "returndatasize",
      "returndatacopy",
      "mcopy",
      "extcodehash",
      "create",
      "create2",
      "call",
      "callcode",
      "delegatecall",
      "staticcall",
      "return",
      "revert",
      "selfdestruct",
      "invalid",
      "log0",
      "log1",
      "log2",
      "log3",
      "log4",
      "chainid",
      "origin",
      "gasprice",
      "blockhash",
      "blobhash",
      "coinbase",
      "timestamp",
      "number",
      "difficulty",
      "prevrandao",
      "gaslimit",
      "basefee",
      "blobbasefee",
    ]
    /// The line comment syntax kind
    LineComment,
    /// The multi-line comment syntax kind
    MultiLineComment,
  }
);

#[cfg(not(feature = "rowan"))]
impl tokit::syntax::Language for super::Yul<SyntaxKind> {
  type SyntaxKind = SyntaxKind;
}

impl super::super::sealed::Sealed for super::Yul<SyntaxKind> {
  const INIT: Self = super::Yul(core::marker::PhantomData);
  const NAME: &'static str = "yul";
  const DECIMAL_NUMBER_PATTERN: &'static str = r"0|[1-9][0-9]*";
  const HEX_NUMBER_PATTERN: &'static str = r"0x[0-9a-fA-F]+";
}

#[cfg(feature = "rowan")]
#[cfg_attr(docsrs, doc(cfg(feature = "rowan")))]
const _: () = {
  use rowan::{Language, SyntaxKind as RowanSyntaxKind};

  impl Language for super::Yul<SyntaxKind> {
    type Kind = SyntaxKind;

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn kind_from_raw(raw: RowanSyntaxKind) -> Self::Kind {
      unsafe { core::mem::transmute::<u16, Self::Kind>(raw.0) }
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn kind_to_raw(kind: Self::Kind) -> RowanSyntaxKind {
      RowanSyntaxKind(kind as u16)
    }
  }
};
