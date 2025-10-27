use derive_more::{IsVariant, TryUnwrap, Unwrap};

mod bytes;
mod str;
mod token;

use token::token;

use super::Lit;

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
  Assign,
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
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
  Assign,
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
  Lit,

  /// Yul EVM built-in function
  ///
  /// Spec: [Yul evm built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltin,
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
      Self::Assign => TokenKind::Assign,
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
      Self::Lit(_) => TokenKind::Lit,
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(_) => TokenKind::EvmBuiltin,
    }
  }
}

