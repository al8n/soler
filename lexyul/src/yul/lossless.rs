use derive_more::{IsVariant, TryUnwrap, Unwrap};

// mod token;

use super::Lit;

/// The lossless token of Yul
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Token<S> {
  /// ' ', only produced in lossless lexing
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Space,
  /// '\t', only produced in lossless lexing
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Tab,
  /// '\n', only produced in lossless lexing
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  NewLine,
  /// '\r', only produced in lossless lexing
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  CarriageReturn,
  /// '\r\n', only produced in lossless lexing
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  CarriageReturnNewLine,

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
pub enum TokenKind {
  /// ' ', only produced in lossless lexing
  Space,
  /// '\t', only produced in lossless lexing
  Tab,
  /// '\n', only produced in lossless lexing
  NewLine,
  /// '\r', only produced in lossless lexing
  CarriageReturn,
  /// '\r\n', only produced in lossless lexing
  CarriageReturnNewLine,

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
