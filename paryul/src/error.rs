pub use lexsol::{
  error::yul::{Error as LexerError, Errors as LexerErrors},
  yul::{
    lossless::{Error as LosslessLexerError, Errors as LosslessLexerErrors},
    syntactic::{Error as AstLexerError, Errors as AstLexerErrors},
  },
};

use derive_more::{From, Into, IsVariant, TryUnwrap, Unwrap};
use lexsol::{
  types::{LitBool, LitDecimal, LitHexadecimal},
  yul::{Yul, lossless, syntactic},
};
use tokit::{
  Token,
  error::{
    IncompleteSyntax, Invalid, Missing, UnclosedBrace, UnclosedParen, UndelimitedBrace,
    UndelimitedParen, UnexpectedEot, UnexpectedSuffix, UnknownLexeme, UnopenedBrace, UnopenedParen,
    token::UnexpectedToken,
  },
  punct::{Comma, Dot},
  span::{SimpleSpan, Spanned},
  state::{recursion_tracker::RecursionLimitExceeded, tracker::LimitExceeded},
  types::{Ident, Keyword},
  utils::Message,
};

use crate::syntax::*;

type DefaultLang = Yul<syntactic::SyntaxKind>;

/// The parser error type for Yul syntactic tokens.
pub type AstParserError<'a, S> = Error<
  S,
  syntactic::Token<S>,
  syntactic::SyntaxKind,
  syntactic::Char<'a, S>,
  RecursionLimitExceeded,
>;

/// The parser error type for Yul lossless tokens.
pub type LosslessParserError<'a, S> =
  Error<S, lossless::Token<S>, lossless::SyntaxKind, lossless::Char<'a, S>, LimitExceeded>;

/// An unknown statement error.
pub type UnknownStatement<Char, Lang = DefaultLang> = UnknownLexeme<Char, Statement<Lang>>;

/// An unknown expression error.
pub type UnknownExpression<Char, Lang = DefaultLang> = UnknownLexeme<Char, Expression<Lang>>;

/// A trailing comma error.
pub type TrailingComma<Char> = UnexpectedSuffix<Char, Comma>;

/// A trailing dot error.
pub type TrailingDot<Char> = UnexpectedSuffix<Char, Dot>;

/// A missing comma error.
pub type MissingComma<Span = SimpleSpan, Lang = DefaultLang> = Missing<Comma, Span, Lang>;

/// A missing dot error.
pub type MissingDot<Span = SimpleSpan, Lang = DefaultLang> = Missing<Dot, Span, Lang>;

/// The invalid path segment error.
pub type InvalidPathSegment<Span = SimpleSpan, Lang = DefaultLang> =
  Invalid<InvalidPathSegmentData, Span, Lang>;

/// The invalid function name error.
pub type InvalidFunctionName<Span = SimpleSpan, Lang = DefaultLang> =
  Invalid<InvalidFunctionNameData, Span, Lang>;

/// The invalid variable name error.
pub type InvalidVariableName<Span = SimpleSpan, Lang = DefaultLang> =
  Invalid<InvalidVariableNameData, Span, Lang>;

/// An incomplete single variable declaration error.
pub type IncompleteSingleVariableDeclaration<Lang = DefaultLang> =
  IncompleteSyntax<SingleVariableDeclaration<Lang>>;

/// An incomplete multiple variables declaration error.
pub type IncompleteMultipleVariablesDeclaration<Lang = DefaultLang> =
  IncompleteSyntax<MultipleVariablesDeclaration<Lang>>;

/// An incomplete variable declaration error.
pub type IncompleteVariableDeclaration<Lang = DefaultLang> =
  IncompleteSyntax<VariableDeclaration<Lang>>;

/// An incomplete single target assignment error.
pub type IncompleteSingleTargetAssignment<Lang = DefaultLang> =
  IncompleteSyntax<SingleTargetAssignment<Lang>>;

/// An incomplete multiple targets assignment error.
pub type IncompleteMultipleTargetsAssignment<Lang = DefaultLang> =
  IncompleteSyntax<MultipleTargetsAssignment<Lang>>;

/// A knowledge of invalid function name data.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into)]
#[repr(transparent)]
pub struct InvalidFunctionNameData(pub InvalidIdentifierData);

/// A knowledge of invalid variable name data.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into)]
#[repr(transparent)]
pub struct InvalidVariableNameData(pub InvalidIdentifierData);

/// Invalid path segment knowledge.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InvalidPathSegmentData {
  /// EVM builtin function
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltinFunction(lexsol::yul::EvmBuiltinFunction),
  /// The keyword
  Keyword(&'static str),
  /// The boolean literal
  LitBool(LitBool),
}

/// A knowledge of semi-identifier, which means it seems like an identifier but is not.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum InvalidIdentifierData {
  /// EVM builtin function
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltinFunction(lexsol::yul::EvmBuiltinFunction),
  /// The keyword
  Keyword(&'static str),
  /// The boolean literal
  LitBool(LitBool),
  /// The number literal
  LitDecimal(LitDecimal),
  /// The hexadecimal number literal
  LitHexadecimal(LitHexadecimal),
}

#[derive(Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Error<S, T, TK: Clone + 'static = syntactic::SyntaxKind, Char = char, StateError = ()> {
  /// Lexer error
  Lexer(LexerErrors<Char, StateError>),
  /// Undelimited brace
  UndelimitedBrace(UndelimitedBrace),
  /// Undelimited parenthesis
  UndelimitedParen(UndelimitedParen),
  /// Unopened brace
  UnopenedBrace(UnopenedBrace),
  /// Unopened parenthesis
  UnopenedParen(UnopenedParen),
  /// Unclosed brace
  UnclosedBrace(UnclosedBrace),
  /// Unclosed parenthesis
  UnclosedParenthesis(UnclosedParen),
  /// Unexpected token
  UnexpectedToken(UnexpectedToken<'static, T, TK>),
  /// Unknown statement
  UnknownStatement(UnknownStatement<Char>),
  /// Unknown expression
  UnknownExpression(UnknownExpression<Char>),
  /// Invalid path segment
  InvalidPathSegment(InvalidPathSegment<S>),
  /// Invalid function name
  InvalidFunctionName(InvalidFunctionName<S>),
  /// Invalid variable name
  InvalidVariableName(InvalidVariableName<S>),
  /// Incomplete single variable declaration
  IncompleteSingleVariableDeclaration(IncompleteSingleVariableDeclaration),
  /// Incomplete multiple variables declaration
  IncompleteMultipleVariablesDeclaration(IncompleteMultipleVariablesDeclaration),
  /// Incomplete variable declaration
  IncompleteVariableDeclaration(IncompleteVariableDeclaration),
  /// Incomplete single target assignment
  IncompleteSingleTargetAssignment(IncompleteSingleTargetAssignment),
  /// Incomplete multiple targets assignment
  IncompleteMultipleTargetsAssignment(IncompleteMultipleTargetsAssignment),
  /// Missing comma
  MissingComma(MissingComma),
  /// Missing dot
  MissingDot(MissingDot),
  /// Trailing comma
  TrailingComma(TrailingComma<Char>),
  /// Trailing dot
  TrailingDot(TrailingDot<Char>),
  /// State error
  State(Spanned<StateError>),
  /// End of token stream
  Eot(UnexpectedEot),
  /// Other error
  #[from(skip)]
  Other(Spanned<Message>),
}

impl<S, T, TK: Clone + 'static, Char, StateError> core::fmt::Debug
  for Error<S, T, TK, Char, StateError>
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    todo!()
  }
}

impl<S, T, TK: Clone + 'static, Char, StateError> Error<S, T, TK, Char, StateError> {
  /// Creates an end-of-token-stream error with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn eot(offset: usize) -> Self {
    Self::Eot(UnexpectedEot::eot(offset))
  }

  /// Creates an unexpected token error with the given span and token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_token(span: SimpleSpan, found: T, expected: TK) -> Self {
    Self::UnexpectedToken(UnexpectedToken::expected_one_with_found(
      span, found, expected,
    ))
  }

  /// Creates a missing comma error with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn missing_comma(err: MissingComma) -> Self {
    Self::MissingComma(err)
  }

  /// Creates an other error with the given message.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn other(span: SimpleSpan, msg: impl Into<Message>) -> Self {
    Self::Other(Spanned::new(span, msg.into()))
  }
}
