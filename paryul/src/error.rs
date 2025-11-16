pub use lexsol::{
  error::yul::{Error as LexerError, Errors as LexerErrors},
  yul::{
    lossless::{Error as LosslessLexerError, Errors as LosslessLexerErrors},
    syntactic::{Error as AstLexerError, Errors as AstLexerErrors},
  },
};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use lexsol::{types::punct::{Comma, Dot}, yul::{lossless, syntactic}};
use logosky::{
  Token,
  error::{
    Missing, UnclosedBrace, UnclosedParen, UndelimitedBrace, UndelimitedParen, UnexpectedEot, UnexpectedSuffix, UnexpectedToken, UnknownLexeme, UnopenedBrace, UnopenedParen
  },
  utils::{Span, Spanned, recursion_tracker::RecursionLimitExceeded, tracker::LimitExceeded},
};

use crate::{
  SyntaxKind, YUL,
  syntax::{Expression, Statement},
};

/// The parser error type for Yul syntactic tokens.
pub type AstParserError<'a, S> = Error<
  syntactic::Token<S>,
  SyntaxKind,
  <syntactic::Token<S> as Token<'a>>::Char,
  RecursionLimitExceeded,
>;

/// The parser error type for Yul lossless tokens.
pub type LosslessParserError<'a, S> =
  Error<lossless::Token<S>, SyntaxKind, <lossless::Token<S> as Token<'a>>::Char, LimitExceeded>;

/// An unknown statement error.
pub type UnknownStatement<Char, Lang = YUL> = UnknownLexeme<Char, Statement<Lang>>;

/// An unknown expression error.
pub type UnknownExpression<Char, Lang = YUL> = UnknownLexeme<Char, Expression<Lang>>;

/// A trailing comma error.
pub type TrailingComma<Char> = UnexpectedSuffix<Char, Comma>;

/// A trailing dot error.
pub type TrailingDot<Char> = UnexpectedSuffix<Char, Dot>;

/// A missing comma error.
pub type MissingComma<Lang = YUL> = Missing<Comma, Lang>;

/// A missing dot error.
pub type MissingDot<Lang = YUL> = Missing<Dot, Lang>;

// /// The parser error type for Yul.
// pub type ParserError<'a, T> = Error<<T as Token<'a>>::Char, <<<T as Token<'a>>::Logos as Logos<'a>>::Extras as State>::Error>;

#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Error<T, TK: 'static = SyntaxKind, Char = char, StateError = ()> {
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
  /// Missing comma
  MissingComma(MissingComma<YUL>),
  /// Missing dot
  MissingDot(MissingDot<YUL>),
  /// Trailing comma
  TrailingComma(TrailingComma<Char>),
  /// Trailing dot
  TrailingDot(TrailingDot<Char>),
  /// State error
  State(Spanned<StateError>),
  /// End of token stream
  Eot(UnexpectedEot),
}

impl<T, TK, Char, StateError> Error<T, TK, Char, StateError> {
  /// Creates an end-of-token-stream error with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn eot(span: Span) -> Self {
    Self::Eot(UnexpectedEot::eot(span))
  }

  /// Creates an unexpected token error with the given span and token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_token(span: Span, found: T, expected: TK) -> Self {
    Self::UnexpectedToken(UnexpectedToken::expected_one_with_found(
      span, found, expected,
    ))
  }

  /// Creates a missing comma error with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn missing_comma(err: Missing<Comma, YUL>) -> Self {
    Self::MissingComma(err)
  }
}
