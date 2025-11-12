pub use lexsol::{
  error::yul::{Error as LexerError, Errors as LexerErrors},
  yul::{
    lossless::{Error as LosslessLexerError, Errors as LosslessLexerErrors},
    syntactic::{Error as SyntacticLexerError, Errors as SyntacticLexerErrors},
  },
};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use lexsol::yul::{lossless, syntactic};
use logosky::{
  Logos, State, Token,
  error::{UnexpectedEot, UnexpectedToken},
  utils::{Span, Spanned, recursion_tracker::RecursionLimitExceeded, tracker::LimitExceeded},
};

use crate::SyntaxKind;

/// The parser error type for Yul syntactic tokens.
pub type SyntacticParserError<'a, S> = Error<
  syntactic::Token<S>,
  SyntaxKind,
  <syntactic::Token<S> as Token<'a>>::Char,
  RecursionLimitExceeded,
>;

/// The parser error type for Yul lossless tokens.
pub type LosslessParserError<'a, S> =
  Error<lossless::Token<S>, SyntaxKind, <lossless::Token<S> as Token<'a>>::Char, LimitExceeded>;

// /// The parser error type for Yul.
// pub type ParserError<'a, T> = Error<<T as Token<'a>>::Char, <<<T as Token<'a>>::Logos as Logos<'a>>::Extras as State>::Error>;

#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Error<T, TK: 'static = SyntaxKind, Char = char, StateError = ()> {
  /// Lexer error
  Lexer(LexerErrors<Char, StateError>),
  /// Unexpected token
  UnexpectedToken(UnexpectedToken<'static, T, TK>),
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
}
