pub use lexsol::{
  error::yul::{Error as LexerError, Errors as LexerErrors},
  yul::{
    lossless::{Error as LosslessLexerError, Errors as LosslessLexerErrors},
    syntactic::{Error as AstLexerError, Errors as AstLexerErrors},
  },
};

use derive_more::{From, Into, IsVariant, TryUnwrap, Unwrap};
use lexsol::{
  types::{
    LitBool, LitDecimal, LitHexadecimal,
    punct::{Comma, Dot},
  },
  yul::{lossless, syntactic},
};
use logosky::{
  Token,
  error::{
    IncompleteSyntax, Invalid, Missing, UnclosedBrace, UnclosedParen, UndelimitedBrace,
    UndelimitedParen, UnexpectedEot, UnexpectedSuffix, UnexpectedToken, UnknownLexeme,
    UnopenedBrace, UnopenedParen,
  },
  types::{Ident, Keyword},
  utils::{
    Message, Span, Spanned, recursion_tracker::RecursionLimitExceeded, tracker::LimitExceeded,
  },
};

use crate::{SyntaxKind, YUL, syntax::*};

/// The parser error type for Yul syntactic tokens.
pub type AstParserError<'a, S> = Error<
  S,
  syntactic::Token<S>,
  SyntaxKind,
  <syntactic::Token<S> as Token<'a>>::Char,
  RecursionLimitExceeded,
>;

/// The parser error type for Yul lossless tokens.
pub type LosslessParserError<'a, S> =
  Error<S, lossless::Token<S>, SyntaxKind, <lossless::Token<S> as Token<'a>>::Char, LimitExceeded>;

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

/// The invalid path segment error.
pub type InvalidPathSegment<S, Lang = YUL> = Invalid<InvalidPathSegmentKnowledge<S, Lang>>;

/// The invalid function name error.
pub type InvalidFunctionName<S, Lang = YUL> = Invalid<InvalidFunctionNameKnowledge<S, Lang>>;

/// The invalid variable name error.
pub type InvalidVariableName<S, Lang = YUL> = Invalid<InvalidVariableNameKnowledge<S, Lang>>;

/// An incomplete variable declaration error.
pub type IncompleteVariableDeclaration<Lang = YUL> = IncompleteSyntax<VariableDeclaration<Lang>>;

/// A knowledge of invalid function name.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into)]
#[repr(transparent)]
pub struct InvalidFunctionNameKnowledge<S, Lang = YUL>(pub SemiIdentifierKnowledge<S, Lang>);

/// A knowledge of invalid variable name.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into)]
#[repr(transparent)]
pub struct InvalidVariableNameKnowledge<S, Lang = YUL>(pub SemiIdentifierKnowledge<S, Lang>);

/// A knowledge of invalid path segment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into)]
#[repr(transparent)]
pub struct InvalidPathSegmentKnowledge<S, Lang = YUL>(pub SemiIdentifierKnowledge<S, Lang>);

/// A knowledge of semi-identifier, which means it seems like an identifier but is not.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum SemiIdentifierKnowledge<S, Lang = YUL> {
  /// EVM builtin function
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltinFunction(Spanned<lexsol::yul::EvmBuiltinFunction>),
  /// The identifier, some language may reserve certain identifiers as contextual keywords
  Identifier(Ident<S, Lang>),
  /// The keyword
  Keyword(Keyword<S, Lang>),
  /// The boolean literal
  LitBool(Spanned<LitBool<S>>),
  /// The decimal literal
  LitDecimal(Spanned<LitDecimal<S>>),
  /// The hexadecimal literal
  LitHexadecimal(Spanned<LitHexadecimal<S>>),
}

// /// The parser error type for Yul.
// pub type ParserError<'a, T> = Error<<T as Token<'a>>::Char, <<<T as Token<'a>>::Logos as Logos<'a>>::Extras as State>::Error>;

#[derive(Debug, Clone, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Error<S, T, TK: 'static = SyntaxKind, Char = char, StateError = ()> {
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
  /// Incomplete variable declaration
  IncompleteVariableDeclaration(IncompleteVariableDeclaration),
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

impl<S, T, TK, Char, StateError> Error<S, T, TK, Char, StateError> {
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

  /// Creates an other error with the given message.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn other(span: Span, msg: impl Into<Message>) -> Self {
    Self::Other(Spanned::new(span, msg.into()))
  }
}
