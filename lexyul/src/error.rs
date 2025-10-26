use logosky::utils::{LineTerminator, PositionedChar, Span, UnexpectedLexeme};

use crate::string_lexer::LitStrDelimiterKind;

#[cfg(any(feature = "std", feature = "alloc"))]
type Message = std::borrow::Cow<'static, str>;

#[cfg(not(any(feature = "std", feature = "alloc")))]
type Message = &'static str;

/// A custom error trait.
pub trait CustomError {
  /// Create a new error with the given message.
  fn other(message: impl Into<Message>) -> Self;
}

/// The error for escaped error
pub trait EscapeSequenceError<Char> {
  /// Create a unsupported escape character error.
  fn unsupported_escape_character(char: PositionedChar<Char>) -> Self;

  /// Create a incomplete hexadecimal escape sequence error.
  fn incomplete_hex_escape_sequence(span: Span) -> Self;

  /// Create a incomplete unicode escape sequence error.
  fn incomplete_unicode_escape_sequence(span: Span) -> Self;
}

/// The error types for regular string literals.
pub trait RegularStrError<Char>: EscapeSequenceError<Char> + CustomError {
  /// Create a new empty string literal error with the given delimiter kind.
  fn empty(span: Span, kind: LitStrDelimiterKind) -> Self;

  /// Create a new unclosed string literal error.
  fn unclosed(span: Span, kind: LitStrDelimiterKind) -> Self;

  /// Create a new unexpected line terminator error.
  fn unexpected_line_terminator(lexeme: UnexpectedLexeme<Char, LineTerminator>) -> Self;

  /// Create a unsupported character error.
  fn unsupported_character(char: PositionedChar<Char>) -> Self;

  /// Create a unsupported characters error.
  fn unsupported_characters(span: Span) -> Self;
}
