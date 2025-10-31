use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{
  Lexeme, LineTerminator, PositionedChar, Span, Unclosed, UnexpectedLexeme,
  human_display::DisplayHuman,
};

use crate::types::LitStrDelimiterKind;

use super::{EscapeSequenceError, HexEscapeSequenceError, UnicodeEscapeSequenceError};

/// The error type for string literal lexing errors
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum StringError<Char = char> {
  /// Unsupported character or characters found in string literal.
  #[from(skip)]
  Unsupported(Lexeme<Char>),
  /// Unexpected line terminator found in string literal.
  UnexpectedLineTerminator(UnexpectedLexeme<Char, LineTerminator>),
  /// Unclosed string literal.
  Unclosed(Unclosed<LitStrDelimiterKind>),
  /// Escape sequence error found in string literal.
  EscapeSequenceError(EscapeSequenceError<Char>),
  /// ... other string literal errors can be added here
  Other(super::Message),
}

impl<Char> Default for StringError<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::Other("unknown string literal error".into())
  }
}

impl<Char> core::fmt::Display for StringError<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Unclosed(unclosed) => match unclosed.delimiter_ref() {
        LitStrDelimiterKind::Single => {
          write!(f, "unclosed non-empty single-quoted string literal")
        }
        LitStrDelimiterKind::Double => {
          write!(f, "unclosed non-empty double-quoted string literal")
        }
      },
      Self::UnexpectedLineTerminator(err) => {
        write!(
          f,
          "unexpected line terminator '{}' in non-empty string literal",
          err.hint(),
        )
      }
      Self::EscapeSequenceError(err) => err.fmt(f),
      Self::Unsupported(lexeme) => match lexeme {
        Lexeme::Char(pc) => {
          write!(
            f,
            "unsupported character '{}' in string literal at {}",
            pc.char_ref().display(),
            pc.position()
          )
        }
        Lexeme::Span(span) => {
          write!(f, "unsupported characters in string literal at {}", span)
        }
      },
      Self::Other(msg) => msg.fmt(f),
    }
  }
}

impl<Char> core::error::Error for StringError<Char> where Char: DisplayHuman + core::fmt::Debug {}

impl<Char> StringError<Char> {
  /// Create a new unclosed string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed(span: Span, delimiter: LitStrDelimiterKind) -> Self {
    Self::Unclosed(Unclosed::new(span, delimiter))
  }

  /// Create a new unclosed single-quoted non-empty string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_single_quote(span: Span) -> Self {
    Self::unclosed(span, LitStrDelimiterKind::Single)
  }

  /// Create a new unclosed double-quoted non-empty string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_double_quote(span: Span) -> Self {
    Self::unclosed(span, LitStrDelimiterKind::Double)
  }

  /// Create a new unexpected line terminator error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_line_terminator(lexeme: UnexpectedLexeme<Char, LineTerminator>) -> Self {
    Self::UnexpectedLineTerminator(lexeme)
  }

  /// Create a unsupported escape character error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unsupported_escape_character(char: PositionedChar<Char>) -> Self {
    Self::EscapeSequenceError(EscapeSequenceError::Unsupported(char))
  }

  /// Create a incomplete hexadecimal escape sequence error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn incomplete_hex_escape_sequence(span: Span) -> Self {
    Self::EscapeSequenceError(EscapeSequenceError::Hexadecimal(
      HexEscapeSequenceError::Incomplete(span),
    ))
  }

  /// Create a incomplete unicode escape sequence error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn incomplete_unicode_escape_sequence(span: Span) -> Self {
    Self::EscapeSequenceError(EscapeSequenceError::Unicode(
      UnicodeEscapeSequenceError::Incomplete(span),
    ))
  }

  /// Create a unsupported character error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unsupported_character(char: PositionedChar<Char>) -> Self {
    Self::Unsupported(Lexeme::Char(char))
  }

  /// Create a unsupported characters error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unsupported_characters(span: Span) -> Self {
    Self::Unsupported(Lexeme::Span(span))
  }

  /// Create a other string literal error with the given message.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn other(message: impl Into<super::Message>) -> Self {
    Self::Other(message.into())
  }
}
