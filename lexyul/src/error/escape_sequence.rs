use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{PositionedChar, Span, human_display::DisplayHuman};



/// Hexadecimal escape sequence error
#[derive(Debug, PartialEq, Eq, Clone, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum HexEscapeSequenceError {
  /// Incomplete hexadecimal escape sequence.
  ///
  /// Returned when a hexadecimal escape sequence is incomplete.
  ///
  /// e.g. `\x` or `\x1` without two hexadecimal digits.
  Incomplete(Span),
}

impl core::fmt::Display for HexEscapeSequenceError {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Incomplete(_) => {
        write!(
          f,
          "incomplete hexadecimal escape sequence, `\\x` must be followed by two hexadecimal digits"
        )
      }
    }
  }
}

impl core::error::Error for HexEscapeSequenceError {}

/// Unicode escape sequence error
#[derive(Debug, PartialEq, Eq, Clone, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum UnicodeEscapeSequenceError {
  /// Incomplete unicode escape sequence.
  ///
  /// Returned when a unicode escape sequence is incomplete.
  /// e.g. `\u` or `\u123` without four hexadecimal digits.
  Incomplete(Span),
}

impl core::fmt::Display for UnicodeEscapeSequenceError {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Incomplete(_) => {
        write!(
          f,
          "incomplete unicode escape sequence, `\\u` must be followed by four hexadecimal digits"
        )
      }
    }
  }
}

impl core::error::Error for UnicodeEscapeSequenceError {}

/// Escape sequence error
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum EscapeSequenceError<Char = char> {
  /// Hexadecimal escape sequence error.
  Hexadecimal(HexEscapeSequenceError),
  /// Unicode escape sequence error.
  Unicode(UnicodeEscapeSequenceError),
  /// Unsupported escape character.
  ///
  /// Returned when an escape character is not recognized.
  /// For example, `\o` where 'o' is not a valid escape character.
  #[from(skip)]
  Unsupported(PositionedChar<Char>),
}

impl<Char> core::fmt::Display for EscapeSequenceError<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Hexadecimal(err) => err.fmt(f),
      Self::Unicode(err) => err.fmt(f),
      Self::Unsupported(pos_char) => {
        write!(
          f,
          "unsupported escape character '{}' in string literal",
          pos_char.char_ref().display()
        )
      }
    }
  }
}

impl<Char> core::error::Error for EscapeSequenceError<Char> where
  Char: DisplayHuman + core::fmt::Debug
{
}