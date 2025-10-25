use logosky::utils::{Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme, human_display::DisplayHuman};
use derive_more::{From, IsVariant, Unwrap, TryUnwrap};

use crate::{LitStrDelimiterKind, yul::LitStrKind};

/// Unclosed string literal representation

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Unclosed {
  span: Span,
  delimiter: LitStrDelimiterKind,
  kind: LitStrKind,
}

impl Unclosed {
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn new(span: Span, delimiter: LitStrDelimiterKind, kind: LitStrKind) -> Self {
    Self { span, delimiter, kind }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn unclosed_single_non_empty(span: Span) -> Self {
    Self::new(span, LitStrDelimiterKind::Single, LitStrKind::NonEmpty)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn unclosed_double_non_empty(span: Span) -> Self {
    Self::new(span, LitStrDelimiterKind::Double, LitStrKind::NonEmpty)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn unclosed_single_hex(span: Span) -> Self {
    Self::new(span, LitStrDelimiterKind::Single, LitStrKind::Hex)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn unclosed_double_hex(span: Span) -> Self {
    Self::new(span, LitStrDelimiterKind::Double, LitStrKind::Hex)
  }

  /// Get the span of the unclosed string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the delimiter kind of the unclosed string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn delimiter_kind(&self) -> LitStrDelimiterKind {
    self.delimiter
  }

  /// Get the kind of the unclosed string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> LitStrKind {
    self.kind
  }
}

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
        write!(f, "incomplete hexadecimal escape sequence, `\\x` must be followed by two hexadecimal digits")
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
        write!(f, "incomplete unicode escape sequence, `\\u` must be followed by four hexadecimal digits")
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
        write!(f, "unsupported escape character '{}' in string literal", pos_char.char_ref().display())
      }
    }
  }
}

impl<Char> core::error::Error for EscapeSequenceError<Char>
where
  Char: DisplayHuman + core::fmt::Debug,
{}

/// The error type for string literal lexing errors
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum StringError<Char = char> {
  /// Empty string literal.
  ///
  /// Returned when a string literal is empty.
  #[from(skip)]
  Empty(LitStrDelimiterKind),
  /// Unsupported character found in string literal.
  #[from(skip)]
  UnsupportedCharacter(PositionedChar<Char>),
  /// Unclosed string literal.
  Unclosed(Unclosed),
  /// Escape sequence error found in string literal.
  EscapeSequenceError(EscapeSequenceError<Char>),
}

impl<Char> core::fmt::Display for StringError<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Empty(kind) => match kind {
        LitStrDelimiterKind::Single => write!(f, "empty single-quoted string literal"),
        LitStrDelimiterKind::Double => write!(f, "empty double-quoted string literal"),
      },
      Self::Unclosed(unclosed) => {
        match (unclosed.delimiter_kind(), unclosed.kind()) {
          (LitStrDelimiterKind::Single, LitStrKind::NonEmpty) => {
            write!(f, "unclosed non-empty string literal")
          },
          (LitStrDelimiterKind::Double, LitStrKind::NonEmpty) => {
            write!(f, "unclosed non-empty string literal")
          },
          (LitStrDelimiterKind::Single, LitStrKind::Hex) => {
            write!(f, "unclosed hex string literal")
          },
          (LitStrDelimiterKind::Double, LitStrKind::Hex) => {
            write!(f, "unclosed hex string literal")
          },
        }
      },
      Self::EscapeSequenceError(err) => err.fmt(f),
      Self::UnsupportedCharacter(pos_char) => {
        write!(f, "unsupported character '{}' in string literal", pos_char.char_ref().display())
      },
    }
  }
}

impl<Char> core::error::Error for StringError<Char>
where
  Char: DisplayHuman + core::fmt::Debug,
{}
