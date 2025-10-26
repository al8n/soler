use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{
  Lexeme, LineTerminator, PositionedChar, Span, UnexpectedLexeme, human_display::DisplayHuman,
};

use crate::yul::{LitStrDelimiterKind, LitStrKind};

/// Unclosed string literal representation

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Unclosed {
  span: Span,
  delimiter: LitStrDelimiterKind,
  kind: LitStrKind,
}

impl Unclosed {
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn new(
    span: Span,
    delimiter: LitStrDelimiterKind,
    kind: LitStrKind,
  ) -> Self {
    Self {
      span,
      delimiter,
      kind,
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn unclosed_regular_str(
    span: Span,
    delimiter: LitStrDelimiterKind,
  ) -> Self {
    Self::new(span, delimiter, LitStrKind::Regular)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn unclosed_single_quoted_regular_str(span: Span) -> Self {
    Self::new(span, LitStrDelimiterKind::Single, LitStrKind::Regular)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::yul) const fn unclosed_double_quoted_regular_str(span: Span) -> Self {
    Self::new(span, LitStrDelimiterKind::Double, LitStrKind::Regular)
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

/// The error type for empty regular string literal
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct EmptyStringError {
  span: Span,
  kind: LitStrDelimiterKind,
}

impl EmptyStringError {
  /// Create a new empty string literal error with the given delimiter kind.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, kind: LitStrDelimiterKind) -> Self {
    Self { span, kind }
  }

  /// Get the span of the empty string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the delimiter kind of the empty string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> LitStrDelimiterKind {
    self.kind
  }
}

/// The error type for string literal lexing errors
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum StringError<Char = char> {
  /// Empty string literal.
  ///
  /// Returned when a string literal is empty.
  Empty(EmptyStringError),
  /// Unsupported character or characters found in string literal.
  #[from(skip)]
  Unsupported(Lexeme<Char>),
  /// Unexpected line terminator found in string literal.
  UnexpectedLineTerminator(UnexpectedLexeme<Char, LineTerminator>),
  /// Unclosed string literal.
  Unclosed(Unclosed),
  /// Escape sequence error found in string literal.
  EscapeSequenceError(EscapeSequenceError<Char>),
  /// ... other string literal errors can be added here
  /// e.g., invalid escape sequences, etc.
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
      Self::Empty(err) => match err.kind() {
        LitStrDelimiterKind::Single => {
          write!(f, "empty single-quoted string literal at {}", err.span())
        }
        LitStrDelimiterKind::Double => {
          write!(f, "empty double-quoted string literal at {}", err.span())
        }
      },
      Self::Unclosed(unclosed) => match (unclosed.delimiter_kind(), unclosed.kind()) {
        (LitStrDelimiterKind::Single, LitStrKind::Regular) => {
          write!(f, "unclosed non-empty string literal")
        }
        (LitStrDelimiterKind::Double, LitStrKind::Regular) => {
          write!(f, "unclosed non-empty string literal")
        }
        (LitStrDelimiterKind::Single, LitStrKind::Hex) => {
          write!(f, "unclosed hex string literal")
        }
        (LitStrDelimiterKind::Double, LitStrKind::Hex) => {
          write!(f, "unclosed hex string literal")
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
  /// Create a new empty string literal error with the given delimiter kind.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn empty(span: Span, delimiter: LitStrDelimiterKind) -> Self {
    Self::Empty(EmptyStringError::new(span, delimiter))
  }

  /// Create a new empty double-quoted string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn empty_double_quoted_regular_str(span: Span) -> Self {
    Self::empty(span, LitStrDelimiterKind::Double)
  }

  /// Create a new empty single-quoted string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn empty_single_quoted_regular_str(span: Span) -> Self {
    Self::empty(span, LitStrDelimiterKind::Single)
  }

  /// Create a new unclosed string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed(span: Span, delimiter: LitStrDelimiterKind, kind: LitStrKind) -> Self {
    Self::Unclosed(Unclosed::new(span, delimiter, kind))
  }

  /// Create a new unclosed single-quoted non-empty string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_regular_str(span: Span, delimiter: LitStrDelimiterKind) -> Self {
    Self::unclosed(span, delimiter, LitStrKind::Regular)
  }

  /// Create a new unclosed single-quoted non-empty string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_single_quoted_regular_str(span: Span) -> Self {
    Self::unclosed_regular_str(span, LitStrDelimiterKind::Single)
  }

  /// Create a new unclosed double-quoted non-empty string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_double_quoted_regular_str(span: Span) -> Self {
    Self::unclosed_regular_str(span, LitStrDelimiterKind::Double)
  }

  /// Create a new unclosed hex string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_hex_str(span: Span, delimiter: LitStrDelimiterKind)
    -> Self {
    Self::unclosed(span, delimiter, LitStrKind::Hex)
  }

  /// Create a new unclosed single-quoted hex string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_single_quoted_hex_str(span: Span) -> Self {
    Self::unclosed_hex_str(span, LitStrDelimiterKind::Single)
  }

  /// Create a new unclosed double-quoted hex string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_double_quoted_hex_str(span: Span) -> Self {
    Self::unclosed_hex_str(span, LitStrDelimiterKind::Double)
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
}

impl<Char> crate::error::EscapeSequenceError<Char> for StringError<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn unsupported_escape_character(char: PositionedChar<Char>) -> Self {
    Self::unsupported_escape_character(char)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn incomplete_hex_escape_sequence(span: Span) -> Self {
    Self::incomplete_hex_escape_sequence(span)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn incomplete_unicode_escape_sequence(span: Span) -> Self {
    Self::incomplete_unicode_escape_sequence(span)
  }
}

impl<Char> crate::error::RegularStrError<Char> for StringError<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn empty(span: Span, kind: LitStrDelimiterKind) -> Self {
    Self::empty(span, kind)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn unclosed(span: Span, kind: LitStrDelimiterKind) -> Self {
    Self::unclosed_regular_str(span, kind)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn unexpected_line_terminator(lexeme: UnexpectedLexeme<Char, LineTerminator>) -> Self {
    Self::unexpected_line_terminator(lexeme)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn unsupported_character(char: PositionedChar<Char>) -> Self {
    Self::unsupported_character(char)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn unsupported_characters(span: Span) -> Self {
    Self::unsupported_characters(span)
  }
}

impl<Char> crate::error::CustomError for StringError<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn other(message: impl Into<super::Message>) -> Self {
    Self::Other(message.into())
  }
}
