use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{
  Lexeme, LineTerminator, PositionedChar, Span, Unclosed, UnexpectedEot, UnexpectedLexeme,
  UnknownLexeme, human_display::DisplayHuman,
};

use super::Message;
use crate::{
  error::{
    EscapeSequenceError, HexEscapeSequenceError, HexStringError, StringError,
    UnicodeEscapeSequenceError,
  },
  sol::SOLIDITY,
  types::LitStrDelimiterKind,
};

/// The hexadecimal literal of solidity
pub type HexadecimalLiteral = super::hex_number::HexadecimalLiteral<SOLIDITY>;
/// The lexing error type for solidity hexadecimal literal
pub type HexadecimalError<Char = char> = super::hex_number::HexadecimalError<SOLIDITY, Char>;

/// The decimal literal of solidity
pub type DecimalLiteral = super::decimal_number::DecimalLiteral<SOLIDITY>;
/// The lexing error type for solidity decimal literal
pub type DecimalError<Char = char> = super::decimal_number::DecimalError<SOLIDITY, Char>;

/// The error type for unicode string literal lexing errors
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum UnicodeStringError<Char = char> {
  /// Unexpected line terminator found in string literal.
  UnexpectedLineTerminator(UnexpectedLexeme<Char, LineTerminator>),
  /// Unclosed string literal.
  Unclosed(Unclosed<LitStrDelimiterKind>),
  /// Escape sequence error found in string literal.
  EscapeSequenceError(EscapeSequenceError<Char>),
  /// ... other string literal errors can be added here
  Other(super::Message),
}

impl<Char> Default for UnicodeStringError<Char> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::Other("unknown string literal error".into())
  }
}

impl<Char> core::fmt::Display for UnicodeStringError<Char>
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
      Self::Other(msg) => msg.fmt(f),
    }
  }
}

impl<Char> core::error::Error for UnicodeStringError<Char> where
  Char: DisplayHuman + core::fmt::Debug
{
}

impl<Char> UnicodeStringError<Char> {
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

  /// Create a other string literal error with the given message.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn other(message: impl Into<super::Message>) -> Self {
    Self::Other(message.into())
  }
}

/// The lexing error type for solidity
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Error<Char = char, StateError = ()> {
  /// Hexadecimal literal error
  Hexadecimal(HexadecimalError<Char>),
  /// Decimal literal error
  Decimal(DecimalError<Char>),
  /// Unicode string literal error
  UnicodeString(UnicodeStringError<Char>),
  /// String literal error
  String(StringError<Char>),
  /// Hex string literal error
  HexString(HexStringError<Char>),
  /// Unknown lexeme encountered during lexing
  Unknown(UnknownLexeme<Char, SOLIDITY>),
  /// Unexpected end of input.
  UnexpectedEndOfInput(UnexpectedEot),
  /// Lexer state error
  #[from(skip)]
  State(StateError),
  /// Other lexing errors
  Other(Message),
}

impl<Char, StateError> Default for Error<Char, StateError> {
  #[inline]
  fn default() -> Self {
    #[allow(warnings)]
    Self::Other("unknown lexing error".into())
  }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<Char, StateError> From<&'static str> for Error<Char, StateError> {
  #[inline]
  fn from(s: &'static str) -> Self {
    Self::Other(std::borrow::Cow::Borrowed(s))
  }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<Char, StateError> From<std::string::String> for Error<Char, StateError> {
  #[inline]
  fn from(s: String) -> Self {
    Self::Other(std::borrow::Cow::Owned(s))
  }
}

impl<Char, StateError> core::fmt::Display for Error<Char, StateError>
where
  Char: DisplayHuman,
  StateError: core::fmt::Display,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Hexadecimal(err) => err.fmt(f),
      Self::Decimal(err) => err.fmt(f),
      Self::UnicodeString(err) => err.fmt(f),
      Self::String(err) => err.fmt(f),
      Self::HexString(err) => err.fmt(f),
      Self::Unknown(lexme) => match lexme.lexeme() {
        Lexeme::Char(pc) => {
          write!(
            f,
            "unknown character '{}' encountered at {}",
            pc.char_ref().display(),
            pc.position()
          )
        }
        Lexeme::Span(span) => {
          write!(f, "unknown lexeme encountered at {}", span)
        }
      },
      Self::UnexpectedEndOfInput(_) => {
        write!(f, "unexpected end of input")
      }
      Self::State(err) => err.fmt(f),
      Self::Other(msg) => msg.fmt(f),
    }
  }
}

impl<Char, StateError> core::error::Error for Error<Char, StateError>
where
  Char: DisplayHuman + core::fmt::Debug + 'static,
  StateError: core::error::Error + 'static,
{
  fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
    match self {
      Self::Hexadecimal(err) => Some(err),
      Self::Decimal(err) => Some(err),
      Self::UnicodeString(err) => Some(err),
      Self::String(err) => Some(err),
      Self::HexString(err) => Some(err),
      Self::Unknown(err) => Some(err),
      Self::UnexpectedEndOfInput(_) => None,
      Self::State(err) => Some(err),
      Self::Other(_) => None,
    }
  }
}

impl<Char, StateError> Error<Char, StateError> {
  /// Creates an `Other` error with the given message.
  #[inline]
  pub fn other<M>(msg: M) -> Self
  where
    M: Into<Message>,
  {
    Self::Other(msg.into())
  }

  /// Creates an unknown lexeme error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unknown_char(ch: Char, pos: usize) -> Self {
    Self::Unknown(UnknownLexeme::from_char(
      PositionedChar::with_position(ch, pos),
      SOLIDITY(()),
    ))
  }

  /// Creates an unknown lexeme error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unknown_lexeme(span: Span) -> Self {
    Self::Unknown(UnknownLexeme::new(Lexeme::Span(span), SOLIDITY(())))
  }

  /// Creates an unexpected end of input error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_eoi() -> Self {
    Self::UnexpectedEndOfInput(UnexpectedEot::EOT)
  }
}
