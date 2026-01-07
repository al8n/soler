use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use tokit::{
  error::{
    DefaultContainer, FixedUnicodeEscapeError, HexEscapeError, IncompleteFixedUnicodeEscape,
    IncompleteHexEscape, Unclosed, UnexpectedEot, UnexpectedLexeme, UnknownLexeme,
  },
  span::{SimpleSpan, Spanned},
  utils::{
    CharLen, EscapedLexeme, Lexeme, Message, PositionedChar, human_display::DisplayHuman,
    knowledge::LineTerminator,
  },
};

use crate::{
  Lxr,
  error::{EscapeSequenceError, HexStringError, StringError},
  sol::Solidity,
  types::LitStrDelimiterKind,
};

/// The hexadecimal literal of solidity
pub type HexadecimalLiteral<Kind> = super::hex_number::HexadecimalLiteral<Solidity<Kind>>;
/// The lexing error type for solidity hexadecimal literal
pub type HexadecimalError<Kind, Char = char> =
  super::hex_number::HexadecimalError<Solidity<Kind>, Char>;

/// The decimal literal of solidity
pub type DecimalLiteral<Kind> = super::decimal_number::DecimalLiteral<Solidity<Kind>>;
/// The lexing error type for solidity decimal literal
pub type DecimalError<Kind, Char = char> =
  super::decimal_number::DecimalError<Solidity<Kind>, Char>;

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
  Other(Spanned<Message>),
}

impl<Char> core::fmt::Display for UnicodeStringError<Char>
where
  Char: DisplayHuman + CharLen,
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

impl<Char> core::error::Error for UnicodeStringError<Char>
where
  Char: DisplayHuman + core::fmt::Debug + CharLen + 'static,
{
  fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
    match self {
      Self::UnexpectedLineTerminator(err) => Some(err),
      Self::Unclosed(err) => Some(err),
      Self::EscapeSequenceError(err) => Some(err),
      Self::Other(_) => None,
    }
  }
}

impl<Char> UnicodeStringError<Char> {
  /// Create a new unclosed string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed(span: SimpleSpan, delimiter: LitStrDelimiterKind) -> Self {
    Self::Unclosed(Unclosed::new(span, delimiter))
  }

  /// Create a new unclosed single-quoted non-empty string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_single_quote(span: SimpleSpan) -> Self {
    Self::unclosed(span, LitStrDelimiterKind::Single)
  }

  /// Create a new unclosed double-quoted non-empty string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_double_quote(span: SimpleSpan) -> Self {
    Self::unclosed(span, LitStrDelimiterKind::Double)
  }

  /// Create a new unexpected line terminator error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_line_terminator(lexeme: UnexpectedLexeme<Char, LineTerminator>) -> Self {
    Self::UnexpectedLineTerminator(lexeme)
  }

  /// Create a unsupported escape character error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unsupported_escape_character(span: SimpleSpan, char: PositionedChar<Char>) -> Self {
    Self::EscapeSequenceError(EscapeSequenceError::unsupported(
      EscapedLexeme::from_positioned_char(span, char),
    ))
  }

  /// Create a incomplete hexadecimal escape sequence error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn incomplete_hex_escape_sequence(span: SimpleSpan) -> Self {
    Self::EscapeSequenceError(EscapeSequenceError::Hexadecimal(
      HexEscapeError::Incomplete(IncompleteHexEscape::new(span)),
    ))
  }

  /// Create a incomplete unicode escape sequence error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn incomplete_unicode_escape_sequence(span: SimpleSpan) -> Self {
    Self::EscapeSequenceError(EscapeSequenceError::Unicode(
      FixedUnicodeEscapeError::Incomplete(IncompleteFixedUnicodeEscape::new(span)),
    ))
  }

  /// Create a other string literal error with the given message.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn other(span: SimpleSpan, message: impl Into<Message>) -> Self {
    Self::Other(Spanned::new(span, message.into()))
  }
}

/// The lexing error type for solidity
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Error<Kind, Char = char, StateError = ()> {
  /// Hexadecimal literal error
  Hexadecimal(HexadecimalError<Kind, Char>),
  /// Decimal literal error
  Decimal(DecimalError<Kind, Char>),
  /// Unicode string literal error
  UnicodeString(UnicodeStringError<Char>),
  /// String literal error
  String(StringError<Char>),
  /// Hex string literal error
  HexString(HexStringError<Char>),
  /// Unknown lexeme encountered during lexing
  Unknown(UnknownLexeme<Char, Solidity<Kind>>),
  /// Unexpected end of input.
  UnexpectedEndOfInput(UnexpectedEot),
  /// Lexer state error
  #[from(skip)]
  State(StateError),
  /// Other lexing errors
  Other(Message),
}

impl<Kind, Char, StateError> Default for Error<Kind, Char, StateError> {
  #[inline]
  fn default() -> Self {
    #[allow(warnings)]
    Self::Other("unknown lexing error".into())
  }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<Kind, Char, StateError> From<&'static str> for Error<Kind, Char, StateError> {
  #[inline]
  fn from(s: &'static str) -> Self {
    Self::Other(Message::from_static(s))
  }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<Kind, Char, StateError> From<std::string::String> for Error<Kind, Char, StateError> {
  #[inline]
  fn from(s: String) -> Self {
    Self::Other(Message::from(s))
  }
}

impl<Kind, Char, StateError> core::fmt::Display for Error<Kind, Char, StateError>
where
  Char: DisplayHuman + CharLen,
  StateError: core::fmt::Display,
  Kind: core::fmt::Debug,
  Solidity<Kind>: Lxr,
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
        Lexeme::Range(span) => {
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

impl<Kind, Char, StateError> core::error::Error for Error<Kind, Char, StateError>
where
  Char: DisplayHuman + CharLen + core::fmt::Debug + 'static,
  StateError: core::error::Error + 'static,
  Kind: core::fmt::Debug + 'static,
  Solidity<Kind>: Lxr,
{
  #[inline]
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

impl<Kind, Char, StateError> Error<Kind, Char, StateError> {
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
    Self::Unknown(UnknownLexeme::from_char(pos, ch, Solidity::new()))
  }

  /// Creates an unknown lexeme error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unknown_lexeme(span: SimpleSpan) -> Self {
    Self::Unknown(UnknownLexeme::new(Lexeme::Range(span), Solidity::new()))
  }

  /// Creates an unexpected end of input error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_eoi(offset: usize) -> Self {
    Self::UnexpectedEndOfInput(UnexpectedEot::eot(offset))
  }
}

/// A collection of errors
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Errors<
  Kind,
  Char = char,
  StateError = (),
  Container = DefaultContainer<Error<Kind, Char, StateError>>,
> {
  errors: Container,
  _m: core::marker::PhantomData<Error<Kind, Char, StateError>>,
}

impl<Kind, Char, StateError, Container> Default for Errors<Kind, Char, StateError, Container>
where
  Container: Default,
{
  #[inline]
  fn default() -> Self {
    Self {
      errors: Container::default(),
      _m: core::marker::PhantomData,
    }
  }
}

impl<Kind, Char, StateError, Container> From<Error<Kind, Char, StateError>>
  for Errors<Kind, Char, StateError, Container>
where
  Container: FromIterator<Error<Kind, Char, StateError>>,
{
  #[inline]
  fn from(error: Error<Kind, Char, StateError>) -> Self {
    Self {
      errors: Container::from_iter([error]),
      _m: core::marker::PhantomData,
    }
  }
}

impl<Kind, Char, StateError, Container> IntoIterator for Errors<Kind, Char, StateError, Container>
where
  Container: IntoIterator<Item = Error<Kind, Char, StateError>>,
{
  type Item = Error<Kind, Char, StateError>;
  type IntoIter = Container::IntoIter;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.errors.into_iter()
  }
}

impl<Kind, Char, StateError, Container> core::ops::Deref
  for Errors<Kind, Char, StateError, Container>
{
  type Target = Container;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn deref(&self) -> &Self::Target {
    &self.errors
  }
}

impl<Kind, Char, StateError, Container> core::ops::DerefMut
  for Errors<Kind, Char, StateError, Container>
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.errors
  }
}

impl<Kind, Char, StateError, Container> AsRef<[Error<Kind, Char, StateError>]>
  for Errors<Kind, Char, StateError, Container>
where
  Container: AsRef<[Error<Kind, Char, StateError>]>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &[Error<Kind, Char, StateError>] {
    self.as_slice()
  }
}

impl<Kind, Char, StateError, Container> AsMut<[Error<Kind, Char, StateError>]>
  for Errors<Kind, Char, StateError, Container>
where
  Container: AsMut<[Error<Kind, Char, StateError>]>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_mut(&mut self) -> &mut [Error<Kind, Char, StateError>] {
    self.as_mut_slice()
  }
}

impl<Kind, Char, StateError, Container> FromIterator<Error<Kind, Char, StateError>>
  for Errors<Kind, Char, StateError, Container>
where
  Container: FromIterator<Error<Kind, Char, StateError>>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from_iter<I: IntoIterator<Item = Error<Kind, Char, StateError>>>(iter: I) -> Self {
    Self::new(Container::from_iter(iter))
  }
}

impl<Kind, Char, StateError, Container> Errors<Kind, Char, StateError, Container> {
  /// Create a new error collection with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(errors: Container) -> Self {
    Self {
      errors,
      _m: core::marker::PhantomData,
    }
  }

  /// Create a new error collection with the given capacity.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn with_capacity(capacity: usize) -> Self
  where
    Container: crate::utils::Container<Error<Kind, Char, StateError>>,
  {
    Self::new(Container::with_capacity(capacity))
  }

  /// Consumes the `Errors`, returning the underlying container.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_inner(self) -> Container {
    self.errors
  }

  /// Returns a reference to the internal error container.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn errors(&self) -> &Container {
    &self.errors
  }

  /// Returns a mutable reference to the internal error container.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn errors_mut(&mut self) -> &mut Container {
    &mut self.errors
  }

  /// Returns a slice of all errors in the collection.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn as_slice(&self) -> &[Error<Kind, Char, StateError>]
  where
    Container: AsRef<[Error<Kind, Char, StateError>]>,
  {
    self.errors().as_ref()
  }

  /// Returns a mutable slice of all errors in the collection.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn as_mut_slice(&mut self) -> &mut [Error<Kind, Char, StateError>]
  where
    Container: AsMut<[Error<Kind, Char, StateError>]>,
  {
    self.errors_mut().as_mut()
  }
}

impl<Kind, Char, StateError, Container> crate::utils::Wrapper
  for Errors<Kind, Char, StateError, Container>
{
  type Underlying = Container;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from_underlying(underlying: Self::Underlying) -> Self {
    Self::new(underlying)
  }
}
