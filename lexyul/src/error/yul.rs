use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::utils::{Lexeme, PositionedChar, Span, human_display::DisplayHuman};

use crate::{error::{HexStringError, StringError}, string_lexer::LitStrDelimiterKind};
use super::Message;

/// The error type for empty regular string literal
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct EmptyStringError {
  span: Span,
  kind: LitStrDelimiterKind,
}

impl core::fmt::Display for EmptyStringError {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self.kind {
      LitStrDelimiterKind::Single => {
        write!(f, "empty single-quoted string literal at {}", self.span)
      }
      LitStrDelimiterKind::Double => {
        write!(f, "empty double-quoted string literal at {}", self.span)
      }
    }
  }
}

impl core::error::Error for EmptyStringError {}

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

/// The error type for hexadecimal literal lexing errors
#[derive(Debug, PartialEq, Eq, Clone, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum HexadecimalError<Char = char> {
  /// Incomplete hexadecimal literal, missing digits.
  ///
  /// Returned when there is a `0x` prefix but no hexadecimal digits follow.
  Incomplete(Span),
  /// Unexpected suffix found after hexadecimal literal.
  ///
  /// Returned when there are invalid characters following a valid hexadecimal literal.
  /// For example, `0x1G` where `G` is not a valid hex digit.
  UnexpectedSuffix(Lexeme<Char>),
}

impl<Char> core::fmt::Display for HexadecimalError<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Incomplete(_) => {
        write!(
          f,
          "incomplete hexadecimal literal, missing digits after '0x' prefix"
        )
      }
      Self::UnexpectedSuffix(suffix) => match suffix {
        Lexeme::Char(pc) => {
          write!(
            f,
            "unexpected character '{}' after hexadecimal literal at {}",
            pc.char_ref().display(),
            pc.position()
          )
        }
        Lexeme::Span(span) => {
          write!(f, "unexpected suffix after hexadecimal literal at {}", span)
        }
      },
    }
  }
}

impl<Char> core::error::Error for HexadecimalError<Char> where Char: DisplayHuman + core::fmt::Debug {}

/// The error type for decimal literal lexing errors
#[derive(Debug, PartialEq, Eq, Clone, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum DecimalError<Char = char> {
  /// Leading zeros are not allowed in decimal literals.
  ///
  /// Returned when a decimal literal starts with `0` followed by other digits.
  LeadingZeros(Lexeme<Char>),
  /// Unexpected suffix found after decimal literal.
  ///
  /// Returned when there are invalid characters following a valid decimal literal.
  /// For example, `123a` where `a` is not a valid digit.
  UnexpectedSuffix(Lexeme<Char>),
}

impl<Char> core::fmt::Display for DecimalError<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::LeadingZeros(l) => {
        if l.is_char() {
          write!(f, "leading zero is not allowed in decimal literal")
        } else {
          write!(f, "leading zeros are not allowed in decimal literal")
        }
      }
      Self::UnexpectedSuffix(suffix) => match suffix {
        Lexeme::Char(pc) => {
          write!(
            f,
            "unexpected character '{}' after decimal literal at {}",
            pc.char_ref().display(),
            pc.position()
          )
        }
        Lexeme::Span(span) => {
          write!(f, "unexpected suffix after decimal literal at {}", span)
        }
      },
    }
  }
}

impl<Char> core::error::Error for DecimalError<Char> where Char: DisplayHuman + core::fmt::Debug {}

/// The lexing error type for Yul
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Error<Char = char, StateError = ()> {
  /// Hexadecimal literal error
  Hexadecimal(HexadecimalError<Char>),
  /// Decimal literal error
  Decimal(DecimalError<Char>),
  /// Empty string error
  EmptyString(EmptyStringError),
  /// String literal error
  String(StringError<Char>),
  /// Hex string literal error
  HexString(HexStringError<Char>),
  /// Unknown character encountered during lexing
  #[from(skip)]
  UnknownCharacter(PositionedChar<Char>),
  /// Unexpected end of input.
  UnexpectedEndOfInput(usize),
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
      Self::EmptyString(err) => err.fmt(f),
      Self::String(err) => err.fmt(f),
      Self::HexString(err) => err.fmt(f),
      Self::UnknownCharacter(pos_char) => {
        write!(
          f,
          "unknown character '{}' encountered at {}",
          pos_char.char_ref().display(),
          pos_char.position()
        )
      }
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
      Self::String(err) => Some(err),
      Self::HexString(err) => Some(err),
      Self::EmptyString(err) => Some(err),
      Self::UnknownCharacter(_) => None,
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

  /// Creates an unknown character error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unknown_char(ch: Char, pos: usize) -> Self {
    Self::UnknownCharacter(PositionedChar::with_position(ch, pos))
  }

  /// Creates an unexpected end of input error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_eoi(pos: usize) -> Self {
    Self::UnexpectedEndOfInput(pos)
  }

  /// Creates a empty single-quoted string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn empty_single_quote(span: Span) -> Self {
    Self::EmptyString(EmptyStringError::new(span, LitStrDelimiterKind::Single))
  }

  /// Creates a empty double-quoted string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn empty_double_quote(span: Span) -> Self {
    Self::EmptyString(EmptyStringError::new(span, LitStrDelimiterKind::Double))
  }
}

/// A collection of errors
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg(not(any(feature = "std", feature = "alloc")))]
pub struct Errors<
  Char = char,
  StateError = (),
  Container = crate::utils::GenericVec<Error<Char, StateError>, 2>,
> {
  errors: Container,
  _m: core::marker::PhantomData<Error<Char, StateError>>,
}

/// A collection of errors
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg(any(feature = "std", feature = "alloc"))]
pub struct Errors<Char = char, StateError = (), Container = std::vec::Vec<Error<Char, StateError>>>
{
  errors: Container,
  _m: core::marker::PhantomData<Error<Char, StateError>>,
}

impl<Char, StateError, Container> Default for Errors<Char, StateError, Container>
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

impl<Char, StateError, Container> From<Error<Char, StateError>>
  for Errors<Char, StateError, Container>
where
  Container: FromIterator<Error<Char, StateError>>,
{
  #[inline]
  fn from(error: Error<Char, StateError>) -> Self {
    Self {
      errors: Container::from_iter([error]),
      _m: core::marker::PhantomData,
    }
  }
}

impl<Char, StateError, Container> IntoIterator for Errors<Char, StateError, Container>
where
  Container: IntoIterator<Item = Error<Char, StateError>>,
{
  type Item = Error<Char, StateError>;
  type IntoIter = Container::IntoIter;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.errors.into_iter()
  }
}

impl<Char, StateError, Container> core::ops::Deref for Errors<Char, StateError, Container> {
  type Target = Container;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn deref(&self) -> &Self::Target {
    &self.errors
  }
}

impl<Char, StateError, Container> core::ops::DerefMut for Errors<Char, StateError, Container> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.errors
  }
}

impl<Char, StateError, Container> AsRef<[Error<Char, StateError>]>
  for Errors<Char, StateError, Container>
where
  Container: AsRef<[Error<Char, StateError>]>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &[Error<Char, StateError>] {
    self.as_slice()
  }
}

impl<Char, StateError, Container> AsMut<[Error<Char, StateError>]>
  for Errors<Char, StateError, Container>
where
  Container: AsMut<[Error<Char, StateError>]>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_mut(&mut self) -> &mut [Error<Char, StateError>] {
    self.as_mut_slice()
  }
}

impl<Char, StateError, Container> Errors<Char, StateError, Container> {
  /// Create a new error collection with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(errors: Container) -> Self {
    Self {
      errors,
      _m: core::marker::PhantomData,
    }
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
  pub fn as_slice(&self) -> &[Error<Char, StateError>]
  where
    Container: AsRef<[Error<Char, StateError>]>,
  {
    self.errors().as_ref()
  }

  /// Returns a mutable slice of all errors in the collection.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn as_mut_slice(&mut self) -> &mut [Error<Char, StateError>]
  where
    Container: AsMut<[Error<Char, StateError>]>,
  {
    self.errors_mut().as_mut()
  }
}

impl<Char, StateError, Container> crate::utils::Wrapper for Errors<Char, StateError, Container> {
  type Underlying = Container;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from_underlying(underlying: Self::Underlying) -> Self {
    Self::new(underlying)
  }
}
