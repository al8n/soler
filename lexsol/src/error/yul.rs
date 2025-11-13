use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  error::{DefaultContainer, UnexpectedEot, UnknownLexeme},
  utils::{CharLen, Lexeme, Message, Span, human_display::DisplayHuman},
};

use crate::{
  error::{HexStringError, StringError},
  types::LitStrDelimiterKind,
  yul::sealed::YUL,
};

/// The hexadecimal literal of Yul
pub type HexadecimalLiteral = super::hex_number::HexadecimalLiteral<YUL>;
/// The lexing error type for yul hexadecimal literal
pub type HexadecimalError<Char = char> = super::hex_number::HexadecimalError<YUL, Char>;
/// The decimal literal of Yul
pub type DecimalLiteral = super::decimal_number::DecimalLiteral<YUL>;
/// The lexing error type for yul decimal literal
pub type DecimalError<Char = char> = super::decimal_number::DecimalError<YUL, Char>;

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
  /// Unknown lexeme encountered during lexing
  Unknown(UnknownLexeme<Char, YUL>),
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
    Self::Other(Message::from_static(s))
  }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<Char, StateError> From<std::string::String> for Error<Char, StateError> {
  #[inline]
  fn from(s: String) -> Self {
    Self::Other(Message::from_string(s))
  }
}

impl<Char, StateError> core::fmt::Display for Error<Char, StateError>
where
  Char: DisplayHuman + CharLen,
  StateError: core::fmt::Display,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Hexadecimal(err) => err.fmt(f),
      Self::Decimal(err) => err.fmt(f),
      Self::EmptyString(err) => err.fmt(f),
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

impl<Char, StateError> core::error::Error for Error<Char, StateError>
where
  Char: DisplayHuman + CharLen + core::fmt::Debug + 'static,
  StateError: core::error::Error + 'static,
{
  fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
    match self {
      Self::Hexadecimal(err) => Some(err),
      Self::Decimal(err) => Some(err),
      Self::String(err) => Some(err),
      Self::HexString(err) => Some(err),
      Self::EmptyString(err) => Some(err),
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
    Self::Unknown(UnknownLexeme::from_char(pos, ch, YUL(())))
  }

  /// Creates an unknown lexeme error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unknown_lexeme(span: Span) -> Self {
    Self::Unknown(UnknownLexeme::new(Lexeme::Range(span), YUL(())))
  }

  /// Creates an unexpected end of input error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_eoi(span: Span) -> Self {
    Self::UnexpectedEndOfInput(UnexpectedEot::eot(span))
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
pub struct Errors<
  Char = char,
  StateError = (),
  Container = DefaultContainer<Error<Char, StateError>>,
> {
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

impl<Char, StateError, Container> FromIterator<Error<Char, StateError>>
  for Errors<Char, StateError, Container>
where
  Container: FromIterator<Error<Char, StateError>>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from_iter<I: IntoIterator<Item = Error<Char, StateError>>>(iter: I) -> Self {
    Self::new(Container::from_iter(iter))
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

  /// Create a new error collection with the given capacity.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn with_capacity(capacity: usize) -> Self
  where
    Container: crate::utils::Container<Error<Char, StateError>>,
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
