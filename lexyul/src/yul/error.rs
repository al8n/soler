use logosky::utils::{Lexeme, PositionedChar, Span, UnexpectedEnd, UnexpectedLexeme, human_display::DisplayHuman};
use derive_more::{From, IsVariant, Unwrap, TryUnwrap};

mod string;



#[cfg(not(any(feature = "std", feature = "alloc")))]
type Message = &'static str;
#[cfg(any(feature = "std", feature = "alloc"))]
type Message = std::borrow::Cow<'static, str>;

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
  UnexpectedSuffix(PositionedChar<Char>),
}

impl<Char> core::fmt::Display for HexadecimalError<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Incomplete(_) => {
        write!(f, "incomplete hexadecimal literal, missing digits after '0x' prefix")
      }
      Self::UnexpectedSuffix(pos_char) => {
        write!(f, "unexpected suffix '{}' after hexadecimal literal at {}", pos_char.char_ref().display(), pos_char.position())
      }
    }
  }
}

impl<Char> core::error::Error for HexadecimalError<Char>
where
  Char: DisplayHuman + core::fmt::Debug,
{}

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
  UnexpectedSuffix(PositionedChar<Char>),
}

impl<Char> core::fmt::Display for DecimalError<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::LeadingZeros(l) => if l.is_char() {
        write!(f, "leading zero is not allowed in decimal literal")
      } else {
        write!(f, "leading zeros are not allowed in decimal literal")
      },
      Self::UnexpectedSuffix(pos_char) => {
        write!(f, "unexpected suffix '{}' after decimal literal at {}", pos_char.char_ref().display(), pos_char.position())
      }
    }
  }
}

impl<Char> core::error::Error for DecimalError<Char>
where
  Char: DisplayHuman + core::fmt::Debug,
{}


/// The lexing error type for Yul
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Error<Char = char> {
  /// Hexadecimal literal error
  Hexadecimal(HexadecimalError<Char>),
  /// Decimal literal error
  Decimal(DecimalError<Char>),
  /// String literal error
  String(StringError<Char>),
  /// Unknown character encountered during lexing
  #[from(skip)]
  UnknownCharacter(PositionedChar<Char>),
  /// Other lexing errors
  Other(Message),
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<Char> From<&'static str> for Error<Char> {
  #[inline]
  fn from(s: &'static str) -> Self {
    Self::Other(std::borrow::Cow::Borrowed(s))
  }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<Char> From<std::string::String> for Error<Char> {
  #[inline]
  fn from(s: String) -> Self {
    Self::Other(std::borrow::Cow::Owned(s))
  }
}

impl<Char> core::fmt::Display for Error<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Hexadecimal(err) => err.fmt(f),
      Self::Decimal(err) => err.fmt(f),
      Self::String(err) => err.fmt(f),
      Self::UnknownCharacter(pos_char) => {
        write!(f, "unknown character '{}' encountered at {}", pos_char.char_ref().display(), pos_char.position())
      }
      Self::Other(msg) => msg.fmt(f),
    }
  }
}

impl<Char> core::error::Error for Error<Char>
where
  Char: DisplayHuman + core::fmt::Debug + 'static,
{
  fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
    match self {
      Self::Hexadecimal(err) => Some(err),
      Self::Decimal(err) => Some(err),
      Self::String(err) => Some(err),
      Self::UnknownCharacter(_) => None,
      Self::Other(_) => None,
    }
  }
}

impl<Char> Error<Char> {
  /// Creates an `Other` error with the given message.
  #[inline]
  pub fn other<M>(msg: M) -> Self
  where
    M: Into<Message>,
  {
    Self::Other(msg.into())
  }
}
