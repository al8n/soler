use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{utils::{
  Lexeme, Span, human_display::DisplayHuman,
}, error::{Malformed, UnexpectedSuffix}};

use crate::Lxr;

/// The decimal literal of Yul
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DecimalLiteral<L>(L);

impl<L: Lxr> DecimalLiteral<L> {
  /// The init constant for decimal literal
  pub(crate) const INIT: Self = Self(L::INIT);
}

impl<L: Lxr> core::fmt::Display for DecimalLiteral<L> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{} decimal number literal", self.0)
  }
}

impl<L: Lxr> DisplayHuman for DecimalLiteral<L> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

/// The error type for decimal literal lexing errors
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum DecimalError<L, Char = char> {
  /// Leading zeros are not allowed in decimal literals.
  ///
  /// Returned when a decimal literal starts with `0` followed by other digits.
  #[from(skip)]
  LeadingZeros(Lexeme<Char>),
  /// Malformed decimal literal.
  ///
  /// Returned when a semi-valid decimal literal is found but is malformed.
  ///
  /// For example,
  /// - `12_34`, where underscores are not allowed in Yul decimal literals.
  Malformed(Malformed<DecimalLiteral<L>>),
  /// Unexpected suffix found after decimal literal.
  ///
  /// Returned when there are invalid characters following a valid decimal literal.
  /// For example, `123a` where `a` is not a valid digit.
  UnexpectedSuffix(UnexpectedSuffix<Char, DecimalLiteral<L>>),
}

impl<Char, L> DecimalError<L, Char>
where
  L: Lxr,
{
  /// Create an unexpected suffix error with the given token and suffix.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_suffix(token: Span, suffix: Lexeme<Char>) -> Self {
    Self::UnexpectedSuffix(
      UnexpectedSuffix::new(token, suffix).with_knowledge_const(DecimalLiteral::INIT),
    )
  }

  /// Create a malformed decimal literal error with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn malformed(span: Span) -> Self {
    Self::Malformed(Malformed::with_knowledge(span, DecimalLiteral::INIT))
  }
}

impl<L, Char> core::fmt::Display for DecimalError<L, Char>
where
  Char: DisplayHuman,
  L: Lxr,
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
      Self::UnexpectedSuffix(suffix) => match suffix.suffix() {
        Lexeme::Char(pc) => {
          write!(
            f,
            "unexpected character '{}' after decimal literal at {} after decimal literal at {}",
            pc.char_ref().display(),
            pc.position(),
            suffix.token(),
          )
        }
        Lexeme::Range(span) => {
          write!(
            f,
            "unexpected suffix after decimal literal at {} after decimal literal at {}",
            span,
            suffix.token()
          )
        }
      },
      Self::Malformed(malformed) => {
        write!(
          f,
          "malformed decimal literal at {}, {} decimal number literal must match the regex ('{}')",
          malformed.span(),
          L::NAME,
          L::DECIMAL_NUMBER_PATTERN
        )
      }
    }
  }
}

impl<Char, L> core::error::Error for DecimalError<L, Char>
where
  Char: DisplayHuman + core::fmt::Debug,
  L: Lxr,
{
}
