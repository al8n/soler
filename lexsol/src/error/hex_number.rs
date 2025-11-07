use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  error::{IncompleteToken, Malformed, UnexpectedSuffix},
  utils::{Lexeme, Span, human_display::DisplayHuman},
};

use crate::Lxr;

/// The hexadecimal literal of Yul
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct HexadecimalLiteral<L>(L);

impl<L: Lxr> HexadecimalLiteral<L> {
  /// The init constant for HexadecimalLiteral
  pub(crate) const INIT: Self = Self(L::INIT);
}

impl<L> core::fmt::Display for HexadecimalLiteral<L>
where
  L: core::fmt::Display,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "{} hex number literal", self.0)
  }
}

impl<L> DisplayHuman for HexadecimalLiteral<L>
where
  L: core::fmt::Display,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

/// The error type for hexadecimal literal lexing errors
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum HexadecimalError<L, Char = char> {
  /// Incomplete hexadecimal literal, missing digits.
  ///
  /// Returned when there is a `0x` prefix but no hexadecimal digits follow.
  Incomplete(IncompleteToken<HexadecimalLiteral<L>>),

  /// Malformed hexadecimal literal.
  ///
  /// Returned when a semi-valid hexadecimal literal is found but is malformed.
  ///
  /// For example,
  /// - `0x12_34`, where underscores are not allowed in Yul hexadecimal literals.
  /// - `0X1264`, where `0X` is not avalid hexadecimal prefix in Yul (should be `0x`).
  /// - `0xx1234`, where multiple `x` characters are not allowed in the prefix.
  Malformed(Malformed<HexadecimalLiteral<L>>),
  /// Unexpected suffix found after hexadecimal literal.
  ///
  /// Returned when there are invalid characters following a valid hexadecimal literal.
  /// For example, `0x1G` where `G` is not a valid hex digit.
  UnexpectedSuffix(UnexpectedSuffix<Char, HexadecimalLiteral<L>>),
}

impl<Char, L> core::fmt::Display for HexadecimalError<L, Char>
where
  Char: DisplayHuman,
  L: Lxr,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Incomplete(_) => {
        write!(
          f,
          "incomplete hexadecimal literal, missing digits after '0x' prefix"
        )
      }
      Self::UnexpectedSuffix(suffix) => match suffix.suffix() {
        Lexeme::Char(pc) => {
          write!(
            f,
            "unexpected character '{}' after hexadecimal literal at {} after hexadecimal literal at {}",
            pc.char_ref().display(),
            pc.position(),
            suffix.token(),
          )
        }
        Lexeme::Range(span) => {
          write!(
            f,
            "unexpected suffix after hexadecimal literal at {} after hexadecimal literal at {}",
            span,
            suffix.token()
          )
        }
      },
      Self::Malformed(malformed) => {
        write!(
          f,
          "malformed hexadecimal literal at {}, {} hex number literal must match the regex ('{}')",
          malformed.span(),
          L::NAME,
          L::HEX_NUMBER_PATTERN
        )
      }
    }
  }
}

impl<L, Char> core::error::Error for HexadecimalError<L, Char>
where
  Char: DisplayHuman + core::fmt::Debug,
  L: Lxr,
{
}

impl<L, Char> HexadecimalError<L, Char>
where
  L: Lxr,
{
  /// Create an unexpected suffix error with the given token and suffix.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_suffix(token: Span, suffix: Lexeme<Char>) -> Self {
    Self::UnexpectedSuffix(
      UnexpectedSuffix::new(token, suffix).with_knowledge_const(HexadecimalLiteral::INIT),
    )
  }

  /// Create a malformed hexadecimal literal error with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn malformed(span: Span) -> Self {
    Self::Malformed(Malformed::with_knowledge(span, HexadecimalLiteral::INIT))
  }

  /// Create an incomplete hexadecimal literal error with the given span.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn incomplete(span: Span) -> Self {
    Self::Incomplete(IncompleteToken::with_knowledge(
      span,
      HexadecimalLiteral::INIT,
    ))
  }
}
