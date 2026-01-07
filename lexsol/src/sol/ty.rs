use derive_more::{IsVariant, TryUnwrap, Unwrap};

pub use denomination::*;
pub use fixed_bytes::*;
pub use int::*;
pub use keywords::*;
pub use reserved::*;
pub use string::*;
use tokit::types::{LitFalse, LitTrue};
pub use uint::*;

use crate::types::{LitBool, LitDecimal, LitHexStr, LitHexadecimal, LitNumber, LitRegularStr};

mod denomination;
mod fixed_bytes;
mod int;
mod keywords;
mod reserved;
mod string;
mod uint;

/// The literal of Solidity
///
/// Spec: [Solidity literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Lit<S = ()> {
  /// The boolean literal
  Boolean(LitBool<S>),
  /// The string literal
  String(LitStr<S>),
  /// The number literal
  Number(LitNumber<S>),
}

impl core::fmt::Display for Lit<()> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Boolean(b) => core::fmt::Display::fmt(b, f),
      Self::String(s) => core::fmt::Display::fmt(s, f),
      Self::Number(n) => core::fmt::Display::fmt(n, f),
    }
  }
}

impl<S> core::convert::From<LitRegularStr<S>> for Lit<S> {
  #[inline]
  fn from(lit: LitRegularStr<S>) -> Self {
    Self::String(lit.into())
  }
}

impl<S> core::convert::From<LitHexStr<S>> for Lit<S> {
  #[inline]
  fn from(lit: LitHexStr<S>) -> Self {
    Self::String(lit.into())
  }
}

impl<S> core::convert::From<LitEmptyStr<S>> for Lit<S> {
  #[inline]
  fn from(lit: LitEmptyStr<S>) -> Self {
    Self::String(lit.into())
  }
}

impl<S> core::convert::From<LitUnicodeStr<S>> for Lit<S> {
  #[inline]
  fn from(lit: LitUnicodeStr<S>) -> Self {
    Self::String(lit.into())
  }
}

impl<S> Lit<S> {
  #[inline]
  pub(super) const fn lit_true(s: S) -> Self {
    Self::Boolean(LitBool::True(LitTrue::with_data((), s)))
  }
  #[inline]
  pub(super) const fn lit_false(s: S) -> Self {
    Self::Boolean(LitBool::False(LitFalse::with_data((), s)))
  }

  #[inline]
  pub(super) const fn lit_decimal(s: S) -> Self {
    Self::Number(LitNumber::Decimal(LitDecimal::new(s)))
  }

  #[inline]
  pub(super) const fn lit_hexadecimal(s: S) -> Self {
    Self::Number(LitNumber::Hexadecimal(LitHexadecimal::new(s)))
  }

  #[inline]
  pub(super) const fn lit_empty_single_quoted_string(s: S) -> Self {
    Self::String(LitStr::Empty(LitEmptyStr::single(s)))
  }

  #[inline]
  pub(super) const fn lit_empty_double_quoted_string(s: S) -> Self {
    Self::String(LitStr::Empty(LitEmptyStr::double(s)))
  }

  #[inline]
  pub(super) const fn lit_single_quoted_regular_string(s: S) -> Self {
    Self::String(LitStr::Regular(LitRegularStr::single(s)))
  }

  #[inline]
  pub(super) const fn lit_double_quoted_regular_string(s: S) -> Self {
    Self::String(LitStr::Regular(LitRegularStr::double(s)))
  }

  #[inline]
  pub(super) const fn lit_single_quoted_hex_string(s: S) -> Self {
    Self::String(LitStr::Hex(LitHexStr::single(s)))
  }

  #[inline]
  pub(super) const fn lit_double_quoted_hex_string(s: S) -> Self {
    Self::String(LitStr::Hex(LitHexStr::double(s)))
  }

  #[inline]
  pub(super) const fn lit_single_quoted_unicode_string(s: S) -> Self {
    Self::String(LitStr::Unicode(LitUnicodeStr::single(s)))
  }

  #[inline]
  pub(super) const fn lit_double_quoted_unicode_string(s: S) -> Self {
    Self::String(LitStr::Unicode(LitUnicodeStr::double(s)))
  }

  /// Maps the inner source of the literal to another type
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<F, U>(self, f: F) -> Lit<U>
  where
    F: FnOnce(S) -> U,
  {
    match self {
      Self::Boolean(b) => Lit::Boolean(b.map(f)),
      Self::String(s) => Lit::String(s.map(f)),
      Self::Number(n) => Lit::Number(n.map(f)),
    }
  }

  /// Returns the unit of the literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> Lit {
    match self {
      Self::Boolean(l) => Lit::Boolean(l.unit()),
      Self::String(l) => Lit::String(l.unit()),
      Self::Number(l) => Lit::Number(l.unit()),
    }
  }
}
