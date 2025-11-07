use derive_more::{IsVariant, TryUnwrap, Unwrap};

pub use denomination::*;
pub use fixed_bytes::*;
pub use int::*;
pub use keywords::*;
pub use reserved::*;
pub use string::*;
pub use uint::*;

use crate::types::{LitBool, LitHexStr, LitNumber, LitRegularStr};

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
pub enum Lit<S> {
  /// The boolean literal
  Boolean(LitBool<S>),
  /// The string literal
  String(LitStr<S>),
  /// The number literal
  Number(LitNumber<S>),
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
    Self::Boolean(LitBool::True(s))
  }
  #[inline]
  pub(super) const fn lit_false(s: S) -> Self {
    Self::Boolean(LitBool::False(s))
  }

  #[inline]
  pub(super) const fn lit_decimal(s: S) -> Self {
    Self::Number(LitNumber::Decimal(s))
  }

  #[inline]
  pub(super) const fn lit_hexadecimal(s: S) -> Self {
    Self::Number(LitNumber::Hexadecimal(s))
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
}
