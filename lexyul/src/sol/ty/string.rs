use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use crate::types::{LitHexStr, LitRegularStr, LitStrDelimiterKind, LitBool, LitNumber};

/// The kind of string literal of Solidity
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
pub enum LitStrKind {
  /// The empty string literal
  Empty,
  /// Non-empty string literal
  Regular,
  /// Hex string literal
  Hex,
  /// Unicode string literal
  Unicode,
}

/// The empty string literal
/// 
/// Spec:
/// - [Solidity empty string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.EmptyStringLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LitEmptyStr {
  delimiter: LitStrDelimiterKind,
}

impl LitEmptyStr {
  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn new(delimiter: LitStrDelimiterKind) -> Self {
    Self { delimiter }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn single() -> Self {
    Self::new(LitStrDelimiterKind::Single)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn double() -> Self {
    Self::new(LitStrDelimiterKind::Double)
  }

  /// Get the delimiter kind of the non-empty string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn delimiter_kind(&self) -> LitStrDelimiterKind {
    self.delimiter
  }
}

/// The unicode string literal
/// 
/// Spec:
/// - [Solidity unicode string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.UnicodeStringLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LitUnicodeStr<S> {
  delimiter: LitStrDelimiterKind,
  lit: S,
}

impl<S> LitUnicodeStr<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn new(delimiter: LitStrDelimiterKind, lit: S) -> Self {
    Self { delimiter, lit }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn single(lit: S) -> Self {
    Self::new(LitStrDelimiterKind::Single, lit)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn double(lit: S) -> Self {
    Self::new(LitStrDelimiterKind::Double, lit)
  }

  /// Get the delimiter kind of the unicode string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn delimiter_kind(&self) -> LitStrDelimiterKind {
    self.delimiter
  }

  /// Returns the source of the unicode string literal, source will be `unicode"..."` or `unicode'...'`, delimiters included
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.lit
  }

  /// Returns the source of the unicode string literal, source will be `unicode"..."` or `unicode'...'`, delimiters included
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.lit
  }
}

/// The string literal of Solidity
///
/// Spec:
/// - [Solidity string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.stringLiteral)
/// - [hex string](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.HexString)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitStr<S> {
  /// The empty string literal
  Empty(LitEmptyStr),
  /// Non-empty string literal
  Regular(LitRegularStr<S>),
  /// Hex string literal
  Hex(LitHexStr<S>),
  /// Unicode string literal
  Unicode(LitUnicodeStr<S>),
}

impl<S> From<LitStr<S>> for LitStrKind {
  #[inline]
  fn from(str: LitStr<S>) -> Self {
    str.kind()
  }
}

impl<S> From<&LitStr<S>> for LitStrKind {
  #[inline]
  fn from(str: &LitStr<S>) -> Self {
    str.kind()
  }
}

impl<S> LitStr<S> {
  /// Returns the delimiter kind of the string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn delimiter_kind(&self) -> LitStrDelimiterKind {
    match self {
      Self::Regular(non_empty) => non_empty.delimiter_kind(),
      Self::Hex(hex) => hex.delimiter_kind(),
      Self::Empty(empty) => empty.delimiter_kind(),
      Self::Unicode(unicode) => unicode.delimiter_kind(),
    }
  }

  /// Returns the kind of the string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> LitStrKind {
    match self {
      Self::Regular(_) => LitStrKind::Regular,
      Self::Hex(_) => LitStrKind::Hex,
      Self::Empty(_) => LitStrKind::Empty,
      Self::Unicode(_) => LitStrKind::Unicode,
    }
  }
}

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

impl<S> From<LitRegularStr<S>> for Lit<S> {
  #[inline]
  fn from(lit: LitRegularStr<S>) -> Self {
    Self::String(lit.into())
  }
}

impl<S> From<LitHexStr<S>> for Lit<S> {
  #[inline]
  fn from(lit: LitHexStr<S>) -> Self {
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
}

