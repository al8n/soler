use crate::types::{LitBool, LitHexStr, LitNumber, LitRegularStr, LitStrDelimiterKind};
use derive_more::{From, IsVariant, TryUnwrap, Unwrap};

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
pub struct LitEmptyStr<S> {
  delimiter: LitStrDelimiterKind,
  source: S,
}

impl<S> LitEmptyStr<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn new(source: S, delimiter: LitStrDelimiterKind) -> Self {
    Self { delimiter, source }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn single(source: S) -> Self {
    Self::new(source, LitStrDelimiterKind::Single)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(super) const fn double(source: S) -> Self {
    Self::new(source, LitStrDelimiterKind::Double)
  }

  /// Get the delimiter kind of the non-empty string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn delimiter_kind(&self) -> LitStrDelimiterKind {
    self.delimiter
  }

  /// Returns the source of the empty string literal, source will be `""` or `''`, delimiters included
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.source
  }

  /// Returns the source of the empty string literal, source will be `""` or `''`, delimiters included
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.source
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
  pub(crate) const fn single(lit: S) -> Self {
    Self::new(LitStrDelimiterKind::Single, lit)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn double(lit: S) -> Self {
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
  Empty(LitEmptyStr<S>),
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
