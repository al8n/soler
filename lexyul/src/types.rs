use derive_more::{IsVariant, TryUnwrap, Unwrap};

/// The boolean literal
///
/// Spec:
/// - [Yul boolean literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBoolean)
/// - [Solidity boolean literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.booleanLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitBool<S> {
  /// The `true` literal
  True(S),
  /// The `false` literal
  False(S),
}

/// The kind of string literal
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
pub enum LitNumberKind {
  /// Decimal number literal
  Decimal,
  /// Hexadecimal number literal
  Hex,
}

/// The number literal
///
/// Spec:
/// - Yul number literal
///   - [Hex number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulHexNumber)
///   - [Decimal number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulDecimalNumber)
/// - [Solidity number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.numberLiteral)
///   - [Hex number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.HexNumber)
///   - [Decimal number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.DecimalNumber)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitNumber<S> {
  /// Decimal number literal
  Decimal(S),
  /// Hexadecimal number literal
  Hexadecimal(S),
}

impl<S> From<LitNumber<S>> for LitNumberKind {
  #[inline]
  fn from(num: LitNumber<S>) -> Self {
    num.kind()
  }
}

impl<S> From<&LitNumber<S>> for LitNumberKind {
  #[inline]
  fn from(num: &LitNumber<S>) -> Self {
    num.kind()
  }
}

impl<S> LitNumber<S> {
  /// Returns the kind of the number literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> LitNumberKind {
    match self {
      Self::Decimal(_) => LitNumberKind::Decimal,
      Self::Hexadecimal(_) => LitNumberKind::Hex,
    }
  }
}

/// The string literal delimiter kind
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
pub enum LitStrDelimiterKind {
  /// `'` single quote
  Single,
  /// `"` double quote
  Double,
}

/// The hex string literal
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LitHexStr<S> {
  delimiter: LitStrDelimiterKind,
  lit: S,
}

impl<S> LitHexStr<S> {
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

  /// Get the delimiter kind of the hex string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn delimiter_kind(&self) -> LitStrDelimiterKind {
    self.delimiter
  }

  /// Returns the source of the hex string literal, source will be `hex"..."` or `hex'...'`, delimiters included
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.lit
  }

  /// Returns the source of the hex string literal, source will be `hex"..."` or `hex'...'`, delimiters included
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.lit
  }
}

/// The non-empty string literal
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LitRegularStr<S> {
  delimiter: LitStrDelimiterKind,
  lit: S,
}

impl<S> LitRegularStr<S> {
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

  /// Get the delimiter kind of the non-empty string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn delimiter_kind(&self) -> LitStrDelimiterKind {
    self.delimiter
  }

  /// Returns the source of the non-empty string literal, source will be `"..."` or `'...'`, delimiters included
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.lit
  }

  /// Returns the source of the non-empty string literal, source will be `"..."` or `'...'`, delimiters included
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.lit
  }
}
