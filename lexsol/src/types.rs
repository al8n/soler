use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use tokit::{span::Spanned, types::Ident};

/// The keywords
pub mod keywords;

/// The punctuators
pub mod punct;

/// The `true` literal
pub type LitTrue<S = (), Lang = ()> = tokit::types::LitTrue<S, (), Lang>;

/// The `false` literal
pub type LitFalse<S = (), Lang = ()> = tokit::types::LitFalse<S, (), Lang>;

/// The boolean literal
///
/// Spec:
/// - [Yul boolean literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBoolean)
/// - [Solidity boolean literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.booleanLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitBool<S = (), Lang: ?Sized = ()> {
  /// The `true` literal
  True(LitTrue<S, Lang>),
  /// The `false` literal
  False(LitFalse<S, Lang>),
}

impl core::fmt::Display for LitBool {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::True(_) => "true".fmt(f),
      Self::False(_) => "false".fmt(f),
    }
  }
}

impl<S, Lang: ?Sized> LitBool<S, Lang> {
  /// Map the inner source to another source
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<U>(self, f: impl FnOnce(S) -> U) -> LitBool<U, Lang> {
    match self {
      Self::True(s) => LitBool::True(s.map_data(f)),
      Self::False(s) => LitBool::False(s.map_data(f)),
    }
  }

  /// Returns the unit literal of this boolean literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> LitBool<(), Lang> {
    match self {
      Self::True(_) => LitBool::True(LitTrue::unit()),
      Self::False(_) => LitBool::False(LitFalse::unit()),
    }
  }

  /// Returns the inner source of the boolean literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_data(self) -> S {
    match self {
      Self::True(s) => s.into_data(),
      Self::False(s) => s.into_data(),
    }
  }

  /// Converts into ident
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_identifier<Span>(this: Spanned<Self, Span>) -> Ident<S, Span, Lang> {
    let span = this.span;
    let source = match this.data {
      LitBool::True(s) => s.into_data(),
      LitBool::False(s) => s.into_data(),
    };
    Ident::new(span, source)
  }
}

/// The kind of string literal
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
pub enum LitNumberKind {
  /// Decimal number literal
  #[display("decimal")]
  Decimal,
  /// Hexadecimal number literal
  #[display("hexadecimal")]
  Hex,
}

/// The decimal number literal for Yul or Solidity
///
/// Spec:
/// - [Yul decimal number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulDecimalNumber)
/// - [Solidity decimal number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.DecimalNumber)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LitDecimal<S = ()>(S);

impl core::fmt::Display for LitDecimal<()> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "decimal number literal")
  }
}

impl<S> LitDecimal<S> {
  /// Creates a new decimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(lit: S) -> Self {
    Self(lit)
  }

  /// Returns the source of the decimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.0
  }

  /// Returns the source of the decimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.0
  }

  /// Converts into ident
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_identifier<Span, Lang>(this: Spanned<Self, Span>) -> Ident<S, Span, Lang> {
    Ident::new(this.span, this.data.0)
  }

  /// Map the inner source to another source
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<U>(self, f: impl FnOnce(S) -> U) -> LitDecimal<U> {
    LitDecimal(f(self.0))
  }

  /// Returns the unit literal of this decimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> LitDecimal<()> {
    LitDecimal(())
  }

  /// Returns the inner source of the decimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_data(self) -> S {
    self.0
  }
}

/// The hexadecimal number literal for Yul or Solidity
///
/// Spec:
///   - [Yul hex number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulHexNumber)
///   - [Solidity Hex number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.HexNumber)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LitHexadecimal<S = ()>(S);

impl core::fmt::Display for LitHexadecimal {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "hexadecimal number literal")
  }
}

impl<S> LitHexadecimal<S> {
  /// Creates a new decimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(lit: S) -> Self {
    Self(lit)
  }

  /// Returns the source of the decimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.0
  }

  /// Returns the source of the decimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.0
  }

  /// Converts into ident
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_identifier<Span, Lang>(this: Spanned<Self, Span>) -> Ident<S, Span, Lang> {
    Ident::new(this.span, this.data.0)
  }

  /// Map the inner source to another source
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<U>(self, f: impl FnOnce(S) -> U) -> LitHexadecimal<U> {
    LitHexadecimal(f(self.0))
  }

  /// Returns the unit literal of this hexadecimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> LitHexadecimal<()> {
    LitHexadecimal(())
  }

  /// Returns the inner source of the hexadecimal literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_data(self) -> S {
    self.0
  }
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitNumber<S = ()> {
  /// Decimal number literal
  Decimal(LitDecimal<S>),
  /// Hexadecimal number literal
  Hexadecimal(LitHexadecimal<S>),
}

impl core::fmt::Display for LitNumber<()> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Decimal(d) => core::fmt::Display::fmt(d, f),
      Self::Hexadecimal(h) => core::fmt::Display::fmt(h, f),
    }
  }
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

  /// Converts into ident
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_identifier<Span, Lang>(this: Spanned<Self, Span>) -> Ident<S, Span, Lang> {
    let (span, data) = (this.span, this.data);
    let source = match data {
      LitNumber::Decimal(lit) => lit.0,
      LitNumber::Hexadecimal(lit) => lit.0,
    };
    Ident::new(span, source)
  }

  /// Map the inner source to another source
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<U>(self, f: impl FnOnce(S) -> U) -> LitNumber<U> {
    match self {
      Self::Decimal(s) => LitNumber::Decimal(s.map(f)),
      Self::Hexadecimal(s) => LitNumber::Hexadecimal(s.map(f)),
    }
  }

  /// Returns the unit literal of this number literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> LitNumber<()> {
    match self {
      Self::Decimal(d) => LitNumber::Decimal(d.unit()),
      Self::Hexadecimal(h) => LitNumber::Hexadecimal(h.unit()),
    }
  }

  /// Returns the inner source of the number literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_data(self) -> S {
    match self {
      Self::Decimal(d) => d.into_data(),
      Self::Hexadecimal(h) => h.into_data(),
    }
  }
}

/// The string literal delimiter kind
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum LitStrDelimiterKind {
  /// `'` single quote
  #[display("'")]
  Single,
  /// `"` double quote
  #[display("\"")]
  Double,
}

/// The hex string literal
///
/// Spec: [hex string](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.HexString)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LitHexStr<S = ()> {
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

  /// Returns the unit literal of this hex string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> LitHexStr<()> {
    LitHexStr::new(self.delimiter, ())
  }

  /// Returns the inner source of the hex string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_data(self) -> S {
    self.lit
  }

  /// Maps the inner source to another source
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<F, U>(self, f: F) -> LitHexStr<U>
  where
    F: FnOnce(S) -> U,
  {
    LitHexStr::new(self.delimiter, f(self.lit))
  }
}

/// The non-empty string literal
///
/// Spec:
/// - [Solidity non-empty string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.NonEmptyStringLiteral)
/// - [Yul string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulStringLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LitRegularStr<S = ()> {
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

  /// Returns the inner source of the string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_data(self) -> S {
    self.lit
  }

  /// Returns the unit literal of this string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> LitRegularStr<()> {
    LitRegularStr::new(self.delimiter, ())
  }

  /// Maps the inner source to another source
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<F, U>(self, f: F) -> LitRegularStr<U>
  where
    F: FnOnce(S) -> U,
  {
    LitRegularStr::new(self.delimiter, f(self.lit))
  }
}
