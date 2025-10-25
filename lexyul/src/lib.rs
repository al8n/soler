#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![allow(clippy::double_parens)]
#![deny(missing_docs)]

#[cfg(all(not(feature = "std"), feature = "alloc"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Yul lexer
pub mod yul;

/// The error types for the lexer.
pub mod error;

use derive_more::{IsVariant, Unwrap, TryUnwrap};

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
  const fn single(lit: S) -> Self {
    Self::new(LitStrDelimiterKind::Single, lit)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn double(lit: S) -> Self {
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
pub struct LitNonEmptyStr<S> {
  delimiter: LitStrDelimiterKind,
  lit: S,
}

impl<S> LitNonEmptyStr<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn new(delimiter: LitStrDelimiterKind, lit: S) -> Self {
    Self { delimiter, lit }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn single(lit: S) -> Self {
    Self::new(LitStrDelimiterKind::Single, lit)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn double(lit: S) -> Self {
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
