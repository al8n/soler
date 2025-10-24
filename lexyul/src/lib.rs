#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![deny(missing_docs)]

#[cfg(all(not(feature = "std"), feature = "alloc"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// Yul lexer
pub mod yul;

/// The hex string literal
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LitHexStr<S> {
  /// Single-quoted hex string literal
  Single(S),
  /// Double-quoted hex string literal
  Double(S),
}

/// The non-empty string literal
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LitNonEmptyStr<S> {
  /// Single-quoted string literal
  Single(S),
  /// Double-quoted string literal
  Double(S),
}
