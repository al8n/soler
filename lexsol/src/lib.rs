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

/// Solidity lexer
pub mod sol;

/// The error types for the lexer.
pub mod error;

/// Utility functions and types
pub mod utils;

/// The types used in the lexer.
pub mod types;

mod string_lexer;

mod handlers;

#[doc(hidden)]
pub trait Lxr: sealed::Sealed {}

mod sealed {
  pub trait Sealed: core::fmt::Display + core::fmt::Debug + Copy + 'static {
    const INIT: Self;
    const NAME: &'static str;
    const HEX_NUMBER_PATTERN: &'static str;
    const DECIMAL_NUMBER_PATTERN: &'static str;
  }

  impl Sealed for super::yul::sealed::YUL {
    const INIT: Self = super::yul::sealed::YUL(());
    const NAME: &'static str = "yul";
    const DECIMAL_NUMBER_PATTERN: &'static str = r"0|[1-9][0-9]*";
    const HEX_NUMBER_PATTERN: &'static str = r"0x[0-9a-fA-F]+";
  }

  impl Sealed for super::sol::sealed::SOLIDITY {
    const INIT: Self = super::sol::sealed::SOLIDITY(());
    const NAME: &'static str = "solidity";
    const DECIMAL_NUMBER_PATTERN: &'static str = r"0|[1-9](_?[0-9_])*";
    const HEX_NUMBER_PATTERN: &'static str = r"0x[0-9a-fA-F_]+";
  }

  impl<T: Sealed> super::Lxr for T {}
}
