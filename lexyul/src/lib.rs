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

/// Utility functions and types
pub mod utils;

use derive_more::{IsVariant, TryUnwrap, Unwrap};

mod string_lexer;