#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![allow(clippy::double_parens, clippy::type_complexity)]
// #![deny(missing_docs)]

#[cfg(all(not(feature = "std"), feature = "alloc"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// The scaffold AST/CST nodes for Yul-like languages.
pub mod scaffold;

// /// The AST nodes for Yul language.
// pub mod ast;

// /// The CST nodes for Yul language.
// pub mod cst;

/// Error types for Yul parser.
pub mod error;

/// The syntax for Yul.
pub mod syntax;
