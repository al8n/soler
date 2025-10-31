pub use escape_sequence::*;
pub use hex_string::*;

pub use string::*;

mod decimal_number;
mod escape_sequence;
mod hex_number;
mod hex_string;
mod string;

/// The error types for Yul lexer.
pub mod yul;

/// The error types for Solidity lexer.
pub mod sol;

#[cfg(any(feature = "std", feature = "alloc"))]
type Message = std::borrow::Cow<'static, str>;

#[cfg(not(any(feature = "std", feature = "alloc")))]
type Message = &'static str;
