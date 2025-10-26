use logosky::utils::{LineTerminator, PositionedChar, Span, UnexpectedLexeme};

use crate::string_lexer::LitStrDelimiterKind;

pub use escape_sequence::*;
pub use hex_string::*;
pub use string::*;

mod escape_sequence;
mod hex_string;
mod string;

/// The error types for Yul lexer.
pub mod yul;

#[cfg(any(feature = "std", feature = "alloc"))]
type Message = std::borrow::Cow<'static, str>;

#[cfg(not(any(feature = "std", feature = "alloc")))]
type Message = &'static str;
