pub use ty::*;

/// The lossless lexer for Solidity
pub mod lossless;
/// The syntactic lexer for Solidity
pub mod syntactic;

mod handlers;
mod ty;

/// The Solidity language marker.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, derive_more::Display)]
#[display("solidity")]
pub struct SOLIDITY(pub(crate) ());
