pub use ty::*;

/// The lossless lexer for Solidity
pub mod lossless;
/// The syntactic lexer for Solidity
pub mod syntactic;

mod handlers;
mod ty;

pub(crate) mod sealed {
  /// The Solidity language marker.
  #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, derive_more::Display)]
  #[display("solidity")]
  #[doc(hidden)]
  pub struct SOLIDITY(pub(crate) ());
}
