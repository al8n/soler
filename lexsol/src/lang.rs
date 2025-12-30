use derive_more::Display;

/// The Solidity language
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display("solidity")]
pub enum Solidity {}
