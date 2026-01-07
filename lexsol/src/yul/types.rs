use derive_more::{Display, From, IsVariant, TryUnwrap, Unwrap};
use tokit::types::{LitFalse, LitTrue};

use crate::types::{
  LitBool, LitDecimal, LitHexStr, LitHexadecimal, LitNumber, LitRegularStr, LitStrDelimiterKind,
};

/// The kind of string literal of Yul
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
pub enum LitStrKind {
  /// Non-empty string literal
  #[display("string")]
  Regular,
  /// Hex string literal
  #[display("hex string")]
  Hex,
}

/// The string literal of Yul
///
/// Spec:
/// - [Yul string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulStringLiteral)
/// - [hex string](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.HexString)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitStr<S = ()> {
  /// Non-empty string literal
  Regular(LitRegularStr<S>),
  /// Hex string literal
  Hex(LitHexStr<S>),
}

impl core::fmt::Display for LitStr {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match (self.kind(), self.delimiter_kind()) {
      (LitStrKind::Regular, LitStrDelimiterKind::Single) => {
        write!(f, "single-quoted string literal")
      }
      (LitStrKind::Regular, LitStrDelimiterKind::Double) => {
        write!(f, "double-quoted string literal")
      }
      (LitStrKind::Hex, LitStrDelimiterKind::Single) => {
        write!(f, "single-quoted hex string literal")
      }
      (LitStrKind::Hex, LitStrDelimiterKind::Double) => {
        write!(f, "double-quoted hex string literal")
      }
    }
  }
}

impl<S> From<LitStr<S>> for LitStrKind {
  #[inline]
  fn from(str: LitStr<S>) -> Self {
    str.kind()
  }
}

impl<S> From<&LitStr<S>> for LitStrKind {
  #[inline]
  fn from(str: &LitStr<S>) -> Self {
    str.kind()
  }
}

impl<S> LitStr<S> {
  /// Returns the delimiter kind of the string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn delimiter_kind(&self) -> LitStrDelimiterKind {
    match self {
      Self::Regular(non_empty) => non_empty.delimiter_kind(),
      Self::Hex(hex) => hex.delimiter_kind(),
    }
  }

  /// Returns the kind of the string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> LitStrKind {
    match self {
      Self::Regular(_) => LitStrKind::Regular,
      Self::Hex(_) => LitStrKind::Hex,
    }
  }

  /// Returns the unit literal of this string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> LitStr<()> {
    match self {
      Self::Regular(non_empty) => LitStr::Regular(non_empty.unit()),
      Self::Hex(hex) => LitStr::Hex(hex.unit()),
    }
  }

  /// Returns the inner source of the string literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_data(self) -> S {
    match self {
      Self::Regular(non_empty) => non_empty.into_data(),
      Self::Hex(hex) => hex.into_data(),
    }
  }

  /// Maps the inner type to another type
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<F, U>(self, f: F) -> LitStr<U>
  where
    F: FnOnce(S) -> U,
  {
    match self {
      Self::Regular(non_empty) => LitStr::Regular(non_empty.map(f)),
      Self::Hex(hex) => LitStr::Hex(hex.map(f)),
    }
  }
}

/// The literal of Yul
///
/// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Lit<S = ()> {
  /// The boolean literal
  Boolean(LitBool<S>),
  /// The string literal
  String(LitStr<S>),
  /// The number literal
  Number(LitNumber<S>),
}

impl core::fmt::Display for Lit {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Boolean(b) => b.fmt(f),
      Self::String(s) => s.fmt(f),
      Self::Number(n) => n.kind().fmt(f),
    }
  }
}

impl<S> From<LitRegularStr<S>> for Lit<S> {
  #[inline]
  fn from(lit: LitRegularStr<S>) -> Self {
    Self::String(lit.into())
  }
}

impl<S> From<LitHexStr<S>> for Lit<S> {
  #[inline]
  fn from(lit: LitHexStr<S>) -> Self {
    Self::String(lit.into())
  }
}

impl<S> Lit<S> {
  #[inline]
  pub(super) const fn lit_true(s: S) -> Self {
    Self::Boolean(LitBool::True(LitTrue::with_data((), s)))
  }

  #[inline]
  pub(super) const fn lit_false(s: S) -> Self {
    Self::Boolean(LitBool::False(LitFalse::with_data((), s)))
  }

  #[inline]
  pub(super) const fn lit_decimal(s: S) -> Self {
    Self::Number(LitNumber::Decimal(LitDecimal::new(s)))
  }

  #[inline]
  pub(super) const fn lit_hexadecimal(s: S) -> Self {
    Self::Number(LitNumber::Hexadecimal(LitHexadecimal::new(s)))
  }

  #[inline]
  pub(super) const fn lit_single_quoted_regular_string(s: S) -> Self {
    Self::String(LitStr::Regular(LitRegularStr::single(s)))
  }

  #[inline]
  pub(super) const fn lit_double_quoted_regular_string(s: S) -> Self {
    Self::String(LitStr::Regular(LitRegularStr::double(s)))
  }

  #[inline]
  pub(super) const fn lit_single_quoted_hex_string(s: S) -> Self {
    Self::String(LitStr::Hex(LitHexStr::single(s)))
  }

  #[inline]
  pub(super) const fn lit_double_quoted_hex_string(s: S) -> Self {
    Self::String(LitStr::Hex(LitHexStr::double(s)))
  }

  /// Maps the inner type to another type
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn map<F, U>(self, f: F) -> Lit<U>
  where
    F: FnOnce(S) -> U,
  {
    match self {
      Self::Boolean(b) => Lit::Boolean(b.map(f)),
      Self::String(s) => Lit::String(s.map(f)),
      Self::Number(n) => Lit::Number(n.map(f)),
    }
  }

  /// Consumes the literal and returns the inner.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_data(self) -> S {
    match self {
      Self::Boolean(b) => b.into_data(),
      Self::String(s) => s.into_data(),
      Self::Number(n) => n.into_data(),
    }
  }

  /// Returns the unit literal of this literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unit(&self) -> Lit<()> {
    match self {
      Self::Boolean(b) => Lit::Boolean(b.unit()),
      Self::String(s) => Lit::String(s.unit()),
      Self::Number(n) => Lit::Number(n.unit()),
    }
  }
}

#[cfg(feature = "evm")]
#[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
pub use evm::*;

#[cfg(feature = "evm")]
mod evm {
  macro_rules! builtin {
    ($($name:literal),+$(,)?) => {
      paste::paste! {
        /// The built-in functions of Yul
        ///
        /// Spec: [Yul built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, derive_more::IsVariant)]
        #[non_exhaustive]
        #[cfg(feature = "evm")]
        #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
        pub enum EvmBuiltinFunction<S = ()> {
          $(
            #[doc = "'" $name "'"]
            [<$name:camel>](S),
          )+
        }

        impl<S> core::fmt::Display for EvmBuiltinFunction<S> {
          #[cfg_attr(not(tarpaulin), inline(always))]
          fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            self.as_str().fmt(f)
          }
        }

        impl EvmBuiltinFunction {
          $(
            #[doc = "Constant for the built-in function '" $name "'"]
            pub const [<$name: upper>]: Self = Self::[<$name: camel>](());
          )+
        }

        impl<S> EvmBuiltinFunction<S> {
          /// Returns the string representation of the built-in function
          #[cfg_attr(not(tarpaulin), inline(always))]
          pub const fn as_str(&self) -> &'static str {
            match self {
              $(
                Self::[<$name:camel>](_) => stringify!($name),
              )+
            }
          }

          /// Returns `true` if the given string is a built-in function
          #[cfg_attr(not(tarpaulin), inline(always))]
          pub fn is_builtin(s: &str) -> bool {
            match s {
              $($name => true,)+
              _ => false,
            }
          }

          /// Maps the inner type to another type
          #[cfg_attr(not(tarpaulin), inline(always))]
          pub fn map<U, F>(self, f: F) -> EvmBuiltinFunction<U>
          where
            F: FnOnce(S) -> U,
          {
            match self {
              $(
                Self::[<$name:camel>](s) => EvmBuiltinFunction::[<$name:camel>](f(s)),
              )+
            }
          }

          /// Returns the unit literal of this built-in function
          #[cfg_attr(not(tarpaulin), inline(always))]
          pub const fn unit(&self) -> EvmBuiltinFunction<()> {
            match self {
              $(
                Self::[<$name:camel>](_) => EvmBuiltinFunction::[<$name:camel>](()),
              )+
            }
          }

          /// Consumes the built-in function and returns the inner.
          #[cfg_attr(not(tarpaulin), inline(always))]
          pub fn into_inner(self) -> S {
            match self {
              $(
                Self::[<$name:camel>](s) => s,
              )+
            }
          }

          /// Returns a reference to the inner.
          #[cfg_attr(not(tarpaulin), inline(always))]
          pub const fn as_inner(&self) -> &S {
            match self {
              $(
                Self::[<$name:camel>](s) => s,
              )+
            }
          }

          /// Returns a mutable reference to the inner.
          #[cfg_attr(not(tarpaulin), inline(always))]
          pub const fn as_inner_mut(&mut self) -> &mut S {
            match self {
              $(
                Self::[<$name:camel>](s) => s,
              )+
            }
          }
        }

        impl<S> tokit::utils::cmp::Equivalent<EvmBuiltinFunction<S>> for str {
          #[cfg_attr(not(tarpaulin), inline(always))]
          fn equivalent(&self, other: &EvmBuiltinFunction<S>) -> bool {
            other.as_str().equivalent(self)
          }
        }
      }
    };
  }

  builtin!(
    "stop",
    "add",
    "sub",
    "mul",
    "div",
    "sdiv",
    "mod",
    "smod",
    "exp",
    "not",
    "lt",
    "gt",
    "slt",
    "sgt",
    "eq",
    "iszero",
    "and",
    "or",
    "xor",
    "byte",
    "shl",
    "shr",
    "sar",
    "clz",
    "addmod",
    "mulmod",
    "signextend",
    "keccak256",
    "pop",
    "mload",
    "mstore",
    "mstore8",
    "sload",
    "sstore",
    "tload",
    "tstore",
    "msize",
    "gas",
    "address",
    "balance",
    "selfbalance",
    "caller",
    "callvalue",
    "calldataload",
    "calldatasize",
    "calldatacopy",
    "extcodesize",
    "extcodecopy",
    "returndatasize",
    "returndatacopy",
    "mcopy",
    "extcodehash",
    "create",
    "create2",
    "call",
    "callcode",
    "delegatecall",
    "staticcall",
    "return",
    "revert",
    "selfdestruct",
    "invalid",
    "log0",
    "log1",
    "log2",
    "log3",
    "log4",
    "chainid",
    "origin",
    "gasprice",
    "blockhash",
    "blobhash",
    "coinbase",
    "timestamp",
    "number",
    "difficulty",
    "prevrandao",
    "gaslimit",
    "basefee",
    "blobbasefee",
  );
}
