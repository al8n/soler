use derive_more::{From, IsVariant, TryUnwrap, Unwrap};

use crate::string_lexer::*;

mod lossless;
mod syntactic;

mod handlers;


/// The error type for Yul lexing
pub mod error;




/// The boolean literal of Yul
///
/// Spec: [Yul boolean literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBoolean)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitBool<S> {
  /// The `true` literal
  True(S),
  /// The `false` literal
  False(S),
}

/// The kind of string literal of Yul
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
pub enum LitNumberKind {
  /// Decimal number literal
  Decimal,
  /// Hexadecimal number literal
  Hex,
}

/// The number literal of Yul
///
/// Spec:
/// - [Hex number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulHexNumber)
/// - [Decimal number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulDecimalNumber)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum LitNumber<S> {
  /// Decimal number literal
  Decimal(S),
  /// Hexadecimal number literal
  Hexadecimal(S),
}

impl<S> From<LitNumber<S>> for LitNumberKind {
  #[inline]
  fn from(num: LitNumber<S>) -> Self {
    num.kind()
  }
}

impl<S> From<&LitNumber<S>> for LitNumberKind {
  #[inline]
  fn from(num: &LitNumber<S>) -> Self {
    num.kind()
  }
}

impl<S> LitNumber<S> {
  /// Returns the kind of the number literal
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> LitNumberKind {
    match self {
      Self::Decimal(_) => LitNumberKind::Decimal,
      Self::Hexadecimal(_) => LitNumberKind::Hex,
    }
  }
}

/// The kind of string literal of Yul
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
pub enum LitStrKind {
  /// Non-empty string literal
  Regular,
  /// Hex string literal
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
pub enum LitStr<S> {
  /// Non-empty string literal
  Regular(LitRegularStr<S>),
  /// Hex string literal
  Hex(LitHexStr<S>),
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
}

/// The literal of Yul
///
/// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Lit<S> {
  /// The boolean literal
  Boolean(LitBool<S>),
  /// The string literal
  String(LitStr<S>),
  /// The number literal
  Number(LitNumber<S>),
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
    Self::Boolean(LitBool::True(s))
  }
  #[inline]
  pub(super) const fn lit_false(s: S) -> Self {
    Self::Boolean(LitBool::False(s))
  }

  #[inline]
  pub(super) const fn lit_decimal(s: S) -> Self {
    Self::Number(LitNumber::Decimal(s))
  }

  #[inline]
  pub(super) const fn lit_hexadecimal(s: S) -> Self {
    Self::Number(LitNumber::Hexadecimal(s))
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
}

#[cfg(feature = "evm")]
#[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
pub use evm::*;

#[cfg(feature = "evm")]
mod evm {
  macro_rules! builtin {
  ($($name:ident),+$(,)?) => {
    paste::paste! {
      /// The built-in functions of Yul
      ///
      /// Spec: [Yul built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
      #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, derive_more::IsVariant)]
      #[non_exhaustive]
      #[cfg(feature = "evm")]
      #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
      pub enum EvmBuiltinFunction {
        $(
          #[doc = "'" $name "'"]
          [<$name:camel>],
        )+
      }

      impl EvmBuiltinFunction {
        /// Returns the string representation of the built-in function
        #[cfg_attr(not(tarpaulin), inline(always))]
        pub const fn as_str(&self) -> &'static str {
          match self {
            $(
              Self::[<$name:camel>] => stringify!($name),
            )+
          }
        }
      }
    }
  };
}

  builtin!(
    stop,
    add,
    sub,
    mul,
    div,
    sdiv,
    mod,
    smod,
    exp,
    not,
    lt,
    gt,
    slt,
    sgt,
    eq,
    iszero,
    and,
    or,
    xor,
    byte,
    shl,
    shr,
    sar,
    clz,
    addmod,
    mulmod,
    signextend,
    keccak256,
    pop,
    mload,
    mstore,
    mstore8,
    sload,
    sstore,
    tload,
    tstore,
    msize,
    gas,
    address,
    balance,
    selfbalance,
    caller,
    callvalue,
    calldataload,
    calldatasize,
    calldatacopy,
    extcodesize,
    extcodecopy,
    returndatasize,
    returndatacopy,
    mcopy,
    extcodehash,
    create,
    create2,
    call,
    callcode,
    delegatecall,
    staticcall,
    return,
    revert,
    selfdestruct,
    invalid,
    log0,
    log1,
    log2,
    log3,
    log4,
    chainid,
    origin,
    gasprice,
    blockhash,
    blobhash,
    coinbase,
    timestamp,
    number,
    difficulty,
    prevrandao,
    gaslimit,
    basefee,
    blobbasefee,
  );
}
