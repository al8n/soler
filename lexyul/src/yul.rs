use crate::{LitHexStr, LitNonEmptyStr};

mod lossless;
mod syntactic;

/// The boolean literal of Yul
///
/// Spec: [Yul boolean literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBoolean)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LitBool<S> {
  /// The `true` literal
  True(S),
  /// The `false` literal
  False(S),
}

/// The number literal of Yul
///
/// Spec:
/// - [Hex number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulHexNumber)
/// - [Decimal number literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulDecimalNumber)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LitNumber<S> {
  /// Decimal number literal
  Decimal(S),
  /// Hexadecimal number literal
  Hexadecimal(S),
}

/// The string literal of Yul
///
/// Spec:
/// - [Yul string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulStringLiteral)
/// - [hex string](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.HexString)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LitStr<S> {
  /// Non-empty string literal
  NonEmpty(LitNonEmptyStr<S>),
  /// Hex string literal
  Hex(LitHexStr<S>),
}

/// The literal of Yul
///
/// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Lit<S> {
  /// The boolean literal
  Boolean(LitBool<S>),
  /// The string literal
  String(LitStr<S>),
  /// The number literal
  Number(LitNumber<S>),
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
  pub(super) const fn lit_single_quoted_non_empty_string(s: S) -> Self {
    Self::String(LitStr::NonEmpty(LitNonEmptyStr::Single(s)))
  }

  #[inline]
  pub(super) const fn lit_double_quoted_non_empty_string(s: S) -> Self {
    Self::String(LitStr::NonEmpty(LitNonEmptyStr::Double(s)))
  }

  #[inline]
  pub(super) const fn lit_single_quoted_hex_string(s: S) -> Self {
    Self::String(LitStr::Hex(LitHexStr::Single(s)))
  }

  #[inline]
  pub(super) const fn lit_double_quoted_hex_string(s: S) -> Self {
    Self::String(LitStr::Hex(LitHexStr::Double(s)))
  }
}

macro_rules! builtin {
  ($($name:ident),+$(,)?) => {
    paste::paste! {
      /// The built-in functions of Yul
      ///
      /// Spec: [Yul built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
      #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
      #[non_exhaustive]
      pub enum BuiltinFunction {
        $(
          #[doc = "'" $name "'"]
          [<$name:camel>],
        )+
      }

      impl BuiltinFunction {
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
