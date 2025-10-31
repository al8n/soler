/// Sized signed integer types. int is an alias of int256.
///
/// Spec: [signed integer](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.SignedIntegerType)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Int {
  /// 8-bit signed integer
  I8 = 8,
  /// 16-bit signed integer
  I16 = 16,
  /// 24-bit signed integer
  I24 = 24,
  /// 32-bit signed integer
  I32 = 32,
  /// 40-bit signed integer
  I40 = 40,
  /// 48-bit signed integer
  I48 = 48,
  /// 56-bit signed integer
  I56 = 56,
  /// 64-bit signed integer
  I64 = 64,
  /// 72-bit signed integer
  I72 = 72,
  /// 80-bit signed integer
  I80 = 80,
  /// 88-bit signed integer
  I88 = 88,
  /// 96-bit signed integer
  I96 = 96,
  /// 104-bit signed integer
  I104 = 104,
  /// 112-bit signed integer
  I112 = 112,
  /// 120-bit signed integer
  I120 = 120,
  /// 128-bit signed integer
  I128 = 128,
  /// 136-bit signed integer
  I136 = 136,
  /// 144-bit signed integer
  I144 = 144,
  /// 152-bit signed integer
  I152 = 152,
  /// 160-bit signed integer
  I160 = 160,
  /// 168-bit signed integer
  I168 = 168,
  /// 176-bit signed integer
  I176 = 176,
  /// 184-bit signed integer
  I184 = 184,
  /// 192-bit signed integer
  I192 = 192,
  /// 200-bit signed integer
  I200 = 200,
  /// 208-bit signed integer
  I208 = 208,
  /// 216-bit signed integer
  I216 = 216,
  /// 224-bit signed integer
  I224 = 224,
  /// 232-bit signed integer
  I232 = 232,
  /// 240-bit signed integer
  I240 = 240,
  /// 248-bit signed integer
  I248 = 248,
  /// 256-bit signed integer
  I256 = 256,
}

impl Default for Int {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::new()
  }
}

impl core::fmt::Display for Int {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.as_str().fmt(f)
  }
}

impl Int {
  /// Returns the default `Int` which is `int256`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new() -> Self {
    Self::I256
  }

  /// Returns the size `N` of the `Int`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn size(&self) -> usize {
    *self as usize
  }

  /// Returns the string representation of the `Int`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::I8 => "int8",
      Self::I16 => "int16",
      Self::I24 => "int24",
      Self::I32 => "int32",
      Self::I40 => "int40",
      Self::I48 => "int48",
      Self::I56 => "int56",
      Self::I64 => "int64",
      Self::I72 => "int72",
      Self::I80 => "int80",
      Self::I88 => "int88",
      Self::I96 => "int96",
      Self::I104 => "int104",
      Self::I112 => "int112",
      Self::I120 => "int120",
      Self::I128 => "int128",
      Self::I136 => "int136",
      Self::I144 => "int144",
      Self::I152 => "int152",
      Self::I160 => "int160",
      Self::I168 => "int168",
      Self::I176 => "int176",
      Self::I184 => "int184",
      Self::I192 => "int192",
      Self::I200 => "int200",
      Self::I208 => "int208",
      Self::I216 => "int216",
      Self::I224 => "int224",
      Self::I232 => "int232",
      Self::I240 => "int240",
      Self::I248 => "int248",
      Self::I256 => "int256",
    }
  }
}
