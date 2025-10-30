/// Unsigned integer types. int is an alias of int256.
///
/// Spec: [unsigned integer](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.UnsignedIntegerType)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Uint {
  /// 8-bit unsigned integer
  U8 = 8,
  /// 16-bit unsigned integer
  U16 = 16,
  /// 24-bit unsigned integer
  U24 = 24,
  /// 32-bit unsigned integer
  U32 = 32,
  /// 40-bit unsigned integer
  U40 = 40,
  /// 48-bit unsigned integer
  U48 = 48,
  /// 56-bit unsigned integer
  U56 = 56,
  /// 64-bit unsigned integer
  U64 = 64,
  /// 72-bit unsigned integer
  U72 = 72,
  /// 80-bit unsigned integer
  U80 = 80,
  /// 88-bit unsigned integer
  U88 = 88,
  /// 96-bit unsigned integer
  U96 = 96,
  /// 104-bit unsigned integer
  U104 = 104,
  /// 112-bit unsigned integer
  U112 = 112,
  /// 120-bit unsigned integer
  U120 = 120,
  /// 128-bit unsigned integer
  U128 = 128,
  /// 136-bit unsigned integer
  U136 = 136,
  /// 144-bit unsigned integer
  U144 = 144,
  /// 152-bit unsigned integer
  U152 = 152,
  /// 160-bit unsigned integer
  U160 = 160,
  /// 168-bit unsigned integer
  U168 = 168,
  /// 176-bit unsigned integer
  U176 = 176,
  /// 184-bit unsigned integer
  U184 = 184,
  /// 192-bit unsigned integer
  U192 = 192,
  /// 200-bit unsigned integer
  U200 = 200,
  /// 208-bit unsigned integer
  U208 = 208,
  /// 216-bit unsigned integer
  U216 = 216,
  /// 224-bit unsigned integer
  U224 = 224,
  /// 232-bit unsigned integer
  U232 = 232,
  /// 240-bit unsigned integer
  U240 = 240,
  /// 248-bit unsigned integer
  U248 = 248,
  /// 256-bit unsigned integer
  U256 = 256,
}

impl Default for Uint {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::new()
  }
}

impl core::fmt::Display for Uint {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.as_str().fmt(f)
  }
}

impl Uint {
  /// Returns the default `Uint` which is `uint256`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new() -> Self {
    Self::U256
  }

  /// Returns the size `N` of the `Unt`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn size(&self) -> usize {
    *self as usize
  }

  /// Returns the string representation of the `Uint`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::U8 => "uint8",
      Self::U16 => "uint16",
      Self::U24 => "uint24",
      Self::U32 => "uint32",
      Self::U40 => "uint40",
      Self::U48 => "uint48",
      Self::U56 => "uint56",
      Self::U64 => "uint64",
      Self::U72 => "uint72",
      Self::U80 => "uint80",
      Self::U88 => "uint88",
      Self::U96 => "uint96",
      Self::U104 => "uint104",
      Self::U112 => "uint112",
      Self::U120 => "uint120",
      Self::U128 => "uint128",
      Self::U136 => "uint136",
      Self::U144 => "uint144",
      Self::U152 => "uint152",
      Self::U160 => "uint160",
      Self::U168 => "uint168",
      Self::U176 => "uint176",
      Self::U184 => "uint184",
      Self::U192 => "uint192",
      Self::U200 => "uint200",
      Self::U208 => "uint208",
      Self::U216 => "uint216",
      Self::U224 => "uint224",
      Self::U232 => "uint232",
      Self::U240 => "uint240",
      Self::U248 => "uint248",
      Self::U256 => "uint256",
    }
  }
}