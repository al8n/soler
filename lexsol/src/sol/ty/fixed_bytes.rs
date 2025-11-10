macro_rules! define_fixed_bytes {
  ($($lit:literal),+$(,)?) => {
    paste::paste! {
      /// A type representing fixed-size byte arrays (bytes1 to bytes32).
      ///
      /// Spec: [fixed-bytes](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.FixedBytes)
      #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::IsVariant)]
      #[repr(u8)]
      pub enum FixedBytes {
        $(
          #[doc = "The `bytes" $lit "` type."]
          [<Bytes $lit>] = $lit,
        )*
      }

      impl FixedBytes {
        $(
          #[doc = "A constant representing the `bytes" $lit "` type."]
          pub const [<BYTES $lit>]: Self = Self::[<Bytes $lit>];
        )*

        /// Returns the size `N` of the `FixedBytes`.
        #[cfg_attr(not(tarpaulin), inline(always))]
        pub const fn size(&self) -> usize {
          match self {
            $(
              Self::[<Bytes $lit>] => $lit,
            )*
          }
        }

        /// Returns the string representation of the `FixedBytes` type.
        #[cfg_attr(not(tarpaulin), inline(always))]
        pub const fn as_str(&self) -> &'static str {
          match self {
            $(
              Self::[<Bytes $lit>] => concat!("bytes", $lit),
            )*
          }
        }
      }
    }
  };
}

define_fixed_bytes!(
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
  27, 28, 29, 30, 31, 32,
);

impl From<FixedBytes> for usize {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(fb: FixedBytes) -> Self {
    fb.size()
  }
}

impl core::fmt::Display for FixedBytes {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.as_str().fmt(f)
  }
}

impl AsRef<str> for FixedBytes {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl PartialEq<str> for FixedBytes {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn eq(&self, other: &str) -> bool {
    self.as_str().eq(other)
  }
}

impl PartialEq<FixedBytes> for str {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn eq(&self, other: &FixedBytes) -> bool {
    <FixedBytes as PartialEq<str>>::eq(other, self)
  }
}

impl PartialEq<&str> for FixedBytes {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn eq(&self, other: &&str) -> bool {
    <str as PartialEq<FixedBytes>>::eq(*other, self)
  }
}

impl PartialEq<FixedBytes> for &str {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn eq(&self, other: &FixedBytes) -> bool {
    <FixedBytes as PartialEq<&str>>::eq(other, self)
  }
}
