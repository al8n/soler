/// Unit denomination for numbers.
///
/// Spec: [denomination](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.Denomination)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Denomination {
  /// Wei
  Wei,
  /// Gwei
  Gwei,
  /// Ether
  Ether,
  /// Seconds
  Seconds,
  /// Minutes
  Minutes,
  /// Hours
  Hours,
  /// Days
  Days,
  /// Weeks
  Weeks,
  /// Years
  Years,
}

impl AsRef<str> for Denomination {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}

impl core::fmt::Display for Denomination {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.as_str().fmt(f)
  }
}

impl Denomination {
  /// Returns the string representation of the `Denomination`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Wei => "wei",
      Self::Gwei => "gwei",
      Self::Ether => "ether",
      Self::Seconds => "seconds",
      Self::Minutes => "minutes",
      Self::Hours => "hours",
      Self::Days => "days",
      Self::Weeks => "weeks",
      Self::Years => "years",
    }
  }
}
