use derive_more::{IsVariant, Display};

/// The reserved keywords in Solidity.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Display)]
pub enum ReservedKeyword {
  /// The `after` keyword
  #[display("after")]
  After,
  /// The `alias` keyword
  #[display("alias")]
  Alias,
  /// The `apply` keyword
  #[display("apply")]
  Apply,
  /// The `auto` keyword
  #[display("auto")]
  Auto,
  /// The `byte` keyword
  #[display("byte")]
  Byte,
  /// The `case` keyword
  #[display("case")]
  Case,
  /// The `copyof` keyword
  #[display("copyof")]
  Copyof,
  /// The `default` keyword
  #[display("default")]
  Default,
  /// The `define` keyword
  #[display("define")]
  Define,
  /// The `final` keyword
  #[display("final")]
  Final,
  /// The `implements` keyword
  #[display("implements")]
  Implements,
  /// The `in` keyword
  #[display("in")]
  In,
  /// The `inline` keyword
  #[display("inline")]
  Inline,
  /// The `let` keyword
  #[display("let")]
  Let,
  /// The `macro` keyword
  #[display("macro")]
  Macro,
  /// The `match` keyword
  #[display("match")]
  Match,
  /// The `mutable` keyword
  #[display("mutable")]
  Mutable,
  /// The `null` keyword
  #[display("null")]
  Null,
  /// The `of` keyword
  #[display("of")]
  Of,
  /// The `partial` keyword
  #[display("partial")]
  Partial,
  /// The `promise` keyword
  #[display("promise")]
  Promise,
  /// The `reference` keyword
  #[display("reference")]
  Reference,
  /// The `relocatable` keyword
  #[display("relocatable")]
  Relocatable,
  /// The `sealed` keyword
  #[display("sealed")]
  Sealed,
  /// The `sizeof` keyword
  #[display("sizeof")]
  Sizeof,
  /// The `static` keyword
  #[display("static")]
  Static,
  /// The `supports` keyword
  #[display("supports")]
  Supports,
  /// The `switch` keyword
  #[display("switch")]
  Switch,
  /// The `typedef` keyword
  #[display("typedef")]
  Typedef,
  /// The `typeof` keyword
  #[display("typeof")]
  Typeof,
  /// The `var` keyword
  #[display("var")]
  Var,
}
