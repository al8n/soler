use super::{Denomination, FixedBytes, Int, Lit, Uint};

use derive_more::{IsVariant, TryUnwrap, Unwrap};

use token::token;

mod bytes;
mod str;
mod token;

/// The syntactic token for Solidity.
///
/// `S` is the backing string type used for identifiers, literals, and other
/// source fragments (e.g. `&'src str`, `String`, or an interned symbol).
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Token<S> {
  // ----- Keywords -----
  /// The `abstract` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Abstract,
  /// The `address` type keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Address,
  /// The `anonymous` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Anonymous,
  /// The `as` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  As,
  /// The `assembly` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Assembly,
  /// The `bool` type keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Bool,
  /// The `break` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Break,
  /// The `bytes` keyword (dynamic byte array type).
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Bytes,
  /// The `calldata` data location keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Calldata,
  /// The `catch` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Catch,
  /// The `constant` keyword (legacy state mutability / variable modifier).
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Constant,
  /// The `constructor` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Constructor,
  /// The `continue` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Continue,
  /// The `contract` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Contract,
  /// The `delete` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Delete,
  /// The `do` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Do,
  /// The `else` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Else,
  /// The `emit` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Emit,
  /// The `enum` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Enum,
  /// The `event` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Event,
  /// The `external` function visibility keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  External,
  /// The `fallback` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Fallback,
  /// The `for` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  For,
  /// The `function` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Function,
  /// The `if` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  If,
  /// The `immutable` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Immutable,
  /// The `import` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Import,
  /// The `indexed` event parameter modifier.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Indexed,
  /// The `interface` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Interface,
  /// The `internal` visibility keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Internal,
  /// The `is` inheritance / base-specifier keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Is,
  /// The `library` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Library,
  /// The `mapping` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Mapping,
  /// The `memory` data location keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Memory,
  /// The `modifier` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Modifier,
  /// The `new` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  New,
  /// The `override` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Override,
  /// The `payable` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Payable,
  /// The `private` visibility keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Private,
  /// The `public` visibility keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Public,
  /// The `pure` state mutability keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Pure,
  /// The `pragma` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Pragma,
  /// The `receive` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Receive,
  /// The `return` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Return,
  /// The `returns` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Returns,
  /// The `storage` data location keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Storage,
  /// The `string` type keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  String,
  /// The `struct` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Struct,
  /// The `try` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Try,
  /// The `type` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Type,
  /// The `unchecked` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Unchecked,
  /// The `using` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Using,
  /// The `view` state mutability keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  View,
  /// The `virtual` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Virtual,
  /// The `while` keyword.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  While,

  // ----- Delimiters & punctuation -----
  /// The `(` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  LParen,
  /// The `)` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  RParen,
  /// The `[` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  LBracket,
  /// The `]` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  RBracket,
  /// The `{` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  LBrace,
  /// The `}` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  RBrace,
  /// The `:` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Colon,
  /// The `;` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Semicolon,
  /// The `.` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Dot,
  /// The `?` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Question,
  /// The `=>` fat arrow.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  FatArrow,
  /// The `->` thin arrow.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  ThinArrow,
  /// The `=` assignment operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Assign,

  // ----- Compound assignment operators -----
  /// The `|=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  BitOrAssign,
  /// The `&=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  BitAndAssign,
  /// The `^=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  BitXorAssign,
  /// The `<<=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  ShlAssign,
  /// The `>>=` arithmetic right-shift assign operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  SarAssign,
  /// The `>>>=` logical right-shift assign operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  ShrAssign,
  /// The `+=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  AddAssign,
  /// The `-=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  SubAssign,
  /// The `*=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  MulAssign,
  /// The `/=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  DivAssign,
  /// The `%=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  ModAssign,

  /// The `,` symbol.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Comma,

  // ----- Logical & bitwise operators -----
  /// The `||` logical-or operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Or,
  /// The `&&` logical-and operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  And,
  /// The `|` bitwise-or operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  BitOr,
  /// The `&` bitwise-and operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  BitAnd,
  /// The `^` bitwise-xor operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  BitXor,
  /// The `<<` shift-left operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Shl,
  /// The `>>` arithmetic right-shift operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Sar,
  /// The `>>>` logical right-shift operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Shr,

  // ----- Arithmetic operators -----
  /// The `+` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Add,
  /// The `-` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Sub,
  /// The `*` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Mul,
  /// The `/` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Div,
  /// The `%` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Mod,
  /// The `**` exponentiation operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Exp,

  // ----- Comparison operators -----
  /// The `==` equality operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Eq,
  /// The `!=` inequality operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Ne,
  /// The `<` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Lt,
  /// The `<=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Le,
  /// The `>` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Gt,
  /// The `>=` operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Ge,

  // ----- Unary operators -----
  /// The `!` logical-negation operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Not,
  /// The `~` bitwise-negation operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  BitNot,
  /// The `++` increment operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Inc,
  /// The `--` decrement operator.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Dec,

  // ----- Type-like tokens with payloads -----
  /// A fixed-size `bytesN` type keyword (e.g. `bytes32`).
  FixedBytes(FixedBytes),

  /// A denomination literal suffix (e.g. `wei`, `gwei`, `ether`, time units).
  Denomination(Denomination),

  /// A signed integer type keyword (e.g. `int8`, `int256`).
  Int(Int),

  /// An unsigned integer type keyword (e.g. `uint8`, `uint256`).
  Uint(Uint),

  /// A fixed-point signed type keyword (e.g. `fixed128x18`), using the backing string.
  Fixed(S),

  /// A fixed-point unsigned type keyword (e.g. `ufixed128x18`), using the backing string.
  UFixed(S),

  // ----- Literals & identifiers -----
  /// A literal value: numeric, string, hex string, boolean, etc.
  Lit(Lit<S>),

  /// An identifier (user-defined name).
  Identifier(S),
}

/// The *structural* kind of a Solidity token.
///
/// Unlike [`Token`], this enum does not carry any payload (no lexeme text,
/// numeric value, etc.). It is useful for:
///
/// - building parsers that only need to branch on token categories,
/// - storing compact token streams,
/// - diagnostics where the exact lexeme is tracked separately.
///
/// See [`Token`] for the full syntactic token with payloads.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
#[non_exhaustive]
pub enum TokenKind {
  // ----- Keywords -----
  /// The `abstract` keyword.
  Abstract,
  /// The `address` type keyword.
  Address,
  /// The `anonymous` keyword.
  Anonymous,
  /// The `as` keyword.
  As,
  /// The `assembly` keyword.
  Assembly,
  /// The `bool` type keyword.
  Bool,
  /// The `break` keyword.
  Break,
  /// The `bytes` keyword (dynamic byte array type).
  Bytes,
  /// The `calldata` data location keyword.
  Calldata,
  /// The `catch` keyword.
  Catch,
  /// The `constant` keyword (legacy).
  Constant,
  /// The `constructor` keyword.
  Constructor,
  /// The `continue` keyword.
  Continue,
  /// The `contract` keyword.
  Contract,
  /// The `delete` keyword.
  Delete,
  /// The `do` keyword.
  Do,
  /// The `else` keyword.
  Else,
  /// The `emit` keyword.
  Emit,
  /// The `enum` keyword.
  Enum,
  /// The `event` keyword.
  Event,
  /// The `external` visibility keyword.
  External,
  /// The `fallback` keyword.
  Fallback,
  /// The `for` keyword.
  For,
  /// The `function` keyword.
  Function,
  /// The `if` keyword.
  If,
  /// The `immutable` keyword.
  Immutable,
  /// The `import` keyword.
  Import,
  /// The `indexed` event parameter modifier.
  Indexed,
  /// The `interface` keyword.
  Interface,
  /// The `internal` visibility keyword.
  Internal,
  /// The `is` inheritance keyword.
  Is,
  /// The `library` keyword.
  Library,
  /// The `mapping` keyword.
  Mapping,
  /// The `memory` data location keyword.
  Memory,
  /// The `modifier` keyword.
  Modifier,
  /// The `new` keyword.
  New,
  /// The `override` keyword.
  Override,
  /// The `payable` keyword.
  Payable,
  /// The `private` visibility keyword.
  Private,
  /// The `public` visibility keyword.
  Public,
  /// The `pure` state mutability keyword.
  Pure,
  /// The `pragma` keyword.
  Pragma,
  /// The `receive` keyword.
  Receive,
  /// The `return` keyword.
  Return,
  /// The `returns` keyword.
  Returns,
  /// The `storage` data location keyword.
  Storage,
  /// The `string` type keyword.
  String,
  /// The `struct` keyword.
  Struct,
  /// The `try` keyword.
  Try,
  /// The `type` keyword.
  Type,
  /// The `unchecked` keyword.
  Unchecked,
  /// The `using` keyword.
  Using,
  /// The `view` state mutability keyword.
  View,
  /// The `virtual` keyword.
  Virtual,
  /// The `while` keyword.
  While,

  // ----- Delimiters & punctuation -----
  /// The `(` symbol.
  LParen,
  /// The `)` symbol.
  RParen,
  /// The `[` symbol.
  LBracket,
  /// The `]` symbol.
  RBracket,
  /// The `{` symbol.
  LBrace,
  /// The `}` symbol.
  RBrace,
  /// The `:` symbol.
  Colon,
  /// The `;` symbol.
  Semicolon,
  /// The `.` symbol.
  Dot,
  /// The `?` symbol.
  Question,
  /// The `=>` fat arrow.
  FatArrow,
  /// The `->` thin arrow.
  ThinArrow,
  /// The `=` assignment operator.
  Assign,

  // ----- Compound assignment operators -----
  /// The `|=` operator.
  BitOrAssign,
  /// The `&=` operator.
  BitAndAssign,
  /// The `^=` operator.
  BitXorAssign,
  /// The `<<=` operator.
  ShlAssign,
  /// The `>>=` arithmetic right-shift assign operator.
  SarAssign,
  /// The `>>>=` logical right-shift assign operator.
  ShrAssign,
  /// The `+=` operator.
  AddAssign,
  /// The `-=` operator.
  SubAssign,
  /// The `*=` operator.
  MulAssign,
  /// The `/=` operator.
  DivAssign,
  /// The `%=` operator.
  ModAssign,

  /// The `,` symbol.
  Comma,

  // ----- Logical & bitwise operators -----
  /// The `||` logical-or operator.
  Or,
  /// The `&&` logical-and operator.
  And,
  /// The `|` bitwise-or operator.
  BitOr,
  /// The `&` bitwise-and operator.
  BitAnd,
  /// The `^` bitwise-xor operator.
  BitXor,
  /// The `<<` shift-left operator.
  Shl,
  /// The `>>` arithmetic right-shift operator.
  Sar,
  /// The `>>>` logical right-shift operator.
  Shr,

  // ----- Arithmetic operators -----
  /// The `+` operator.
  Add,
  /// The `-` operator.
  Sub,
  /// The `*` operator.
  Mul,
  /// The `/` operator.
  Div,
  /// The `%` operator.
  Mod,
  /// The `**` exponentiation operator.
  Exp,

  // ----- Comparison operators -----
  /// The `==` equality operator.
  Eq,
  /// The `!=` inequality operator.
  Ne,
  /// The `<` operator.
  Lt,
  /// The `<=` operator.
  Le,
  /// The `>` operator.
  Gt,
  /// The `>=` operator.
  Ge,

  // ----- Unary operators -----
  /// The `!` logical-negation operator.
  Not,
  /// The `~` bitwise-negation operator.
  BitNot,
  /// The `++` increment operator.
  Inc,
  /// The `--` decrement operator.
  Dec,

  // ----- Type-like & literal-ish kinds -----
  /// A fixed-size `bytesN` type keyword (e.g. `bytes32`).
  FixedBytes,
  /// A denomination suffix (e.g. `wei`, `gwei`, `ether`, time units).
  Denomination,
  /// A signed integer type keyword (e.g. `int8`, `int256`).
  Int,
  /// An unsigned integer type keyword (e.g. `uint8`, `uint256`).
  Uint,
  /// A signed fixed-point type keyword (e.g. `fixed128x18`).
  Fixed,
  /// An unsigned fixed-point type keyword (e.g. `ufixed128x18`).
  UFixed,

  // ----- Literals & identifiers -----
  /// A literal value (numeric, string, hex string, boolean, etc.).
  Lit,
  /// An identifier (user-defined name).
  Identifier,
}

impl<S> From<&Token<S>> for TokenKind {
  #[inline]
  fn from(token: &Token<S>) -> Self {
    token.kind()
  }
}

impl<S> From<Token<S>> for TokenKind {
  #[inline]
  fn from(token: Token<S>) -> Self {
    token.kind()
  }
}

impl<S> Token<S> {
  /// Return the payload-free [`TokenKind`] corresponding to this token.
  #[inline]
  pub const fn kind(&self) -> TokenKind {
    match self {
      Self::Abstract => TokenKind::Abstract,
      Self::Address => TokenKind::Address,
      Self::Anonymous => TokenKind::Anonymous,
      Self::As => TokenKind::As,
      Self::Assembly => TokenKind::Assembly,
      Self::Bool => TokenKind::Bool,
      Self::Break => TokenKind::Break,
      Self::Bytes => TokenKind::Bytes,
      Self::Calldata => TokenKind::Calldata,
      Self::Catch => TokenKind::Catch,
      Self::Constant => TokenKind::Constant,
      Self::Constructor => TokenKind::Constructor,
      Self::Continue => TokenKind::Continue,
      Self::Contract => TokenKind::Contract,
      Self::Delete => TokenKind::Delete,
      Self::Do => TokenKind::Do,
      Self::Else => TokenKind::Else,
      Self::Emit => TokenKind::Emit,
      Self::Enum => TokenKind::Enum,
      Self::Event => TokenKind::Event,
      Self::External => TokenKind::External,
      Self::Fallback => TokenKind::Fallback,
      Self::For => TokenKind::For,
      Self::Function => TokenKind::Function,
      Self::If => TokenKind::If,
      Self::Immutable => TokenKind::Immutable,
      Self::Import => TokenKind::Import,
      Self::Indexed => TokenKind::Indexed,
      Self::Interface => TokenKind::Interface,
      Self::Internal => TokenKind::Internal,
      Self::Is => TokenKind::Is,
      Self::Library => TokenKind::Library,
      Self::Mapping => TokenKind::Mapping,
      Self::Memory => TokenKind::Memory,
      Self::Modifier => TokenKind::Modifier,
      Self::New => TokenKind::New,
      Self::Override => TokenKind::Override,
      Self::Payable => TokenKind::Payable,
      Self::Private => TokenKind::Private,
      Self::Public => TokenKind::Public,
      Self::Pure => TokenKind::Pure,
      Self::Pragma => TokenKind::Pragma,
      Self::Receive => TokenKind::Receive,
      Self::Return => TokenKind::Return,
      Self::Returns => TokenKind::Returns,
      Self::Storage => TokenKind::Storage,
      Self::String => TokenKind::String,
      Self::Struct => TokenKind::Struct,
      Self::Try => TokenKind::Try,
      Self::Type => TokenKind::Type,
      Self::Unchecked => TokenKind::Unchecked,
      Self::Using => TokenKind::Using,
      Self::View => TokenKind::View,
      Self::Virtual => TokenKind::Virtual,
      Self::While => TokenKind::While,

      Self::LParen => TokenKind::LParen,
      Self::RParen => TokenKind::RParen,
      Self::LBracket => TokenKind::LBracket,
      Self::RBracket => TokenKind::RBracket,
      Self::LBrace => TokenKind::LBrace,
      Self::RBrace => TokenKind::RBrace,
      Self::Colon => TokenKind::Colon,
      Self::Semicolon => TokenKind::Semicolon,
      Self::Dot => TokenKind::Dot,
      Self::Question => TokenKind::Question,
      Self::FatArrow => TokenKind::FatArrow,
      Self::ThinArrow => TokenKind::ThinArrow,
      Self::Assign => TokenKind::Assign,

      Self::BitOrAssign => TokenKind::BitOrAssign,
      Self::BitAndAssign => TokenKind::BitAndAssign,
      Self::BitXorAssign => TokenKind::BitXorAssign,
      Self::ShlAssign => TokenKind::ShlAssign,
      Self::SarAssign => TokenKind::SarAssign,
      Self::ShrAssign => TokenKind::ShrAssign,
      Self::AddAssign => TokenKind::AddAssign,
      Self::SubAssign => TokenKind::SubAssign,
      Self::MulAssign => TokenKind::MulAssign,
      Self::DivAssign => TokenKind::DivAssign,
      Self::ModAssign => TokenKind::ModAssign,

      Self::Comma => TokenKind::Comma,

      Self::Or => TokenKind::Or,
      Self::And => TokenKind::And,
      Self::BitOr => TokenKind::BitOr,
      Self::BitAnd => TokenKind::BitAnd,
      Self::BitXor => TokenKind::BitXor,
      Self::Shl => TokenKind::Shl,
      Self::Sar => TokenKind::Sar,
      Self::Shr => TokenKind::Shr,

      Self::Add => TokenKind::Add,
      Self::Sub => TokenKind::Sub,
      Self::Mul => TokenKind::Mul,
      Self::Div => TokenKind::Div,
      Self::Mod => TokenKind::Mod,
      Self::Exp => TokenKind::Exp,

      Self::Eq => TokenKind::Eq,
      Self::Ne => TokenKind::Ne,
      Self::Lt => TokenKind::Lt,
      Self::Le => TokenKind::Le,
      Self::Gt => TokenKind::Gt,
      Self::Ge => TokenKind::Ge,

      Self::Not => TokenKind::Not,
      Self::BitNot => TokenKind::BitNot,
      Self::Inc => TokenKind::Inc,
      Self::Dec => TokenKind::Dec,

      Self::FixedBytes(_) => TokenKind::FixedBytes,
      Self::Denomination(_) => TokenKind::Denomination,
      Self::Int(_) => TokenKind::Int,
      Self::Uint(_) => TokenKind::Uint,
      Self::Fixed(_) => TokenKind::Fixed,
      Self::UFixed(_) => TokenKind::UFixed,
      Self::Lit(_) => TokenKind::Lit,
      Self::Identifier(_) => TokenKind::Identifier,
    }
  }
}
