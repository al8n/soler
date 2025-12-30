use super::{Denomination, FixedBytes, Int, Lexsol, Lit, Solidity, Uint};

use crate::{SourceBridge, TokenBridge, error::sol as error};

use derive_more::{Display, IsVariant, TryUnwrap, Unwrap};
use token::token;
use tokit::{
  Source, State, Token as TokenT,
  logos::Logos,
  utils::{SimpleSpan, tracker::LimitExceeded},
};

mod bytes;
mod str;
mod token;

/// The syntactic lexer for Solidity.
pub type Lexer<'a, S = &'a str> = Lexsol<'a, S, Token<<S as Source<usize>>::Slice<'a>>>;

/// The char type used for the syntactic token.
pub type Char<'a, S> = <<<Lexer<'a, S> as tokit::Lexer<'a>>::Source as Source<usize>>::Slice<
  'a,
> as tokit::lexer::source::Slice<'a>>::Char;
/// The error type for lexing based on syntactic [`Token`].
pub type Error<'a, S> = error::Error<Char<'a, S>, LimitExceeded>;
/// A collection of errors for syntactic [`Token`].
pub type Errors<'a, S> = error::Errors<Char<'a, S>, LimitExceeded>;

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
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
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
  FixedBytes(FixedBytes),
  /// A denomination suffix (e.g. `wei`, `gwei`, `ether`, time units).
  Denomination(Denomination),
  /// A signed integer type keyword (e.g. `int8`, `int256`).
  Int(Int),
  /// An unsigned integer type keyword (e.g. `uint8`, `uint256`).
  Uint(Uint),
  /// A signed fixed-point type keyword (e.g. `fixed128x18`).
  Fixed,
  /// An unsigned fixed-point type keyword (e.g. `ufixed128x18`).
  UFixed,

  // ----- Literals & identifiers -----
  /// A literal value (numeric, string, hex string, boolean, etc.).
  Lit(Lit),
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

      Self::FixedBytes(v) => TokenKind::FixedBytes(*v),
      Self::Denomination(v) => TokenKind::Denomination(*v),
      Self::Int(v) => TokenKind::Int(*v),
      Self::Uint(v) => TokenKind::Uint(*v),
      Self::Fixed(_) => TokenKind::Fixed,
      Self::UFixed(_) => TokenKind::UFixed,
      Self::Lit(v) => TokenKind::Lit(v.unit()),
      Self::Identifier(_) => TokenKind::Identifier,
    }
  }
}

super::syntax_kind!(
  /// The syntax kind for Solidity.
  enum SyntaxKind {}
);

impl<'inp, S> tokit::Lexer<'inp> for Lexsol<'inp, S, Token<S::Slice<'inp>>>
where
  Token<S::Slice<'inp>>: TokenBridge<'inp, Kind = TokenKind>,
  <Token<S::Slice<'inp>> as TokenT<'inp>>::Error: From<<<Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos as Logos<'inp>>::Error>
    + From<<<<Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos as Logos<'inp>>::Extras as State>::Error>,
  <Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos: Logos<'inp, Source = <S as SourceBridge<'inp>>::Logos>,
  <<Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos as Logos<'inp>>::Extras: State,
  S: SourceBridge<'inp>,
{
  type State = <<Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos as Logos<'inp>>::Extras;
  type Source = S;
  type Token = Token<S::Slice<'inp>>;
  type Span = SimpleSpan;
  type Offset = usize;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn new(input: &'inp Self::Source) -> Self
  where
    Self::State: Default
  {
    let inner = tokit::logos::Lexer::new(input.to_logos_source());
    Self { input, inner }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn with_state(input: &'inp Self::Source, state: Self::State) -> Self {
    let inner = tokit::logos::Lexer::with_extras(input.to_logos_source(), state);
    Self { input, inner }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn check(&self) -> Result<(), <Self::Token as tokit::Token<'inp>>::Error> {
    self.inner.extras.check().map_err(Into::into)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn state(&self) -> &Self::State {
    &self.inner.extras
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn state_mut(&mut self) -> &mut Self::State {
    &mut self.inner.extras
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn into_state(self) -> Self::State {
    self.inner.extras
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn source(&self) -> &'inp Self::Source {
    self.input
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn span(&self) -> Self::Span {
    self.inner.span().into()
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn slice(&self) -> <Self::Source as tokit::Source<Self::Offset>>::Slice<'inp> {
    let span = self.inner.span();
    self
      .input
      .slice(&span.start..&span.end)
      .expect("slice of the current lexer span should not be None")
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn lex(&mut self) -> Option<Result<Self::Token, <Self::Token as tokit::Token<'inp>>::Error>> {
    match self.inner.next() {
      Some(Ok(tok)) => match self.check() {
        Ok(_) => Some(Ok(
          match <Token<S::Slice<'inp>> as TokenBridge<'inp>>::kind(&tok) {
            TokenKind::Abstract => Token::Abstract,
            TokenKind::Address => Token::Address,
            TokenKind::Anonymous => Token::Anonymous,
            TokenKind::As => Token::As,
            TokenKind::Assembly => Token::Assembly,
            TokenKind::Bool => Token::Bool,
            TokenKind::Break => Token::Break,
            TokenKind::Bytes => Token::Bytes,
            TokenKind::Calldata => Token::Calldata,
            TokenKind::Catch => Token::Catch,
            TokenKind::Constant => Token::Constant,
            TokenKind::Constructor => Token::Constructor,
            TokenKind::Continue => Token::Continue,
            TokenKind::Contract => Token::Contract,
            TokenKind::Delete => Token::Delete,
            TokenKind::Do => Token::Do,
            TokenKind::Else => Token::Else,
            TokenKind::Emit => Token::Emit,
            TokenKind::Enum => Token::Enum,
            TokenKind::Event => Token::Event,
            TokenKind::External => Token::External,
            TokenKind::Fallback => Token::Fallback,
            TokenKind::For => Token::For,
            TokenKind::Function => Token::Function,
            TokenKind::If => Token::If,
            TokenKind::Immutable => Token::Immutable,
            TokenKind::Import => Token::Import,
            TokenKind::Indexed => Token::Indexed,
            TokenKind::Interface => Token::Interface,
            TokenKind::Internal => Token::Internal,
            TokenKind::Is => Token::Is,
            TokenKind::Library => Token::Library,
            TokenKind::Mapping => Token::Mapping,
            TokenKind::Memory => Token::Memory,
            TokenKind::Modifier => Token::Modifier,
            TokenKind::New => Token::New,
            TokenKind::Override => Token::Override,
            TokenKind::Payable => Token::Payable,
            TokenKind::Private => Token::Private,
            TokenKind::Public => Token::Public,
            TokenKind::Pure => Token::Pure,
            TokenKind::Pragma => Token::Pragma,
            TokenKind::Receive => Token::Receive,
            TokenKind::Return => Token::Return,
            TokenKind::Returns => Token::Returns,
            TokenKind::Storage => Token::Storage,
            TokenKind::String => Token::String,
            TokenKind::Struct => Token::Struct,
            TokenKind::Try => Token::Try,
            TokenKind::Type => Token::Type,
            TokenKind::Unchecked => Token::Unchecked,
            TokenKind::Using => Token::Using,
            TokenKind::View => Token::View,
            TokenKind::Virtual => Token::Virtual,
            TokenKind::While => Token::While,
            TokenKind::LParen => Token::LParen,
            TokenKind::RParen => Token::RParen,
            TokenKind::LBracket => Token::LBracket,
            TokenKind::RBracket => Token::RBracket,
            TokenKind::LBrace => Token::LBrace,
            TokenKind::RBrace => Token::RBrace,
            TokenKind::Colon => Token::Colon,
            TokenKind::Semicolon => Token::Semicolon,
            TokenKind::Dot => Token::Dot,
            TokenKind::Question => Token::Question,
            TokenKind::FatArrow => Token::FatArrow,
            TokenKind::ThinArrow => Token::ThinArrow,
            TokenKind::Assign => Token::Assign,
            TokenKind::BitOrAssign => Token::BitOrAssign,
            TokenKind::BitAndAssign => Token::BitAndAssign,
            TokenKind::BitXorAssign => Token::BitXorAssign,
            TokenKind::ShlAssign => Token::ShlAssign,
            TokenKind::SarAssign => Token::SarAssign,
            TokenKind::ShrAssign => Token::ShrAssign,
            TokenKind::AddAssign => Token::AddAssign,
            TokenKind::SubAssign => Token::SubAssign,
            TokenKind::MulAssign => Token::MulAssign,
            TokenKind::DivAssign => Token::DivAssign,
            TokenKind::ModAssign => Token::ModAssign,
            TokenKind::Comma => Token::Comma,
            TokenKind::Or => Token::Or,
            TokenKind::And => Token::And,
            TokenKind::BitOr => Token::BitOr,
            TokenKind::BitAnd => Token::BitAnd,
            TokenKind::BitXor => Token::BitXor,
            TokenKind::Shl => Token::Shl,
            TokenKind::Sar => Token::Sar,
            TokenKind::Shr => Token::Shr,
            TokenKind::Add => Token::Add,
            TokenKind::Sub => Token::Sub,
            TokenKind::Mul => Token::Mul,
            TokenKind::Div => Token::Div,
            TokenKind::Mod => Token::Mod,
            TokenKind::Exp => Token::Exp,
            TokenKind::Eq => Token::Eq,
            TokenKind::Ne => Token::Ne,
            TokenKind::Lt => Token::Lt,
            TokenKind::Le => Token::Le,
            TokenKind::Gt => Token::Gt,
            TokenKind::Ge => Token::Ge,
            TokenKind::Not => Token::Not,
            TokenKind::BitNot => Token::BitNot,
            TokenKind::Inc => Token::Inc,
            TokenKind::Dec => Token::Dec,
            TokenKind::FixedBytes(fixed_bytes) => Token::FixedBytes(fixed_bytes),
            TokenKind::Denomination(denomination) => Token::Denomination(denomination),
            TokenKind::Int(int) => Token::Int(int),
            TokenKind::Uint(uint) => Token::Uint(uint),
            TokenKind::Fixed => Token::Fixed(self.slice()),
            TokenKind::UFixed => Token::UFixed(self.slice()),
            TokenKind::Lit(lit) => Token::Lit(lit.map(|_| self.slice())),
            TokenKind::Identifier => Token::Identifier(self.slice()),
          }
        )),
        Err(e) => Some(Err(e)),
      },
      Some(Err(err)) => Some(Err(err.into())),
      None => None,
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn bump(&mut self, n: &Self::Offset) {
    self.inner.bump(*n);
  }
}

impl super::super::sealed::Sealed for Solidity<SyntaxKind> {
  const INIT: Self = Solidity::new();
  const NAME: &'static str = "solidity";
  const DECIMAL_NUMBER_PATTERN: &'static str = r"0|[1-9](_?[0-9_])*";
  const HEX_NUMBER_PATTERN: &'static str = r"0x[0-9a-fA-F_]+";
}

#[cfg(not(feature = "rowan"))]
impl tokit::syntax::Language for Solidity<SyntaxKind> {
  type SyntaxKind = SyntaxKind;
}

#[cfg(feature = "rowan")]
#[cfg_attr(docsrs, doc(cfg(feature = "rowan")))]
const _: () = {
  use rowan::{Language, SyntaxKind as RowanSyntaxKind};

  impl Language for super::Solidity<SyntaxKind> {
    type Kind = SyntaxKind;

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn kind_from_raw(raw: RowanSyntaxKind) -> Self::Kind {
      unsafe { core::mem::transmute::<u16, Self::Kind>(raw.0) }
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn kind_to_raw(kind: Self::Kind) -> RowanSyntaxKind {
      RowanSyntaxKind(kind as u16)
    }
  }
};
