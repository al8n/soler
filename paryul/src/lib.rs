#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(docsrs, allow(unused_attributes))]
#![allow(clippy::double_parens)]
// #![deny(missing_docs)]

use logosky::utils::Span;

#[cfg(all(not(feature = "std"), feature = "alloc"))]
extern crate alloc as std;

#[cfg(feature = "std")]
extern crate std;

/// The AST for Yul.
pub mod ast;

/// The CST for Yul.
pub mod cst;

/// Error types for Yul parser.
pub mod error;

/// The syntax kinds for Yul
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, derive_more::IsVariant)]
pub enum SyntaxKind {
  // ========= Nodes =========
  /// Statement
  /// 
  /// Spec: [Yul Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulStatement)
  Statement,

  /// Block
  /// 
  /// Spec: [Yul Block](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBlock)
  Block,

  /// Variable Declaration
  ///
  /// Spec: [Yul variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
  VariableDeclaration,

  /// Assignment
  /// 
  /// Spec: [Yul Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
  Assignment,

  /// If statement
  ///
  /// Spec: [Yul If Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulIfStatement)
  IfStatement,

  /// For Statement
  ///
  /// Spec: [Yul For Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulForStatement)
  ForStatement,

  /// Switch Statement
  /// 
  /// Spec: [Yul Switch Statements](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulSwitchStatement)
  SwitchStatement,

  /// Function definition
  /// 
  /// Spec: [Yul Function Definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionDefinition)
  FunctionDefinition,

  /// Path segment
  PathSegment,

  /// Path
  /// 
  /// Spec: [Yul Path](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulPath)
  Path,

  /// Function call
  /// 
  /// Spec: [Yul Function Call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
  FunctionCall,

  /// Boolean literal
  ///
  /// Spec: [Yul boolean literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBoolean)
  LitBool,

  /// Decimal literal
  ///
  /// Spec: [Yul decimal literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulDecimalNumber)
  LitDecimal,

  /// Hexadecimal literal
  ///
  /// Spec: [Yul hexadecimal literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulHexNumber)
  LitHexadecimal,

  /// String literal
  ///
  /// Spec: [Yul string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulStringLiteral)
  LitString,

  /// Hex string literal
  ///
  /// Spec: [Yul hex string literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.HexString)
  LitHexString,

  /// Literal
  /// 
  /// Spec: [Yul Literal](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
  Literal,

  /// Expression
  /// 
  /// Spec: [Yul Expression](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulExpression)
  Expression,

  /// Identifier
  ///
  /// Spec: [Yul identifier](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulIdentifier)
  Identifier,

  // /// Token syntax kind
  // Token(TK),
}
 
/// An identifier.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Ident<S> {
  span: Span,
  ident: S,
}

impl<S> Ident<S> {
  /// Create a new identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, ident: S) -> Self {
    Self { span, ident }
  }

  /// Get the span of the identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the string of the identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn ident_ref(&self) -> &S {
    &self.ident
  }

  /// Get the string of the identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn ident(self) -> S
  where
    S: Copy,
  {
    self.ident
  }
}
