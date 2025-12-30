use core::marker::PhantomData;
use derive_more::Display;
use tokit::{Lexer, logos};

pub use ty::*;

use crate::sealed::TokenBridge;

/// The lossless lexer for Solidity
pub mod lossless;
/// The syntactic lexer for Solidity
pub mod syntactic;

mod handlers;
mod ty;

/// The lexer for Solidity
pub struct Lexsol<'inp, S, T: TokenBridge<'inp>> {
  input: &'inp S,
  inner: logos::Lexer<'inp, T::Logos>,
}

impl<'inp, S, T> Clone for Lexsol<'inp, S, T>
where
  T: TokenBridge<'inp>,
  logos::Lexer<'inp, T::Logos>: Clone,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn clone(&self) -> Self {
    Self {
      input: self.input,
      inner: self.inner.clone(),
    }
  }
}

impl<'inp, S, T> Iterator for Lexsol<'inp, S, T>
where
  Self: tokit::Lexer<'inp, Token = T>,
  T: TokenBridge<'inp>,
{
  type Item = Result<T, <T as tokit::Token<'inp>>::Error>;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn next(&mut self) -> Option<Self::Item> {
    self.lex()
  }
}

impl<'inp, S, T> Iterator for &mut Lexsol<'inp, S, T>
where
  Self: tokit::Lexer<'inp, Token = T>,
  T: TokenBridge<'inp>,
{
  type Item = Result<T, <T as tokit::Token<'inp>>::Error>;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn next(&mut self) -> Option<Self::Item> {
    self.lex()
  }
}

/// The Solidity language
#[derive(Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display("solidity")]
pub struct Solidity<Kind>(PhantomData<Kind>);

impl<Kind> Default for Solidity<Kind> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::new()
  }
}

impl<Kind> Solidity<Kind> {
  /// Creates a new Solidity language instance
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new() -> Self {
    Self(PhantomData)
  }
}

impl<Kind> core::fmt::Debug for Solidity<Kind> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "Solidity")
  }
}

macro_rules! syntax_kind {
  (
    $(#[$meta:meta])*
    enum $kind:ident {
      $(
        $(#[$variant_meta:meta])*
        $name:ident,
      )*
    }
  ) => {
    paste::paste! {
      $(#[$meta])*
      #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::IsVariant)]
      #[non_exhaustive]
      #[repr(u16)]
      #[allow(non_camel_case_types)]
      pub enum $kind {
        $(
          $(#[$variant_meta])*
          $name,
        )*

        // ========= Punctuation =========
        /// Yul left brace '{'
        LBrace,
        /// Yul right brace '}'
        RBrace,
        /// Yul left parenthesis '('
        LParen,
        /// Yul right parenthesis ')'
        RParen,
        /// Yul comma ','
        Comma,
        /// Yul dot '.'
        Dot,

        // ========= Operators =========
        /// Yul assignment operator ':='
        ColonAssign,
        /// Yul thin arrow '->'
        ThinArrow,

        // ========= Keywords =========
        /// Yul keyword 'function'
        function_KW,
        /// Yul keyword 'switch'
        switch_KW,
        /// Yul keyword 'case'
        case_KW,
        /// Yul keyword 'default'
        default_KW,
        /// Yul keyword 'for'
        for_KW,
        /// Yul keyword 'if'
        if_KW,
        /// Yul keyword 'let'
        let_KW,
        /// Yul keyword 'break'
        break_KW,
        /// Yul keyword 'continue'
        continue_KW,
        /// Yul keyword 'leave'
        leave_KW,

        /// Yul switch case keyword, either 'case' or 'default'
        SwitchCaseKeyword,

        // ========= Nodes =========
        /// Statement
        ///
        /// Spec: [Yul Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulStatement)
        Statement,

        /// Block
        ///
        /// Spec: [Yul Block](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBlock)
        Block,

        /// Single variable declaration
        ///
        /// Spec: [Yul Single Variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
        SingleVariableDeclaration,

        /// Multiple variables declaration
        ///
        /// Spec: [Yul Multiple Variables Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
        MultipleVariablesDeclaration,

        /// Variable Declaration
        ///
        /// Spec: [Yul variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
        VariableDeclaration,

        /// Single target assignment
        ///
        /// Spec: [Yul Single Target Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
        SingleTargetAssignment,

        /// Multiple targets assignment
        ///
        /// Spec: [Yul Multiple Target Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
        MultipleTargetAssignment,

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

        /// The name of a function call
        ///
        /// Spec: [Yul Function Call Name](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
        FunctionName,

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
        Lit,

        /// Expression
        ///
        /// Spec: [Yul Expression](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulExpression)
        Expression,

        /// Identifier
        ///
        /// Spec: [Yul identifier](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulIdentifier)
        Identifier,
      }
    }
  };
}

use syntax_kind;
