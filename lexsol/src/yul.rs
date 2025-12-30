use core::marker::PhantomData;

use derive_more::Display;
use tokit::{Lexer, logos};

use super::TokenBridge;

pub use types::*;

/// The lossless lexer for Yul
pub mod lossless;
/// The syntactic lexer for Yul
pub mod syntactic;

mod types;

mod handlers;

/// The Yul language
#[derive(Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display("yul")]
pub struct Yul<Kind>(PhantomData<Kind>);

impl<Kind> Default for Yul<Kind> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::new()
  }
}

impl<Kind> Yul<Kind> {
  /// Creates a new Yul language instance
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new() -> Self {
    Self(PhantomData)
  }
}

impl<Kind> core::fmt::Debug for Yul<Kind> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "Yul")
  }
}

/// The lexer for Yul
pub struct Lexyul<'inp, S, T: TokenBridge<'inp>> {
  input: &'inp S,
  inner: logos::Lexer<'inp, T::Logos>,
}

impl<'inp, S, T> Clone for Lexyul<'inp, S, T>
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

impl<'inp, S, T> Iterator for Lexyul<'inp, S, T>
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

impl<'inp, S, T> Iterator for &mut Lexyul<'inp, S, T>
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

macro_rules! syntax_kind {
  (
    $(#[$meta:meta])*
    enum $kind:ident {
      $(@evm: [
        $($evm:literal),+$(,)?
      ])?
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
        MultipleTargetsAssignment,

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

        /// EVM builtin function name
        ///
        /// Spec: [Yul EVM Builtin Function](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEvmBuiltin)
        #[cfg(feature = "evm")]
        #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
        #[doc(hidden)]
        #[is_variant(ignore)]
        __EvmBuiltinFunction = 65000,

        $(
          $(
            #[doc = "EVM builtin function `" $evm "`"]
            ///
            /// Spec: [Yul EVM Builtin Function](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEvmBuiltin)
            #[cfg(feature = "evm")]
            #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
            [< $evm:snake _EVM >],
          )*
        )?
      }

      impl $kind {
        /// Returns the syntax kinds of all EVM builtin functions
        #[cfg(feature = "evm")]
        #[cfg_attr(not(tarpaulin), inline(always))]
        #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
        pub const fn evm_builtin_fns() -> &'static [$kind] {
          &[
            $(
              $(
                $kind::[< $evm:snake _EVM >],
              )*
            )?
          ]
        }

        /// Returns `true` if the syntax kind is an EVM builtin function
        #[cfg(feature = "evm")]
        #[cfg_attr(not(tarpaulin), inline(always))]
        #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
        pub const fn is_evm_builtin_fn(&self) -> bool {
          match self {
            $(
              $(
                Self::[< $evm:snake _EVM >] => true,
              )*
            )?
            _ => false,
          }
        }
      }
    }
  };
}

use syntax_kind;
