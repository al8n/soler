use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{
  IdentifierToken, KeywordToken, LitToken, OperatorToken, PunctuatorToken, Require,
  Token as TokenT, TriviaToken,
  utils::{cmp::Equivalent, recursion_tracker::RecursionLimitExceeded},
};

use token::token;

use super::Lit;

use crate::{
  error::yul as error,
  types::{LitBool, LitNumber},
};

mod bytes;
mod str;
mod token;

/// The syntactic lexer for Yul.
pub type Lexer<'a, S = &'a str> = logosky::Tokenizer<'a, Token<S>>;

/// The char type used for the syntactic token.
pub type TokenChar<'a, S> = <Token<S> as TokenT<'a>>::Char;
/// The error type for lexing based on syntactic [`Token`].
pub type Error<'a, S> = error::Error<<Token<S> as TokenT<'a>>::Char, RecursionLimitExceeded>;
/// A collection of errors for syntactic [`Token`].
pub type Errors<'a, S> = error::Errors<<Token<S> as TokenT<'a>>::Char, RecursionLimitExceeded>;

/// The syntactic token of Yul
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Token<S> {
  /// ":="
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  ColonAssign,
  /// "->"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  ThinArrow,
  /// "{"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  LBrace,
  /// "}"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  RBrace,
  /// "("
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  LParen,
  /// ")"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  RParen,
  /// "."
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Dot,
  /// ","
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Comma,
  /// "leave"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Leave,
  /// "continue"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Continue,
  /// "break"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Break,
  /// "switch"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Switch,
  /// "case"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Case,
  /// "default"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Default,
  /// "function"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Function,
  /// "let"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Let,
  /// "if"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  If,
  /// "for"
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  For,

  /// Yul identifier
  ///
  /// Spec: [Yul identifier](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulIdentifier)
  Identifier(S),
  /// Yul literal
  ///
  /// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
  Lit(Lit<S>),

  /// Yul EVM built-in function
  ///
  /// Spec: [Yul evm built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltin(super::EvmBuiltinFunction),
}

/// The kind of Yul syntactic token.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
#[non_exhaustive]
pub enum TokenKind {
  /// ":="
  ColonAssign,
  /// "->"
  ThinArrow,
  /// "{"
  LBrace,
  /// "}"
  RBrace,
  /// "("
  LParen,
  /// ")"
  RParen,
  /// "."
  Dot,
  /// ","
  Comma,
  /// "leave"
  Leave,
  /// "continue"
  Continue,
  /// "break"
  Break,
  /// "switch"
  Switch,
  /// "case"
  Case,
  /// "default"
  Default,
  /// "function"
  Function,
  /// "let"
  Let,
  /// "if"
  If,
  /// "for"
  For,
  /// Yul identifier
  ///
  /// Spec: [Yul identifier](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulIdentifier)
  Identifier,
  /// Yul literal
  ///
  /// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
  Lit,
  /// Yul EVM built-in function
  ///
  /// Spec: [Yul evm built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltin,
}

impl<S> Token<S> {
  /// Get the kind of the syntactic token
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> TokenKind {
    match self {
      Self::ColonAssign => TokenKind::ColonAssign,
      Self::ThinArrow => TokenKind::ThinArrow,
      Self::LBrace => TokenKind::LBrace,
      Self::RBrace => TokenKind::RBrace,
      Self::LParen => TokenKind::LParen,
      Self::RParen => TokenKind::RParen,
      Self::Dot => TokenKind::Dot,
      Self::Comma => TokenKind::Comma,
      Self::Leave => TokenKind::Leave,
      Self::Continue => TokenKind::Continue,
      Self::Break => TokenKind::Break,
      Self::Switch => TokenKind::Switch,
      Self::Case => TokenKind::Case,
      Self::Default => TokenKind::Default,
      Self::Function => TokenKind::Function,
      Self::Let => TokenKind::Let,
      Self::If => TokenKind::If,
      Self::For => TokenKind::For,
      Self::Identifier(_) => TokenKind::Identifier,
      Self::Lit(_) => TokenKind::Lit,
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(_) => TokenKind::EvmBuiltin,
    }
  }

  /// Returns `true` if the token may be a YUL statement start token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_statement_start(&self) -> bool {
    match self {
      Self::Leave
      | Self::Continue
      | Self::Break
      | Self::LBrace
      | Self::Switch
      | Self::Function
      | Self::Let
      | Self::If
      | Self::For
      | Self::Identifier(_) => true,
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(_) => true,
      _ => false,
    }
  }
}

impl<'a, S: 'a> TriviaToken<'a> for Token<S>
where
  Token<S>: logosky::Token<'a>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_trivia(&self) -> bool {
    false
  }
}

impl<'a, S: 'a> PunctuatorToken<'a> for Token<S>
where
  Token<S>: logosky::Token<'a>,
  str: Equivalent<S>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_brace_open(&self) -> bool {
    matches!(self, Self::LBrace)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_brace_close(&self) -> bool {
    matches!(self, Self::RBrace)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_paren_open(&self) -> bool {
    matches!(self, Self::LParen)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_paren_close(&self) -> bool {
    matches!(self, Self::RParen)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_dot(&self) -> bool {
    matches!(self, Self::Dot)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_comma(&self) -> bool {
    matches!(self, Self::Comma)
  }
}

impl<'a, S: 'a> OperatorToken<'a> for Token<S>
where
  Token<S>: logosky::Token<'a>,
  str: Equivalent<S>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_colon_eq_assign(&self) -> bool {
    matches!(self, Self::ColonAssign)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_arrow(&self) -> bool {
    matches!(self, Self::ThinArrow)
  }
}

impl<'a, S: 'a> LitToken<'a> for Token<S>
where
  Token<S>: logosky::Token<'a>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_literal(&self) -> bool {
    matches!(self, Self::Lit(_))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_decimal_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Number(LitNumber::Decimal(_))))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_hexadecimal_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Number(LitNumber::Hexadecimal(_))))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_boolean_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Boolean(_)))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_true_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Boolean(LitBool::True(_))))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_false_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::Boolean(LitBool::False(_))))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_string_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::String(_)))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_inline_string_literal(&self) -> bool {
    matches!(self, Self::Lit(Lit::String(_)))
  }
}

impl<'a, S: 'a> IdentifierToken<'a> for Token<S>
where
  Token<S>: logosky::Token<'a>,
  <<Token<S> as logosky::Token<'a>>::Logos as logosky::Logos<'a>>::Source:
    logosky::Source<Slice<'a> = S>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_identifier(&self) -> bool {
    matches!(self, Self::Identifier(_))
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn identifier(
    &self,
  ) -> Option<&<<Self::Logos as logosky::Logos<'a>>::Source as logosky::Source>::Slice<'a>> {
    match self {
      Self::Identifier(s) => Some(s),
      _ => None,
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn try_into_identifier(
    self,
  ) -> Result<<<Self::Logos as logosky::Logos<'a>>::Source as logosky::Source>::Slice<'a>, Self>
  where
    Self: Sized,
  {
    self.try_unwrap_identifier().map_err(|e| e.input)
  }
}

impl<'a, S: 'a> KeywordToken<'a> for Token<S>
where
  Token<S>: logosky::Token<'a>,
  str: Equivalent<S>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn keyword(&self) -> Option<&'static str> {
    match self {
      Self::Leave => Some("leave"),
      Self::Continue => Some("continue"),
      Self::Break => Some("break"),
      Self::Switch => Some("switch"),
      Self::Case => Some("case"),
      Self::Default => Some("default"),
      Self::Function => Some("function"),
      Self::Let => Some("let"),
      Self::If => Some("if"),
      Self::For => Some("for"),
      _ => None,
    }
  }
}

impl<S> Equivalent<Token<S>> for str
where
  str: Equivalent<S>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn equivalent(&self, other: &Token<S>) -> bool {
    match other {
      Token::Leave => self.eq("leave"),
      Token::Continue => self.eq("continue"),
      Token::Break => self.eq("break"),
      Token::Switch => self.eq("switch"),
      Token::Case => self.eq("case"),
      Token::Default => self.eq("default"),
      Token::Function => self.eq("function"),
      Token::Let => self.eq("let"),
      Token::If => self.eq("if"),
      Token::For => self.eq("for"),
      Token::LBrace => self.eq("{"),
      Token::RBrace => self.eq("}"),
      Token::LParen => self.eq("("),
      Token::RParen => self.eq(")"),
      Token::Dot => self.eq("."),
      Token::Comma => self.eq(","),
      Token::ColonAssign => self.eq(":="),
      Token::ThinArrow => self.eq("->"),
      Token::Identifier(s) => self.equivalent(s),
      #[cfg(feature = "evm")]
      Token::EvmBuiltin(b) => self.eq(b.as_str()),
      _ => false,
    }
  }
}

#[cfg(feature = "evm")]
#[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
impl<S> Require<super::EvmBuiltinFunction> for Token<S> {
  type Err = Self;

  fn require(self) -> Result<super::EvmBuiltinFunction, Self::Err>
  where
    Self: Sized,
  {
    self.try_unwrap_evm_builtin().map_err(|e| e.input)
  }
}
