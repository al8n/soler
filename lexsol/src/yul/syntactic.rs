use derive_more::{Display, IsVariant, TryUnwrap, Unwrap};

#[cfg(feature = "evm")]
use tokit::Require;
use tokit::{
  SimpleSpan, Source, State, Token as TokenT,
  logos::Logos,
  state::recursion_tracker::RecursionLimitExceeded,
  token::{IdentifierToken, KeywordToken, LitToken, OperatorToken, PunctuatorToken},
  utils::cmp::Equivalent,
};

use token::token;

use super::{Lexyul, Lit};

use crate::{
  SourceBridge, TokenBridge,
  error::yul as error,
  types::{LitBool, LitNumber},
};

mod bytes;
mod str;
mod token;

/// The syntactic lexer for Yul.
pub type Lexer<'a, S = &'a str> = Lexyul<'a, S, Token<<S as Source<usize>>::Slice<'a>>>;

/// The char type used for the syntactic token.
pub type Char<'a, S> = <<<Lexer<'a, S> as tokit::Lexer<'a>>::Source as Source<usize>>::Slice<
  'a,
> as tokit::Slice<'a>>::Char;
/// The error type for lexing based on syntactic [`Token`].
pub type Error<'a, S> = error::Error<Char<'a, S>, RecursionLimitExceeded>;
/// A collection of errors for syntactic [`Token`].
pub type Errors<'a, S> = error::Errors<Char<'a, S>, RecursionLimitExceeded>;

impl<'inp, S> tokit::Lexer<'inp> for Lexyul<'inp, S, Token<S::Slice<'inp>>>
where
  Token<S::Slice<'inp>>: TokenBridge<'inp, Kind = TokenKind>,
  <Token<S::Slice<'inp>> as TokenT<'inp>>::Error: From<<<Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos as Logos<'inp>>::Error>
    + From<<<<Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos as Logos<'inp>>::Extras as State>::Error>,
  <Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos: Logos<'inp, Source = <S as SourceBridge<'inp>>::Logos>,
  <<Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos as Logos<'inp>>::Extras: State + Default,
  S: SourceBridge<'inp>,
{
  type State = <<Token<S::Slice<'inp>> as TokenBridge<'inp>>::Logos as Logos<'inp>>::Extras;
  type Source = S;
  type Token = Token<S::Slice<'inp>>;
  type Span = SimpleSpan;
  type Offset = usize;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn new(input: &'inp Self::Source) -> Self {
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
  fn slice(&self) -> <Self::Source as Source<Self::Offset>>::Slice<'inp> {
    let span = self.inner.span();
    self
      .input
      .slice(&span.start..&span.end)
      .expect("slice of the current lexer span should not be None")
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn lex(&mut self) -> Option<Result<Token<S::Slice<'inp>>, <Token<S::Slice<'inp>> as TokenT<'inp>>::Error>> {
    match self.inner.next() {
      Some(Ok(tok)) => match self.check() {
        Ok(_) => Some(Ok(
          match <Token<S::Slice<'inp>> as TokenBridge<'inp>>::kind(&tok) {
            TokenKind::ColonAssign => Token::ColonAssign,
            TokenKind::ThinArrow => Token::ThinArrow,
            TokenKind::LBrace => Token::LBrace,
            TokenKind::RBrace => Token::RBrace,
            TokenKind::LParen => Token::LParen,
            TokenKind::RParen => Token::RParen,
            TokenKind::Dot => Token::Dot,
            TokenKind::Comma => Token::Comma,
            TokenKind::Leave => Token::Leave,
            TokenKind::Continue => Token::Continue,
            TokenKind::Break => Token::Break,
            TokenKind::Switch => Token::Switch,
            TokenKind::Case => Token::Case,
            TokenKind::Default => Token::Default,
            TokenKind::Function => Token::Function,
            TokenKind::Let => Token::Let,
            TokenKind::If => Token::If,
            TokenKind::For => Token::For,
            TokenKind::Identifier => Token::Identifier(self.slice()),
            TokenKind::Lit(lit) => Token::Lit(lit.map(|_| self.slice())),
            #[cfg(feature = "evm")]
            TokenKind::EvmBuiltin(f) => {
              Token::EvmBuiltin(f.map(|_| self.slice()))
            },
          }
        )),
        Err(e) => Some(Err(e)),
      },
      Some(Err(err)) => Some(Err(err.into())),
      None => None,
    }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn bump(&mut self, n: &usize) {
    self.inner.bump(*n);
  }
}

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
  EvmBuiltin(super::EvmBuiltinFunction<S>),
}

/// The kind of Yul syntactic token.
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, IsVariant)]
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
  Lit(Lit),
  /// Yul EVM built-in function
  ///
  /// Spec: [Yul evm built-in functions](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulEVMBuiltin)
  #[cfg(feature = "evm")]
  #[cfg_attr(docsrs, doc(cfg(feature = "evm")))]
  EvmBuiltin(super::EvmBuiltinFunction),
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
      Self::Lit(lit) => TokenKind::Lit(lit.unit()),
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(evm) => TokenKind::EvmBuiltin(evm.unit()),
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

  /// Returns `true` if the token may be a YUL path start token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_path_start(&self) -> bool {
    matches!(self, Self::Identifier(_))
  }

  /// Returns `true` if the token may be a YUL expression start token.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_expression_start(&self) -> bool {
    match self {
      Self::Identifier(_) | Self::Lit(_) => true,
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(_) => true,
      _ => false,
    }
  }

  /// Returns `true` if the token is a keyword.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_keyword(&self) -> bool {
    matches!(
      self,
      Self::Leave
        | Self::Continue
        | Self::Break
        | Self::Switch
        | Self::Case
        | Self::Default
        | Self::Function
        | Self::Let
        | Self::If
        | Self::For
    )
  }

  /// Returns `true` if the token is a semi identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_semi_identifier(&self) -> bool {
    match self {
      Self::Leave
      | Self::Continue
      | Self::Break
      | Self::Switch
      | Self::Case
      | Self::Default
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

impl<'a, S: 'a> PunctuatorToken<'a> for Token<S>
where
  Token<S>: tokit::Token<'a>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_open_brace(&self) -> bool {
    matches!(self, Self::LBrace)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_close_brace(&self) -> bool {
    matches!(self, Self::RBrace)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_open_paren(&self) -> bool {
    matches!(self, Self::LParen)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_close_paren(&self) -> bool {
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
  Token<S>: tokit::Token<'a>,
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
  Token<S>: tokit::Token<'a>,
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
  Token<S>: tokit::Token<'a>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_identifier(&self) -> bool {
    matches!(self, Self::Identifier(_))
  }
}

impl<'a, S: 'a> KeywordToken<'a> for Token<S>
where
  Token<S>: tokit::Token<'a>,
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
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(f) => Some(f.as_str()),
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
const _: () = {
  impl<S> Require<super::EvmBuiltinFunction<S>> for Token<S> {
    type Err = Self;

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn matched(&self) -> bool {
      self.is_evm_builtin()
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn require(self) -> Result<super::EvmBuiltinFunction<S>, Self::Err>
    where
      Self: Sized,
    {
      self.try_unwrap_evm_builtin().map_err(|e| e.input)
    }
  }

  impl<S> TryFrom<Token<S>> for super::EvmBuiltinFunction<S> {
    type Error = Token<S>;

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn try_from(value: Token<S>) -> Result<Self, Self::Error> {
      value.try_unwrap_evm_builtin().map_err(|e| e.input)
    }
  }
};

super::syntax_kind!(
  /// The syntax kinds for Yul
  enum SyntaxKind { @evm: [
    "stop",
    "add",
    "sub",
    "mul",
    "div",
    "sdiv",
    "mod",
    "smod",
    "exp",
    "not",
    "lt",
    "gt",
    "slt",
    "sgt",
    "eq",
    "iszero",
    "and",
    "or",
    "xor",
    "byte",
    "shl",
    "shr",
    "sar",
    "clz",
    "addmod",
    "mulmod",
    "signextend",
    "keccak256",
    "pop",
    "mload",
    "mstore",
    "mstore8",
    "sload",
    "sstore",
    "tload",
    "tstore",
    "msize",
    "gas",
    "address",
    "balance",
    "selfbalance",
    "caller",
    "callvalue",
    "calldataload",
    "calldatasize",
    "calldatacopy",
    "extcodesize",
    "extcodecopy",
    "returndatasize",
    "returndatacopy",
    "mcopy",
    "extcodehash",
    "create",
    "create2",
    "call",
    "callcode",
    "delegatecall",
    "staticcall",
    "return",
    "revert",
    "selfdestruct",
    "invalid",
    "log0",
    "log1",
    "log2",
    "log3",
    "log4",
    "chainid",
    "origin",
    "gasprice",
    "blockhash",
    "blobhash",
    "coinbase",
    "timestamp",
    "number",
    "difficulty",
    "prevrandao",
    "gaslimit",
    "basefee",
    "blobbasefee",
]});

impl super::super::sealed::Sealed for super::Yul<SyntaxKind> {
  const INIT: Self = super::Yul(core::marker::PhantomData);
  const NAME: &'static str = "yul";
  const DECIMAL_NUMBER_PATTERN: &'static str = r"0|[1-9][0-9]*";
  const HEX_NUMBER_PATTERN: &'static str = r"0x[0-9a-fA-F]+";
}

#[cfg(not(feature = "rowan"))]
impl tokit::syntax::Language for super::Yul<SyntaxKind> {
  type SyntaxKind = SyntaxKind;
}

#[cfg(feature = "rowan")]
#[cfg_attr(docsrs, doc(cfg(feature = "rowan")))]
const _: () = {
  use rowan::{Language, SyntaxKind as RowanSyntaxKind};

  impl Language for super::Yul<SyntaxKind> {
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
