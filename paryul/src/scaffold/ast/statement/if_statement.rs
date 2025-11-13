use core::marker::PhantomData;

use lexsol::yul::YUL;
use logosky::{
  KeywordToken, LogoStream, Logos, Source, Token,
  chumsky::{Parseable, Parser, extra::ParserExtra, keyword},
  error::UnexpectedToken,
  utils::{Span, cmp::Equivalent},
};

use crate::SyntaxKind;

/// A scaffold AST for Yul if statement.
///
/// See [Yul if statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulIfStatement)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct IfStatement<Expr, Block, Lang = YUL> {
  span: Span,
  condition: Expr,
  block: Block,
  _lang: PhantomData<Lang>,
}

impl<Expr, Block, Lang> IfStatement<Expr, Block, Lang> {
  /// Create a new if statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, condition: Expr, block: Block) -> Self {
    Self {
      span,
      condition,
      block,
      _lang: PhantomData,
    }
  }

  /// Get the span of the if statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the condition of the if statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn condition(&self) -> &Expr {
    &self.condition
  }

  /// Get the block of the if statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn block(&self) -> &Block {
    &self.block
  }
}

impl<'a, Expr, Block, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for IfStatement<Expr, Block, Lang>
where
  T: KeywordToken<'a>,
  str: Equivalent<T>,
  Expr: Parseable<'a, I, T, Error>,
  Block: Parseable<'a, I, T, Error>,
  Error: From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedToken<'a, T, SyntaxKind>> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    keyword("if", || SyntaxKind::if_KW)
      .ignore_then(Expr::parser())
      .then(Block::parser())
      .map_with(|(condition, block), exa| Self::new(exa.span(), condition, block))
  }
}
