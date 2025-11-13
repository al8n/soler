use core::marker::PhantomData;

use derive_more::{IsVariant, TryUnwrap, Unwrap};
use logosky::{
  KeywordToken, Lexed, LogoStream, Logos, Source, Token,
  chumsky::{
    Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra, prelude::*,
    token::expected_keyword,
  },
  error::{UnexpectedEot, UnexpectedToken},
  syntax::Language,
  utils::{Span, Spanned, cmp::Equivalent},
};

use crate::{SyntaxKind, YUL};

/// A switch case in a switch statement.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SwitchCase<Literal, Block, Lang = YUL> {
  span: Span,
  literal: Literal,
  block: Block,
  _m: PhantomData<Lang>,
}

impl<Literal, Block, Lang> SwitchCase<Literal, Block, Lang> {
  /// Create a new switch case.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, literal: Literal, block: Block) -> Self {
    Self {
      span,
      literal,
      block,
      _m: PhantomData,
    }
  }

  /// Get the span of the switch case.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the literal of the switch case.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn literal(&self) -> &Literal {
    &self.literal
  }

  /// Get the block of the switch case.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn block(&self) -> &Block {
    &self.block
  }
}

impl<'a, Literal, Block, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for SwitchCase<Literal, Block, Lang>
where
  T: KeywordToken<'a>,
  str: Equivalent<T>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Literal: Parseable<'a, I, T, Error>,
  Block: Parseable<'a, I, T, Error>,
  Error: From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedToken<'a, T, Lang::SyntaxKind>> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    expected_keyword("case", || SyntaxKind::case_KW.into())
      .ignore_then(Literal::parser())
      .then(Block::parser())
      .map_with(|(literal, block), exa| Self::new(exa.span(), literal, block))
  }
}

/// The default case in a switch statement.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DefaultCase<Block, Lang = YUL> {
  span: Span,
  block: Block,
  _m: PhantomData<Lang>,
}

impl<Block, Lang> DefaultCase<Block, Lang> {
  /// Create a new default case.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, block: Block) -> Self {
    Self {
      span,
      block,
      _m: PhantomData,
    }
  }

  /// Get the span of the default case.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the block of the default case.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn block(&self) -> &Block {
    &self.block
  }
}

impl<'a, Block, Lang, I, T, Error> Parseable<'a, I, T, Error> for DefaultCase<Block, Lang>
where
  T: KeywordToken<'a>,
  str: Equivalent<T>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Block: Parseable<'a, I, T, Error>,
  Error: From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedToken<'a, T, Lang::SyntaxKind>> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    expected_keyword("default", || SyntaxKind::default_KW.into())
      .ignore_then(Block::parser())
      .map_with(|block, exa| Self::new(exa.span(), block))
  }
}

/// A case in a switch statement, either a switch case or a default case.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Case<Literal, Block, Lang = YUL> {
  /// The switch case.
  Switch(SwitchCase<Literal, Block, Lang>),
  /// The default case.
  Default(DefaultCase<Block, Lang>),
}

impl<'a, Literal, Block, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for Case<Literal, Block, Lang>
where
  T: KeywordToken<'a>,
  str: Equivalent<T>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Literal: Parseable<'a, I, T, Error>,
  Block: Parseable<'a, I, T, Error>,
  Error: From<<T::Logos as Logos<'a>>::Error>
    + From<UnexpectedToken<'a, T, Lang::SyntaxKind>>
    + From<UnexpectedEot>
    + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    custom(|inp| {
      let before = inp.cursor();
      match inp.next() {
        None => Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
        Some(Lexed::Error(e)) => Err(<Error as core::convert::From<_>>::from(e)),
        Some(Lexed::Token(Spanned { span, data: tok })) => Ok(match () {
          () if Equivalent::equivalent("case", &tok) => {
            let (lit, block) = inp.parse(Literal::parser().then(Block::parser()))?;
            Self::Switch(SwitchCase::new(inp.span_since(&before), lit, block))
          }
          () if Equivalent::equivalent("default", &tok) => {
            let block = inp.parse(Block::parser())?;
            Self::Default(DefaultCase::new(inp.span_since(&before), block))
          }
          _ => {
            return Err(
              UnexpectedToken::expected_one_with_found(
                span,
                tok,
                SyntaxKind::SwitchCaseKeyword.into(),
              )
              .into(),
            );
          }
        }),
      }
    })
  }
}

/// A Yul switch statement.
///
/// See [Yul switch statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulSwitchStatement)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SwitchStatement<Expr, Case, Container = Vec<Case>, Lang = YUL> {
  span: Span,
  expression: Expr,
  cases: Container,
  _c: PhantomData<Case>,
  _m: PhantomData<Lang>,
}

impl<Expr, Case, Container, Lang> SwitchStatement<Expr, Case, Container, Lang> {
  /// Create a new switch statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, expression: Expr, cases: Container) -> Self {
    Self {
      span,
      expression,
      cases,
      _c: PhantomData,
      _m: PhantomData,
    }
  }

  /// Get the span of the switch statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the expression of the switch statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn expression(&self) -> &Expr {
    &self.expression
  }

  /// Get the cases of the switch statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn cases(&self) -> &Container {
    &self.cases
  }

  /// Returns the slice of the cases.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn cases_slice(&self) -> &[Case]
  where
    Container: AsRef<[Case]>,
  {
    self.cases.as_ref()
  }
}

impl<'a, Expr, Case, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for SwitchStatement<Expr, Case, Container, Lang>
where
  T: KeywordToken<'a>,
  str: Equivalent<T>,
  Expr: Parseable<'a, I, T, Error>,
  Case: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Case>,
  Error: From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedToken<'a, T, Lang::SyntaxKind>> + 'a,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    expected_keyword("switch", || SyntaxKind::switch_KW.into())
      .ignore_then(Expr::parser())
      .then(Case::parser().repeated().at_least(1).collect())
      .map_with(|(expression, cases), exa| Self::new(exa.span(), expression, cases))
  }
}
