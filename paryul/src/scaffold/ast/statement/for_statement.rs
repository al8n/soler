use core::marker::PhantomData;

use logosky::{
  KeywordToken, LogoStream, Logos, Source, Token,
  chumsky::{Parseable, Parser, extra::ParserExtra, token::expected_keyword},
  error::UnexpectedToken,
  syntax::Language,
  utils::{Span, cmp::Equivalent},
};

use crate::{SyntaxKind, YUL};

/// A scaffold AST for Yul for statement.
///
/// See [Yul for statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulForStatement)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ForStatement<Init, Condition, Post, Block, Lang = YUL> {
  span: Span,
  init: Init,
  condition: Condition,
  post: Post,
  block: Block,
  _lang: PhantomData<Lang>,
}

impl<Init, Condition, Post, Block, Lang> ForStatement<Init, Condition, Post, Block, Lang> {
  /// Create a new for statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, init: Init, condition: Condition, post: Post, block: Block) -> Self {
    Self {
      span,
      init,
      condition,
      post,
      block,
      _lang: PhantomData,
    }
  }

  /// Get the span of the for statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the initialization part of the for statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn init(&self) -> &Init {
    &self.init
  }

  /// Get the condition part of the for statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn condition(&self) -> &Condition {
    &self.condition
  }

  /// Get the post-expression part of the for statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn post(&self) -> &Post {
    &self.post
  }

  /// Get the block of the for statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn block(&self) -> &Block {
    &self.block
  }
}

impl<'a, Init, Condition, Post, Block, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for ForStatement<Init, Condition, Post, Block, Lang>
where
  T: KeywordToken<'a>,
  str: Equivalent<T>,
  Init: Parseable<'a, I, T, Error>,
  Condition: Parseable<'a, I, T, Error>,
  Post: Parseable<'a, I, T, Error>,
  Block: Parseable<'a, I, T, Error>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Error: From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedToken<'a, T, Lang::SyntaxKind>> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    expected_keyword("for", || SyntaxKind::for_KW.into())
      .ignore_then(Init::parser())
      .then(Condition::parser())
      .then(Post::parser())
      .then(Block::parser())
      .map_with(|(((init, condition), post), block), exa| {
        Self::new(exa.span(), init, condition, post, block)
      })
  }
}
