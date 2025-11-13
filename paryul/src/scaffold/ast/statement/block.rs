use core::marker::PhantomData;

use logosky::{
  LogoStream, Logos, PunctuatorToken, Source, Token,
  chumsky::{
    IterParser, Parser,
    container::Container as ChumskyContainer,
    extra::ParserExtra,
    token::punct::{brace_close, brace_open},
  },
  error::UnexpectedToken,
  syntax::Language,
  utils::{Span, cmp::Equivalent},
};

use crate::{SyntaxKind, YUL};

/// A scaffold AST for Yul block statement.
///
/// See [Yul block statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBlock)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Block<Statement, Container = Vec<Statement>, Lang = YUL> {
  span: Span,
  statements: Container,
  _m: PhantomData<Lang>,
  _s: PhantomData<Statement>,
}

impl<Statement, Container, Lang> Block<Statement, Container, Lang> {
  /// Create a new block statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, statements: Container) -> Self {
    Self {
      span,
      statements,
      _m: PhantomData,
      _s: PhantomData,
    }
  }

  /// Get the span of the block statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the mutable span of the block statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Get the statements of the block statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn statements(&self) -> &Container {
    &self.statements
  }

  /// Returns the slice of the block statements.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn statements_slice(&self) -> &[Statement]
  where
    Container: AsRef<[Statement]>,
  {
    self.statements.as_ref()
  }

  /// Returns a parser for the FunctionCall with the given expression parser.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn parser<'a, I, T, Error, E>(
    statement_parser: impl Parser<'a, I, Statement, E> + Clone,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: PunctuatorToken<'a>,
    str: Equivalent<T>,
    Lang: Language,
    Lang::SyntaxKind: From<SyntaxKind> + 'a,
    Error:
      From<UnexpectedToken<'a, T, Lang::SyntaxKind>> + From<<T::Logos as Logos<'a>>::Error> + 'a,
    Container: ChumskyContainer<Statement>,
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    statement_parser
      .repeated()
      .collect()
      .delimited_by(
        brace_open(|| SyntaxKind::LBrace.into()),
        brace_close(|| SyntaxKind::RBrace.into()),
      )
      .map_with(|statements, exa| Self::new(exa.span(), statements))
  }
}
