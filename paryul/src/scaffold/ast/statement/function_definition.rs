use core::marker::PhantomData;
use logosky::{
  KeywordToken, LogoStream, Logos, OperatorToken, PunctuatorToken, Source, Token,
  chumsky::{
    Parseable, Parser,
    container::Container as ChumskyContainer,
    extra::ParserExtra,
    prelude::*,
    token::{
      expected_keyword,
      operator::arrow,
      punct::{comma, paren_close, paren_open},
    },
  },
  error::{UnexpectedEot, UnexpectedToken},
  syntax::Language,
  utils::{Span, cmp::Equivalent},
};

use crate::{SyntaxKind, YUL};

/// A scaffold AST node for Yul function definition arguments.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDefinitionArguments<Arg, Container = Vec<Arg>, Lang = YUL> {
  span: Span,
  args: Container,
  _m: PhantomData<Arg>,
  _lang: PhantomData<Lang>,
}

impl<Arg, Container, Lang> FunctionDefinitionArguments<Arg, Container, Lang> {
  /// Create a new function definition arguments.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, args: Container) -> Self {
    Self {
      span,
      args,
      _m: PhantomData,
      _lang: PhantomData,
    }
  }

  /// Get the span of the function definition arguments.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the arguments of the function definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn arguments(&self) -> &Container {
    &self.args
  }

  /// Returns the slice of the function definition arguments.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn arguments_slice(&self) -> &[Arg]
  where
    Container: AsRef<[Arg]>,
  {
    self.args.as_ref()
  }
}

impl<'a, Arg, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for FunctionDefinitionArguments<Arg, Container, Lang>
where
  T: PunctuatorToken<'a>,
  Arg: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Arg>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Error: From<<T::Logos as Logos<'a>>::Error>
    + From<UnexpectedToken<'a, T, Lang::SyntaxKind>>
    + From<UnexpectedEot>
    + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Arg::parser()
      .separated_by(comma(|| SyntaxKind::Comma.into()))
      .collect()
      .map_with(|args, exa| Self::new(exa.span(), args))
  }
}

/// A scaffold AST node for Yul function definition return parameters.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDefinitionReturnParameters<Param, Container, Lang = YUL> {
  span: Span,
  params: Container,
  _m: PhantomData<Param>,
  _lang: PhantomData<Lang>,
}

impl<Param, Container, Lang> FunctionDefinitionReturnParameters<Param, Container, Lang> {
  /// Create a new function definition return parameters.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, params: Container) -> Self {
    Self {
      span,
      params,
      _m: PhantomData,
      _lang: PhantomData,
    }
  }

  /// Get the span of the function definition return parameters.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the return parameters of the function definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn return_parameters(&self) -> &Container {
    &self.params
  }

  /// Returns the slice of the function definition return parameters.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn return_parameters_slice(&self) -> &[Param]
  where
    Container: AsRef<[Param]>,
  {
    self.params.as_ref()
  }
}

impl<'a, Param, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for FunctionDefinitionReturnParameters<Param, Container, Lang>
where
  T: PunctuatorToken<'a>,
  Param: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Param>,
  Error: From<<T::Logos as Logos<'a>>::Error>
    + From<UnexpectedToken<'a, T, Lang::SyntaxKind>>
    + From<UnexpectedEot>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Param::parser()
      .separated_by(comma(|| SyntaxKind::Comma.into()))
      .collect()
      .map_with(|params, exa| Self::new(exa.span(), params))
  }
}

/// The scaffold AST for [yul-function-definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionDefinition)
///
/// Spec: [Yul Function Definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionDefinition)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDefinition<Name, Arguments, ReturnParameters, Block, Lang = YUL> {
  span: Span,
  name: Name,
  arguments: Arguments,
  return_params: Option<ReturnParameters>,
  block: Block,
  _lang: PhantomData<Lang>,
}

impl<Name, Arguments, ReturnParameters, Block, Lang>
  FunctionDefinition<Name, Arguments, ReturnParameters, Block, Lang>
{
  /// Create a new function definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(
    span: Span,
    name: Name,
    arguments: Arguments,
    return_params: Option<ReturnParameters>,
    block: Block,
  ) -> Self {
    Self {
      span,
      name,
      arguments,
      return_params,
      block,
      _lang: PhantomData,
    }
  }

  /// Return the span of the function definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the name of the function definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Get the arguments of the function definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn arguments(&self) -> &Arguments {
    &self.arguments
  }

  /// Get the return parameters of the function definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn return_parameters(&self) -> Option<&ReturnParameters> {
    self.return_params.as_ref()
  }

  /// Get the block of the function definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn block(&self) -> &Block {
    &self.block
  }
}

impl<'a, Name, Arguments, ReturnParameters, Block, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for FunctionDefinition<Name, Arguments, ReturnParameters, Block, Lang>
where
  T: KeywordToken<'a> + PunctuatorToken<'a> + OperatorToken<'a>,
  str: Equivalent<T>,
  Name: Parseable<'a, I, T, Error>,
  Arguments: Parseable<'a, I, T, Error>,
  ReturnParameters: Parseable<'a, I, T, Error>,
  Block: Parseable<'a, I, T, Error>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Error: From<<T::Logos as Logos<'a>>::Error>
    + From<UnexpectedToken<'a, T, Lang::SyntaxKind>>
    + From<UnexpectedEot>
    + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    expected_keyword("function", || SyntaxKind::function_KW.into())
      .ignore_then(Name::parser())
      .then(Arguments::parser().delimited_by(
        paren_open(|| SyntaxKind::LBrace.into()),
        paren_close(|| SyntaxKind::RBrace.into()),
      ))
      .then(
        arrow(|| SyntaxKind::ThinArrow.into())
          .ignore_then(ReturnParameters::parser())
          .or_not(),
      )
      .then(Block::parser())
      .map_with(|(((name, args), return_parameters), block), exa| {
        Self::new(exa.span(), name, args, return_parameters, block)
      })
  }
}
