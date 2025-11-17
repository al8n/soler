use core::marker::PhantomData;

#[cfg(feature = "evm")]
use lexsol::yul::EvmBuiltinFunction;
#[cfg(feature = "evm")]
use logosky::{
  Require,
  error::{UnexpectedEot, UnexpectedToken},
  utils::Spanned,
};

use logosky::{
  IdentifierToken, Lexed, LogoStream, Logos, PunctuatorToken, Source, Token,
  chumsky::{
    Parseable, Parser,
    container::Container as ChumskyContainer,
    extra::ParserExtra,
    prelude::*,
    token::punct::{comma, paren_close, paren_open},
  },
  syntax::Language,
  types::Ident,
  utils::{AsSpan, Span},
};

use crate::{SyntaxKind, YUL};

/// A scaffold AST node for a Yul function call name.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionName<S, Lang = YUL> {
  ident: Ident<S, Lang>,
  _lang: PhantomData<Lang>,
}

impl<S, Lang> From<Ident<S, Lang>> for FunctionName<S, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(ident: Ident<S, Lang>) -> Self {
    Self::new(ident)
  }
}

impl<S, Lang> FunctionName<S, Lang> {
  /// Create a new path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(ident: Ident<S, Lang>) -> Self {
    Self {
      ident,
      _lang: PhantomData,
    }
  }

  /// Returns the span of the path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.ident.span()
  }

  /// Get the identifier of the path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn ident(&self) -> &Ident<S, Lang> {
    &self.ident
  }
}

#[cfg(not(feature = "evm"))]
impl<'a, S, I, T, Lang, Error> Parseable<'a, I, T, Error> for FunctionName<S, Lang>
where
  T: IdentifierToken<'a>,
  T::Logos: Logos<'a>,
  <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
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
    logosky::chumsky::token::identifier_slice(|| SyntaxKind::Identifier.into()).map(
      |ident: Spanned<S>| {
        let (span, ident) = ident.into_components();
        FunctionName::new(Ident::new(span, ident))
      },
    )
  }
}

#[cfg(feature = "evm")]
impl<'a, S, I, T, Lang, Error> Parseable<'a, I, T, Error> for FunctionName<S, Lang>
where
  T: IdentifierToken<'a> + Require<EvmBuiltinFunction, Err = T>,
  T::Logos: Logos<'a>,
  <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
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
    custom(|inp| {
      let before = inp.cursor();
      let tok: Option<Lexed<'_, T>> = inp.next();
      match tok {
        None => Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
        Some(Lexed::Error(err)) => Err(err.into()),
        Some(Lexed::Token(Spanned { span, data: tok })) => {
          let ident = match tok.try_into_identifier() {
            Ok(ident) => Ident::new(span, ident),
            Err(tok) => match tok.require() {
              Ok(_) => Ident::new(span, inp.slice(&before..&inp.cursor())),
              Err(tok) => {
                return Err(
                  UnexpectedToken::expected_one_with_found(
                    span,
                    tok,
                    SyntaxKind::FunctionName.into(),
                  )
                  .into(),
                );
              }
            },
          };

          Ok(FunctionName::new(ident))
        }
      }
    })
  }
}

/// A scaffold AST node for a Yul function call.
///
/// See: [Yul function call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionCall<Name, Expression, Container = Vec<Expression>, Lang = YUL> {
  span: Span,
  name: Name,
  expressions: Container,
  _m: PhantomData<Expression>,
  _lang: PhantomData<Lang>,
}

impl<Name, Expression, Container, Lang> AsSpan<Span>
  for FunctionCall<Name, Expression, Container, Lang>
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    self.span_ref()
  }
}

impl<Name, Expression, Container, Lang> FunctionCall<Name, Expression, Container, Lang> {
  /// Create a new function call.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, name: Name, expressions: Container) -> Self {
    Self {
      span,
      name,
      expressions,
      _m: PhantomData,
      _lang: PhantomData,
    }
  }

  /// Get the span of the function call.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the reference to the span of the function call.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_ref(&self) -> &Span {
    &self.span
  }

  /// Get the mutable reference to the span of the function call.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Get the name of the function call.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Get the expressions of the function call.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn expressions(&self) -> &Container {
    &self.expressions
  }

  /// Returns the slice of the expressions.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn expressions_slice(&self) -> &[Expression]
  where
    Container: AsRef<[Expression]>,
  {
    self.expressions.as_ref()
  }

  /// Returns a parser for the FunctionCall with the given expression parser.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn parser<'a, I, T, Error, E>(
    expression_parser: impl Parser<'a, I, Expression, E> + Clone,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: PunctuatorToken<'a>,
    Name: Parseable<'a, I, T, Error>,
    Error:
      From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedToken<'a, T, Lang::SyntaxKind>> + 'a,
    Container: ChumskyContainer<Expression>,
    Lang: Language,
    Lang::SyntaxKind: From<SyntaxKind> + 'a,
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Name::parser()
      .then(
        expression_parser
          .separated_by(comma(|| SyntaxKind::Comma.into()))
          .collect::<Container>()
          .delimited_by(
            paren_open(|| SyntaxKind::LParen.into()),
            paren_close(|| SyntaxKind::RParen.into()),
          ),
      )
      .map_with(|(name, expressions), exa| Self::new(exa.span(), name, expressions))
  }
}
