use core::marker::PhantomData;

use lexsol::yul::YUL;

#[cfg(feature = "evm")]
use lexsol::yul::EvmBuiltinFunction;
#[cfg(feature = "evm")]
use logosky::{
  Require,
  error::{UnexpectedEot, UnexpectedToken},
  utils::Spanned,
};

use logosky::{
  IdentifierToken, Lexed, LogoStream, Logos, PunctuatorToken, Source, Token, chumsky::{
    Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra, prelude::*, punctuator,
  }, utils::{Span, cmp::Equivalent}
};

use crate::{Ident, SyntaxKind};

/// A scaffold AST node for a Yul function call name.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionCallName<S, Lang = YUL> {
  ident: Ident<S>,
  _lang: PhantomData<Lang>,
}

impl<S, Lang> From<Ident<S>> for FunctionCallName<S, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(ident: Ident<S>) -> Self {
    Self::new(ident)
  }
}

impl<S, Lang> FunctionCallName<S, Lang> {
  /// Create a new path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(ident: Ident<S>) -> Self {
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
  pub const fn ident(&self) -> &Ident<S> {
    &self.ident
  }
}

#[cfg(not(feature = "evm"))]
impl<'a, S, I, T, Lang, Error> Parseable<'a, I, T, Error> for FunctionCallName<S, Lang>
where
  T: IdentifierToken<'a>,
  T::Logos: Logos<'a>,
  <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
  Error: From<<T::Logos as Logos<'a>>::Error>
    + From<UnexpectedToken<'a, T, SyntaxKind>>
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
            Err(tok) => {
              return Err(
                UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::Identifier).into(),
              );
            }
          };

          Ok(Self::new(ident))
        }
      }
    })
  }
}

#[cfg(feature = "evm")]
impl<'a, S, I, T, Lang, Error> Parseable<'a, I, T, Error> for FunctionCallName<S, Lang>
where
  T: IdentifierToken<'a> + Require<EvmBuiltinFunction, Err = T>,
  T::Logos: Logos<'a>,
  <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
  Error: From<<T::Logos as Logos<'a>>::Error>
    + From<UnexpectedToken<'a, T, SyntaxKind>>
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
                  UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::FunctionCallName)
                    .into(),
                );
              }
            },
          };

          Ok(FunctionCallName::new(ident))
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
    str: Equivalent<T>,
    Name: Parseable<'a, I, T, Error>,
    Error: From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedEot> + From<UnexpectedToken<'a, T, SyntaxKind>> + 'a,
    Container: ChumskyContainer<Expression>,
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Name::parser()
      .then(
        expression_parser
          .separated_by(punctuator(",", || SyntaxKind::Comma))
          .collect::<Container>()
          .delimited_by(punctuator("(", || SyntaxKind::LParen), punctuator(")", || SyntaxKind::RParen)),
      )
      .map_with(|(name, expressions), exa| Self::new(exa.span(), name, expressions))
  }
}

// impl<'a, Name, Expression, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
//   for FunctionCall<Name, Expression, Container, Lang>
// where
//   T: Token<'a>,
//   Name: Parseable<'a, I, T, Error>,
//   LParen: Parseable<'a, I, T, Error>,
//   RParen: Parseable<'a, I, T, Error>,
//   Comma: Parseable<'a, I, T, Error>,
//   Expression: Parseable<'a, I, T, Error>,
//   Container: ChumskyContainer<Expression>,
// {
//   fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
//   where
//     Self: Sized + 'a,
//     I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//     T: Token<'a>,
//     Error: 'a,
//     E: ParserExtra<'a, I, Error = Error> + 'a,
//   {
//     Self::parser_with_expression(Expression::parser())
//   }
// }

// impl<'a, Name, Expression, Container, Lang, I, T, Error> Recoverable<'a, I, T, Error>
//   for FunctionCall<Name, Expression, Container, Lang>
// where
//   T: IdentifierToken<'a> + PunctuatorToken<'a> + TriviaToken<'a>,
//   T::Logos: Logos<'a>,
//   Name: Parseable<'a, I, T, Error> + ErrorNode,
//   LParen: Parseable<'a, I, T, Error>,
//   RParen: Parseable<'a, I, T, Error>,
//   Comma: Parseable<'a, I, T, Error>,
//   Expression: Recoverable<'a, I, T, Error> + ErrorNode,
//   Error: From<<T::Logos as Logos<'a>>::Error>
//     + From<UnexpectedToken<'a, T, SyntaxKind>>
//     + From<UnexpectedEot>
//     + 'a,
//   Container: ChumskyContainer<Expression>,
// {
//   fn recoverable_parser<E>() -> impl Parser<'a, I, Self, E> + Clone
//   where
//     Self: Sized + 'a,
//     I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//     T: Token<'a>,
//     Error: 'a,
//     E: ParserExtra<'a, I, Error = Error> + 'a,
//   {
//     Name::parser()
//       .recover_with(via_parser(skip_until_token(|t: &T| t.is_identifier()).map_with(|_, exa| Name::error(exa.span()))))
//       .then_ignore(
//         LParen::parser()
//           .recover_with(via_parser(any().validate().re))
//       )
//       .then(
//         Expression::parser()
//           .separated_by(Comma::parser())
//           .collect::<Container>(),
//       )
//       .then_ignore(RParen::parser())
//       .map_with(|(name, expressions), exa| Self::new(exa.span(), name, expressions))
//       .recover_with(via_parser(skip_until_token(|t: &T| {
//         !t.is_trivia()
//       })
//       .map_with(|_, exa| Self::new(exa.span(), Name::parser().parse_inner(exa).unwrap(), Container::default()))))
//   }
// }
