use derive_more::{IsVariant, TryUnwrap, Unwrap};
use lexsol::yul::YUL;
use logosky::{
  LogoStream, Logos, PunctuatorToken, Source, Token, chumsky::{
    Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra, prelude::*,
  }, error::{UnexpectedEot, UnexpectedToken}, utils::cmp::Equivalent
};

use crate::SyntaxKind;

use super::statement::function_call::FunctionCall;

/// A Yul expression.
///
/// See: [Yul expression](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulExpression)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Expression<Name, Path, Literal, Container, Lang = YUL> {
  /// Yul path
  ///
  /// See: [Yul path](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulPath)
  Path(Path),
  /// Yul function call
  ///
  /// See: [Yul function call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
  FunctionCall(FunctionCall<Name, Self, Container, Lang>),
  /// Yul literal
  ///
  /// See: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
  Literal(Literal),
}

impl<'a, Name, Container, Path, Literal, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for Expression<Name, Path, Literal, Container, Lang>
where
  T: PunctuatorToken<'a>,
  str: Equivalent<T>,
  Path: Parseable<'a, I, T, Error>,
  Name: Parseable<'a, I, T, Error>,
  Literal: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Self>,
  Error: From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedEot> + From<UnexpectedToken<'a, T, SyntaxKind>>,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    recursive(|expr| {
      let fncall_parser = FunctionCall::parser(expr);

      choice((
        Path::parser().map(Self::Path),
        fncall_parser.map(Self::FunctionCall),
        Literal::parser().map(Self::Literal),
      ))
    })
  }
}
