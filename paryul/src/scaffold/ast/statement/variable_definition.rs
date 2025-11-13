use core::marker::PhantomData;

use derive_more::{IsVariant, TryUnwrap, Unwrap};
use lexsol::yul::YUL;
use logosky::{
  KeywordToken, Lexed, LogoStream, Logos, OperatorToken, PunctuatorToken, Source, Token,
  chumsky::{
    IterParser, Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra,
    keyword, operator, prelude::custom, punctuator,
  },
  error::{UnexpectedEot, UnexpectedToken},
  utils::{Span, Spanned, cmp::Equivalent},
};

use crate::SyntaxKind;

/// A scaffold AST for a single variable declaration in Yul.
///
/// ## Yul Syntax Examples
///
/// ```yul
/// let x := 42
/// let y
/// ```
///
/// See [Yul variable declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SingleVariableDeclaration<Name, Expr, Lang = YUL> {
  span: Span,
  name: Name,
  expr: Option<Expr>,
  _m: PhantomData<Lang>,
}

impl<Name, Expr, Lang> SingleVariableDeclaration<Name, Expr, Lang> {
  /// Create a new single variable declaration.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, name: Name, expr: Option<Expr>) -> Self {
    Self {
      span,
      name,
      expr,
      _m: PhantomData,
    }
  }

  /// Get the span of the single variable declaration.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the name of the single variable declaration.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Get the expression of the single variable declaration.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn expr(&self) -> Option<&Expr> {
    self.expr.as_ref()
  }
}

impl<'a, Name, Expr, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for SingleVariableDeclaration<Name, Expr, Lang>
where
  T: KeywordToken<'a> + OperatorToken<'a>,
  str: Equivalent<T>,
  Name: Parseable<'a, I, T, Error>,
  Expr: Parseable<'a, I, T, Error>,
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
    keyword("let", || SyntaxKind::let_KW)
      .ignore_then(Name::parser())
      .then(
        operator(":=", || SyntaxKind::ColonAssign)
          .ignore_then(Expr::parser())
          .or_not(),
      )
      .map_with(|(name, expr), exa| Self::new(exa.span(), name, expr))
  }
}

/// A scaffold AST for multiple name variable declarations in Yul.
///
/// ## Yul Syntax Examples
///
/// ```yul
/// let x, y := foo()
/// let a, b
/// ```
///
/// See [Yul variable declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MultipleVariableDeclaration<Name, FunctionCall, Container = Vec<Name>, Lang = YUL> {
  span: Span,
  names: Container,
  fn_call: Option<FunctionCall>,
  _m: PhantomData<Lang>,
  _n: PhantomData<Name>,
}

impl<Name, FunctionCall, Container, Lang>
  MultipleVariableDeclaration<Name, FunctionCall, Container, Lang>
{
  /// Create a new multiple variable declaration.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, names: Container, fn_call: Option<FunctionCall>) -> Self {
    Self {
      span,
      names,
      fn_call,
      _m: PhantomData,
      _n: PhantomData,
    }
  }

  /// Get the span of the multiple variable declaration.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the names of the multiple variable declaration.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn names(&self) -> &Container {
    &self.names
  }

  /// Returns the slice of the names.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn names_slice(&self) -> &[Name]
  where
    Container: AsRef<[Name]>,
  {
    self.names.as_ref()
  }

  /// Get the function call of the multiple variable declaration.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn fn_call(&self) -> Option<&FunctionCall> {
    self.fn_call.as_ref()
  }
}

impl<'a, Name, FunctionCall, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for MultipleVariableDeclaration<Name, FunctionCall, Container, Lang>
where
  T: KeywordToken<'a> + PunctuatorToken<'a> + OperatorToken<'a>,
  str: Equivalent<T>,
  Name: Parseable<'a, I, T, Error>,
  FunctionCall: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Name> + Clone + 'a,
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
    keyword("let", || SyntaxKind::let_KW)
      .ignore_then(
        Name::parser()
          .separated_by(punctuator(",", || SyntaxKind::Comma))
          .at_least(2)
          .collect(),
      )
      .then(
        operator(":=", || SyntaxKind::ColonAssign)
          .ignore_then(FunctionCall::parser())
          .or_not(),
      )
      .map_with(|(names, fn_call), exa| Self::new(exa.span(), names, fn_call))
  }
}

/// A scaffold AST for variable declarations in Yul, which can be either single or multiple.
///
/// ## Yul Syntax Examples
///
/// ```yul
/// let x := 42
/// let y
/// let a, b := foo()
/// let c, d
/// ```
///
/// See [Yul variable declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum VariableDeclaration<Name, Expr, FunctionCall, Container = Vec<Name>, Lang = YUL> {
  /// A single variable declaration.
  Single(SingleVariableDeclaration<Name, Expr, Lang>),
  /// A multiple variables declaration.
  Multiple(MultipleVariableDeclaration<Name, FunctionCall, Container, Lang>),
}

impl<'a, Name, Expr, FunctionCall, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for VariableDeclaration<Name, Expr, FunctionCall, Container, Lang>
where
  T: KeywordToken<'a> + PunctuatorToken<'a> + OperatorToken<'a>,
  str: Equivalent<T>,
  Name: Parseable<'a, I, T, Error>,
  Expr: Parseable<'a, I, T, Error>,
  FunctionCall: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Name> + Clone + 'a,
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

      // eat "let"
      inp.parse(keyword("let", || SyntaxKind::let_KW).ignored())?;

      // parse first name
      let first_name = inp.parse(Name::parser())?;

      Ok(match inp.next() {
        None => Self::Single(SingleVariableDeclaration::new(
          inp.span_since(&before),
          first_name,
          None,
        )),
        Some(Lexed::Error(e)) => return Err(<Error as core::convert::From<_>>::from(e)),
        Some(Lexed::Token(Spanned { span, data: tok })) => match () {
          () if tok.is_colon_assign() => {
            let expr = inp.parse(Expr::parser())?;
            Self::Single(SingleVariableDeclaration::new(
              inp.span_since(&before),
              first_name,
              Some(expr),
            ))
          }
          () if tok.is_comma() => {
            let mut names = Container::default();
            names.push(first_name);

            // parse remaining names
            loop {
              let name = inp.parse(Name::parser())?;
              names.push(name);

              match inp.next() {
                None => {
                  return Ok(Self::Multiple(MultipleVariableDeclaration::new(
                    inp.span_since(&before),
                    names,
                    None,
                  )));
                }
                Some(Lexed::Error(e)) => return Err(<Error as core::convert::From<_>>::from(e)),
                Some(Lexed::Token(Spanned { span, data: tok })) => match () {
                  () if tok.is_comma() => {
                    continue;
                  }
                  () if tok.is_colon_assign() => {
                    let fn_call = inp.parse(FunctionCall::parser())?;
                    return Ok(Self::Multiple(MultipleVariableDeclaration::new(
                      inp.span_since(&before),
                      names,
                      Some(fn_call),
                    )));
                  }
                  _ => {
                    return Err(
                      UnexpectedToken::expected_one_of_with_found(
                        span,
                        tok,
                        &[SyntaxKind::Comma, SyntaxKind::ColonAssign],
                      )
                      .into(),
                    );
                  }
                },
              }
            }
          }
          _ => {
            return Err(
              UnexpectedToken::expected_one_of_with_found(
                span,
                tok,
                &[SyntaxKind::Comma, SyntaxKind::ColonAssign],
              )
              .into(),
            );
          }
        },
      })
    })
  }
}
