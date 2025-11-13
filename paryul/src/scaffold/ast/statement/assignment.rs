use core::marker::PhantomData;

use derive_more::{IsVariant, TryUnwrap, Unwrap};
use lexsol::yul::YUL;
use logosky::{
  KeywordToken, Lexed, LogoStream, Logos, OperatorToken, PunctuatorToken, Source, Token,
  chumsky::{
    IterParser, Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra,
    operator, prelude::custom, punctuator,
  },
  error::{UnexpectedEot, UnexpectedToken},
  utils::{Span, Spanned, cmp::Equivalent},
};

use crate::SyntaxKind;

/// A scaffold AST for single assignment in Yul.
///
/// ## Yul Syntax Examples
///
/// ```yul
/// x := 42
/// ```
///
/// See [Yul assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SingleTargetAssignment<Path, Expr, Lang = YUL> {
  span: Span,
  path: Path,
  expr: Expr,
  _m: PhantomData<Lang>,
}

impl<Path, Expr, Lang> SingleTargetAssignment<Path, Expr, Lang> {
  /// Create a new single assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, path: Path, expr: Expr) -> Self {
    Self {
      span,
      path,
      expr,
      _m: PhantomData,
    }
  }

  /// Get the span of the single assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the path of the single assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn path(&self) -> &Path {
    &self.path
  }

  /// Get the expression of the single assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn expr(&self) -> &Expr {
    &self.expr
  }
}

impl<'a, Path, Expr, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for SingleTargetAssignment<Path, Expr, Lang>
where
  T: KeywordToken<'a> + OperatorToken<'a>,
  str: Equivalent<T>,
  Path: Parseable<'a, I, T, Error>,
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
    Path::parser()
      .then_ignore(operator(":=", || SyntaxKind::ColonAssign))
      .then(Expr::parser())
      .map_with(|(path, expr), exa| Self::new(exa.span(), path, expr))
  }
}

/// A scaffold AST for multiple variables assignment in Yul.
///
/// ## Yul Syntax Examples
/// ```yul
/// x, y := foo()
/// ```
///
/// See [Yul assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MultipleTargetAssignment<Path, FunctionCall, Container = Vec<Path>, Lang = YUL> {
  span: Span,
  paths: Container,
  fn_call: FunctionCall,
  _m: PhantomData<Lang>,
  _p: PhantomData<Path>,
}

impl<Path, FunctionCall, Container, Lang>
  MultipleTargetAssignment<Path, FunctionCall, Container, Lang>
{
  /// Create a new multiple assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, paths: Container, fn_call: FunctionCall) -> Self {
    Self {
      span,
      paths,
      fn_call,
      _m: PhantomData,
      _p: PhantomData,
    }
  }

  /// Get the span of the multiple assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the paths of the multiple assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn paths(&self) -> &Container {
    &self.paths
  }

  /// Get the function call of the multiple assignment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn function_call(&self) -> &FunctionCall {
    &self.fn_call
  }
}

impl<'a, Path, FunctionCall, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for MultipleTargetAssignment<Path, FunctionCall, Container, Lang>
where
  T: KeywordToken<'a> + OperatorToken<'a> + PunctuatorToken<'a>,
  str: Equivalent<T>,
  Path: Parseable<'a, I, T, Error>,
  FunctionCall: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Path> + Clone + 'a,
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
    Path::parser()
      .separated_by(punctuator(",", || SyntaxKind::Comma))
      .at_least(2)
      .collect()
      .then_ignore(operator(":=", || SyntaxKind::ColonAssign))
      .then(FunctionCall::parser())
      .map_with(|(paths, fn_call), exa| Self::new(exa.span(), paths, fn_call))
  }
}

/// A scaffold AST for assignment in Yul.
///
/// ## Yul Syntax Examples
/// ```yul
/// x := 42
/// x, y := foo()
/// ```
///
/// See [Yul assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Assignment<Path, Expr, FunctionCall, Container = Vec<Path>, Lang = YUL> {
  /// A single target assignment.
  Single(SingleTargetAssignment<Path, Expr, Lang>),
  /// A multiple target assignment.
  Multiple(MultipleTargetAssignment<Path, FunctionCall, Container, Lang>),
}

impl<'a, Path, Expr, FunctionCall, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for Assignment<Path, Expr, FunctionCall, Container, Lang>
where
  T: OperatorToken<'a> + PunctuatorToken<'a>,
  str: Equivalent<T>,
  Path: Parseable<'a, I, T, Error>,
  Expr: Parseable<'a, I, T, Error>,
  FunctionCall: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Path> + Clone + 'a,
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
      let first_path = inp.parse(Path::parser())?;

      Ok(match inp.next() {
        None => return Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
        Some(Lexed::Error(e)) => return Err(<Error as core::convert::From<_>>::from(e)),
        Some(Lexed::Token(Spanned { span, data: tok })) => match () {
          () if Equivalent::equivalent(":=", &tok) => {
            let expr = inp.parse(Expr::parser())?;
            Self::Single(SingleTargetAssignment::new(
              inp.span_since(&before),
              first_path,
              expr,
            ))
          }
          () if Equivalent::equivalent(",", &tok) => {
            let mut paths = Container::default();
            paths.push(first_path);

            loop {
              let path = inp.parse(Path::parser())?;
              paths.push(path);

              match inp.next() {
                None => return Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
                Some(Lexed::Error(e)) => return Err(<Error as core::convert::From<_>>::from(e)),
                Some(Lexed::Token(Spanned { span, data: tok })) => match () {
                  () if Equivalent::equivalent(",", &tok) => {
                    continue;
                  }
                  () if Equivalent::equivalent(":=", &tok) => {
                    let fn_call = inp.parse(FunctionCall::parser())?;
                    break Self::Multiple(MultipleTargetAssignment::new(
                      inp.span_since(&before),
                      paths,
                      fn_call,
                    ));
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
