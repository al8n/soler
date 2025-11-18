use core::marker::PhantomData;

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  KeywordToken, Lexed, LogoStream, Logos, OperatorToken, PunctuatorToken, Source, Token,
  chumsky::{
    IterParser, Parseable, Parser,
    container::Container as ChumskyContainer,
    extra::ParserExtra,
    prelude::custom,
    token::{expected_keyword, operator::colon_eq_assign, punct::comma},
  },
  error::{UnexpectedEot, UnexpectedToken},
  syntax::Language,
  types::Ident,
  utils::{AsSpan, Span, Spanned, cmp::Equivalent},
};

use crate::{SyntaxKind, YUL};

/// A scaffold AST node for a Yul function call name.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariableName<S, Lang = YUL> {
  ident: Ident<S, Lang>,
  _lang: PhantomData<Lang>,
}

impl<S, Lang> AsSpan<Span> for VariableName<S, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    self.ident.span_ref()
  }
}

impl<S, Lang> From<Ident<S, Lang>> for VariableName<S, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(ident: Ident<S, Lang>) -> Self {
    Self::new(ident)
  }
}

impl<S, Lang> VariableName<S, Lang> {
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

  /// Consume the name and return the span and identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_components(self) -> (Span, Ident<S, Lang>) {
    (self.ident.span(), self.ident)
  }

  /// Returns `true` if the variable name is valid node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_valid(&self) -> bool {
    self.ident.is_valid()
  }

  /// Returns `true` if the variable name is an error node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_error(&self) -> bool {
    self.ident.is_error()
  }

  /// Returns `true` if the variable name is a missing node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_missing(&self) -> bool {
    self.ident.is_missing()
  }
}

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
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  str: Equivalent<T>,
  Name: Parseable<'a, I, T, Error>,
  Expr: Parseable<'a, I, T, Error>,
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
    expected_keyword("let", || SyntaxKind::let_KW.into())
      .ignore_then(Name::parser())
      .then(
        colon_eq_assign(|| SyntaxKind::ColonAssign.into())
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
pub struct MultipleVariablesDeclaration<Name, FunctionCall, Container = Vec<Name>, Lang = YUL> {
  span: Span,
  names: Container,
  fn_call: Option<FunctionCall>,
  _m: PhantomData<Lang>,
  _n: PhantomData<Name>,
}

impl<Name, FunctionCall, Container, Lang>
  MultipleVariablesDeclaration<Name, FunctionCall, Container, Lang>
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
  for MultipleVariablesDeclaration<Name, FunctionCall, Container, Lang>
where
  T: KeywordToken<'a> + PunctuatorToken<'a> + OperatorToken<'a>,
  str: Equivalent<T>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Name: Parseable<'a, I, T, Error>,
  FunctionCall: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Name> + Clone + 'a,
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
    expected_keyword("let", || SyntaxKind::let_KW.into())
      .ignore_then(
        Name::parser()
          .separated_by(comma(|| SyntaxKind::Comma.into()))
          .at_least(2)
          .collect(),
      )
      .then(
        colon_eq_assign(|| SyntaxKind::ColonAssign.into())
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum VariableDeclaration<Name, Expr, FunctionCall, Container = Vec<Name>, Lang = YUL> {
  /// A single variable declaration.
  Single(SingleVariableDeclaration<Name, Expr, Lang>),
  /// A multiple variables declaration.
  Multiple(MultipleVariablesDeclaration<Name, FunctionCall, Container, Lang>),
}

impl<'a, Name, Expr, FunctionCall, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for VariableDeclaration<Name, Expr, FunctionCall, Container, Lang>
where
  T: KeywordToken<'a> + PunctuatorToken<'a> + OperatorToken<'a>,
  str: Equivalent<T>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Name: Parseable<'a, I, T, Error>,
  Expr: Parseable<'a, I, T, Error>,
  FunctionCall: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Name> + Clone + 'a,
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

      // eat "let"
      inp.parse(expected_keyword("let", || SyntaxKind::let_KW.into()).ignored())?;

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
          () if tok.is_colon_eq_assign() => {
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
                  return Ok(Self::Multiple(MultipleVariablesDeclaration::new(
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
                  () if tok.is_colon_eq_assign() => {
                    let fn_call = inp.parse(FunctionCall::parser())?;
                    return Ok(Self::Multiple(MultipleVariablesDeclaration::new(
                      inp.span_since(&before),
                      names,
                      Some(fn_call),
                    )));
                  }
                  _ => {
                    return Err(
                      UnexpectedToken::expected_one_with_found(
                        span,
                        tok,
                        SyntaxKind::ColonAssign.into(),
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
              UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::ColonAssign.into())
                .into(),
            );
          }
        },
      })
    })
  }
}
