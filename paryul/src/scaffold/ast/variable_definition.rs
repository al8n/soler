use core::marker::PhantomData;

use lexsol::yul::YUL;
use logosky::{
  KeywordToken, LogoStream, Logos, Source, Token,
  chumsky::{
    Parseable, Parser, extra::ParserExtra, keyword,
  },
  error::UnexpectedToken,
  utils::Span,
};

use crate::SyntaxKind;

/// A scaffold AST for a single variable definition in Yul.
/// 
/// ## Yul Syntax Examples
/// 
/// ```yul
/// let x := 42
/// let y
/// ```
/// 
/// See [Yul variable definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDefinition)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SingleVariableDefinition<Name, Expr, Lang = YUL> {
  span: Span,
  name: Name,
  expr: Option<Expr>,
  _m: PhantomData<Lang>,
}

impl<Name, Expr, Lang> SingleVariableDefinition<Name, Expr, Lang> {
  /// Create a new single variable definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, name: Name, expr: Option<Expr>) -> Self {
    Self {
      span,
      name,
      expr,
      _m: PhantomData,
    }
  }

  /// Get the span of the single variable definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Get the name of the single variable definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Get the expression of the single variable definition.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn expr(&self) -> Option<&Expr> {
    self.expr.as_ref()
  }
}

impl<'a, Name, Expr, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for SingleVariableDefinition<Name, Expr, Lang>
where
  T: KeywordToken<'a>,
  Name: Parseable<'a, I, T, Error>,
  Expr: Parseable<'a, I, T, Error>,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    let let_kw = keyword::<_, T>("let");
    let assign_op = keyword::<_, T>(":=");
    
    let name_parser = Name::parser();
    let expr_parser = Expr::parser();

    let variable_with_expr = 
      name_parser
        .then(assign_op.ignore_then(expr_parser).or_not())
        .map_with(|(name, expr), exa| {
          Self::new(exa.span(), name, expr)
        });

    let variable_without_expr = 
      name_parser
        .map_with(|name, exa| {
          Self::new(exa.span(), name, None)
        });

    let_kw.ignore_then(
      variable_with_expr.or(variable_without_expr)
    )
  }
}
