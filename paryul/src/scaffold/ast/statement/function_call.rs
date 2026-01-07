use core::marker::PhantomData;

use lexsol::yul::{Yul, syntactic::SyntaxKind};
use tokit::{
  SimpleSpan, span::AsSpan, types::Ident
};


/// A scaffold AST node for a Yul function call name.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionName<S, Span = SimpleSpan, Lang: ?Sized = Yul<SyntaxKind>> {
  ident: Ident<S, Span, Lang>,
  _lang: PhantomData<Lang>,
}

impl<S, Span, Lang> From<Ident<S, Span, Lang>> for FunctionName<S, Span, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(ident: Ident<S, Span, Lang>) -> Self {
    Self::new(ident)
  }
}

impl<S, Span, Lang> FunctionName<S, Span, Lang> {
  /// Create a new path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(ident: Ident<S, Span, Lang>) -> Self {
    Self {
      ident,
      _lang: PhantomData,
    }
  }

  /// Returns the span of the path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span where Span: Copy {
    self.ident.span()
  }

  /// Get the identifier of the path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn ident(&self) -> &Ident<S, Span, Lang> {
    &self.ident
  }

  /// Consume the name and return the span and identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_ident(self) -> Ident<S, Span, Lang> {
    self.ident
  }

  /// Returns `true` if the function name is a valid function name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_valid(&self) -> bool {
    self.ident.is_valid()
  }

  /// Returns `true` if the function name is an error node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_error(&self) -> bool {
    self.ident.is_error()
  }

  /// Returns `true` if the function name is a missing node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_missing(&self) -> bool {
    self.ident.is_missing()
  }
}

/// A scaffold AST node for a Yul function call.
///
/// See: [Yul function call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionCall<Name, Expression, Span = SimpleSpan, Container = Vec<Expression>, Lang: ?Sized = Yul<SyntaxKind>> {
  span: Span,
  name: Name,
  expressions: Container,
  _m: PhantomData<Expression>,
  _lang: PhantomData<Lang>,
}

impl<Name, Expression, Span, Container, Lang: ?Sized> AsSpan<Span>
  for FunctionCall<Name, Expression, Span, Container, Lang>
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    self.span_ref()
  }
}

impl<Name, Expression, Span, Container, Lang: ?Sized> FunctionCall<Name, Expression, Span, Container, Lang> {
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
  pub const fn span(&self) -> Span where Span: Copy {
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
}
