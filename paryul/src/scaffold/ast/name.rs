use lexsol::yul::{Yul, syntactic::SyntaxKind};
use tokit::{
  span::{AsSpan, SimpleSpan, Span},
  types::Ident,
};

/// A scaffold AST node for a Yul function call name.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Name<S, Span = SimpleSpan, Lang = Yul<SyntaxKind>> {
  ident: Ident<S, Span, Lang>,
}

impl<S, Span, Lang> From<Name<S, Span, Lang>> for Ident<S, Span, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(name: Name<S, Span, Lang>) -> Self {
    name.ident
  }
}

impl<S, Span, Lang> From<Ident<S, Span, Lang>> for Name<S, Span, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(ident: Ident<S, Span, Lang>) -> Self {
    Self::new(ident)
  }
}

impl<S, Span, Lang> AsSpan<Span> for Name<S, Span, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    self.ident.span_ref()
  }
}

impl<S, Span, Lang> Name<S, Span, Lang> {
  /// Create a new name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(ident: Ident<S, Span, Lang>) -> Self {
    Self { ident }
  }

  /// Returns the span of the name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span
  where
    Span: Copy,
  {
    self.ident.span()
  }

  /// Returns a reference to the span of the name
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_ref(&self) -> &Span {
    self.ident.span_ref()
  }

  /// Returns a mutable reference to the span of the name
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn span_mut(&mut self) -> &mut Span {
    self.ident.span_mut()
  }

  /// Get the identifier of the name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn ident(&self) -> &Ident<S, Span, Lang> {
    &self.ident
  }

  /// Consume the name and return the identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_ident(self) -> Ident<S, Span, Lang> {
    self.ident
  }
}
