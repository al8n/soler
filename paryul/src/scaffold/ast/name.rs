use core::marker::PhantomData;

use logosky::{
  types::Ident,
  utils::{AsSpan, Span},
};

use crate::YUL;

/// A scaffold AST node for a Yul function call name.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Name<S, Lang = YUL> {
  ident: Ident<S, Lang>,
  _lang: PhantomData<Lang>,
}

impl<S, Lang> From<Name<S, Lang>> for Ident<S, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(name: Name<S, Lang>) -> Self {
    name.ident
  }
}

impl<S, Lang> From<Ident<S, Lang>> for Name<S, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(ident: Ident<S, Lang>) -> Self {
    Self::new(ident)
  }
}

impl<S, Lang> AsSpan<Span> for Name<S, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    self.ident.span_ref()
  }
}

impl<S, Lang> Name<S, Lang> {
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

  /// Consume the name and return the identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_components(self) -> (Span, Ident<S, Lang>) {
    (self.ident.span(), self.ident)
  }
}
