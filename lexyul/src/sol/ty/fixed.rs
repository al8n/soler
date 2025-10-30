use logosky::utils::Span;

/// The kind of fixed point type, either `fixed` or `ufixed`
/// 
/// Spec: [Fixed Point Numbers](https://docs.soliditylang.org/en/latest/types.html#fixed-point-numbers)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum PrefixKind {
  /// Fixed point
  Fixed,
  /// Unsigned fixed point
  UFixed,
}

/// The prefix part of fixed point numbers type (`fixed` or `ufixed`)
/// 
/// Spec: [Fixed Point Numbers](https://docs.soliditylang.org/en/latest/types.html#fixed-point-numbers)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Prefix {
  span: Span,
  kind: PrefixKind,
}

impl Prefix {
  /// Create a new `fixed` prefix
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn fixed(span: Span) -> Self {
    Self {
      span,
      kind: PrefixKind::Fixed,
    }
  }

  /// Create a new `ufixed` prefix
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn ufixed(span: Span) -> Self {
    Self {
      span,
      kind: PrefixKind::UFixed,
    }
  }

  /// Get the kind of the prefix
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn kind(&self) -> PrefixKind {
    self.kind
  }
}


/// The 'M' part of fixed type
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct M<S> {
  span: Span,
  source: S,
}

impl<S> M<S> {
  /// Create a new 'M' part
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, source: S) -> Self {
    Self { span, source }
  }

  /// Returns the span of the 'M' part
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the source of the 'M' part
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.source
  }

  /// Returns the source of the 'M' part
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.source
  }
}

/// The 'N' part of fixed type
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct N<S> {
  span: Span,
  source: S,
}

impl<S> N<S> {
  /// Create a new 'N' part
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, source: S) -> Self {
    Self { span, source }
  }

  /// Returns the span of the 'N' part
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the source of the 'N' part
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.source
  }

  /// Returns the source of the 'N' part
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.source
  }
}

/// The fixed type
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Fixed<S> {
  span: Span,
  source: S,
  prefix: Prefix,
  m: M<S>,
  n: N<S>,
}

impl<S> Fixed<S> {
  /// Create a new fixed type
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, source: S, prefix: Prefix, m: M<S>, n: N<S>) -> Self {
    Self { span, source, prefix, m, n }
  }

  /// Returns the span of the fixed type
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the source of the fixed type
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source_ref(&self) -> &S {
    &self.source
  }

  /// Returns the source of the fixed type
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn source(self) -> S
  where
    S: Copy,
  {
    self.source
  }

  /// Returns the prefix part of the fixed type
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn prefix(&self) -> &Prefix {
    &self.prefix
  }

  /// Returns the 'M' part of the fixed type, which represents the number of bits taken by the type.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn bits(&self) -> &M<S> {
    &self.m
  }

  /// Returns the 'N' part of the fixed type, which represents how many decimal points are available.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn points(&self) -> &N<S> {
    &self.n
  }
}
