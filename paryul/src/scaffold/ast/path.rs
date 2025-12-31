use core::marker::PhantomData;

#[cfg(feature = "evm")]
use lexsol::yul::EvmBuiltinFunction;
#[cfg(feature = "evm")]
use tokit::{
  Emitter, Lexer, ParseContext, Require, TryParseInput,
  lexer::InputRef,
  utils::Maybe::{Owned, Ref},
};

use tokit::{
  Source,
  error::UnexpectedEot,
  lexer::IdentifierToken,
  types::Ident,
  utils::{AsSpan, SimpleSpan},
};

use lexsol::yul::{
  Yul,
  syntactic::{SyntaxKind, Token},
};

/// A segment of a path of Yul.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment<S, Span = SimpleSpan, Lang: ?Sized = Yul<SyntaxKind>> {
  ident: Ident<S, Span, Lang>,
  _lang: PhantomData<Lang>,
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for PathSegment<S, Span, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    self.ident.span_ref()
  }
}

impl<S, Span, Lang: ?Sized> From<Ident<S, Span, Lang>> for PathSegment<S, Span, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(ident: Ident<S, Span, Lang>) -> Self {
    Self::new(ident)
  }
}

impl<S, Span, Lang: ?Sized> PathSegment<S, Span, Lang> {
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
  pub const fn span(&self) -> Span
  where
    Span: Copy,
  {
    self.ident.span()
  }

  /// Returns the reference to the span of the path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_ref(&self) -> &Span {
    self.ident.span_ref()
  }

  /// Returns the mutable reference to the span of the path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_mut(&mut self) -> &mut Span {
    self.ident.span_mut()
  }

  /// Bumps the span of the path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn bump(&mut self, by: &Span::Offset) -> &mut Self
  where
    Span: tokit::lexer::Span,
  {
    self.ident.span_mut().bump(by);
    self
  }

  /// Get the identifier of the path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn ident(&self) -> &Ident<S, Span, Lang> {
    &self.ident
  }

  /// Consume the path segment and return the identifier.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn into_components(self) -> Ident<S, Span, Lang> {
    self.ident
  }

  /// Returns `true` if the path segment is a valid path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_valid(&self) -> bool {
    self.ident.is_valid()
  }

  /// Returns `true` if the path segment is an error node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_error(&self) -> bool {
    self.ident.is_error()
  }

  /// Returns `true` if the path segment is a missing node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn is_missing(&self) -> bool {
    self.ident.is_missing()
  }
}

impl<'inp, L, Ctx, Lang>
  TryParseInput<
    'inp,
    L,
    PathSegment<<L::Source as Source<L::Offset>>::Slice<'inp>, L::Span, Lang>,
    Ctx,
    Lang,
  > for PathSegment<(), (), ()>
where
  L: Lexer<'inp>,
  L::Source: Source<L::Offset>,
  L::Token: IdentifierToken<'inp>
    + Require<EvmBuiltinFunction<<L::Source as Source<L::Offset>>::Slice<'inp>>, Err = L::Token>,
  Ctx: ParseContext<'inp, L, Lang>,
  <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error: From<UnexpectedEot<L::Offset, Lang>>,
  Lang: ?Sized,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn try_parse_input(
    &mut self,
    inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ) -> Result<
    Option<PathSegment<<L::Source as Source<L::Offset>>::Slice<'inp>, L::Span, Lang>>,
    <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error,
  > {
    Self::try_parse_of(inp)
  }
}

impl PathSegment<(), ()> {
  /// A parser for the PathSegment for Yul.
  ///
  /// The parser will not consume any valid token if it is not a valid path segment.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_parse<'inp, L, Ctx>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
  ) -> Result<
    Option<PathSegment<<L::Source as Source<L::Offset>>::Slice<'inp>, L::Span, Yul<SyntaxKind>>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error,
  >
  where
    L: Lexer<'inp>,
    L::Source: Source<L::Offset>,
    L::Token: IdentifierToken<'inp>
      + Require<EvmBuiltinFunction<<L::Source as Source<L::Offset>>::Slice<'inp>>, Err = L::Token>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>,
  {
    PathSegment::try_parse_of(inp)
  }
}

impl PathSegment<(), (), ()> {
  /// A parser for the PathSegment for any language.
  ///
  /// The parser will not consume any valid token if it is not a valid path segment.
  #[cfg(feature = "evm")]
  pub fn try_parse_of<'inp, L, Ctx, Lang>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ) -> Result<
    Option<PathSegment<<L::Source as Source<L::Offset>>::Slice<'inp>, L::Span, Lang>>,
    <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error,
  >
  where
    L: Lexer<'inp>,
    L::Source: Source<L::Offset>,
    L::Token: IdentifierToken<'inp>
      + Require<EvmBuiltinFunction<<L::Source as Source<L::Offset>>::Slice<'inp>>, Err = L::Token>,
    Ctx: ParseContext<'inp, L, Lang>,
    <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error: From<UnexpectedEot<L::Offset, Lang>>,
    Lang: ?Sized,
  {
    let end = inp.cursor().as_inner().clone();
    let tok = inp.sync_until_token()?;
    match tok {
      None => Err(UnexpectedEot::eot_of(end).into()),
      Some(ct) => Ok(match ct {
        Owned(ct) => {
          let (span, tok) = ct.into_token().into_components();
          if tok.is_identifier() {
            inp.skip_one();
            return Ok(Some(PathSegment::new(Ident::new(span, inp.slice()))));
          }

          if let Ok(ebf) = tok.require() {
            inp.skip_one();
            return Ok(Some(PathSegment::new(Ident::new(span, ebf.into_inner()))));
          }

          None
        }
        Ref(ct) => {
          let (span, tok) = ct.into_token().into_components();

          if !(tok.is_identifier() || tok.matched()) {
            return Ok(None);
          }

          let span = span.clone();
          inp.skip_one();

          Some(PathSegment::new(Ident::new(span, inp.slice())))
        }
      }),
    }
  }

  /// Returns a parser for the PathSegment.
  ///
  /// The parser will not consume any valid token if it is not a valid path segment.
  #[cfg(not(feature = "evm"))]
  pub fn try_parse_of<'inp, L, Ctx, Lang>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  ) -> Result<
    Option<PathSegment<<L::Source as Source<L::Offset>>::Slice<'inp>, L::Span, Lang>>,
    <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error,
  >
  where
    L: Lexer<'inp>,
    L::Source: Source<L::Offset>,
    L::Token: IdentifierToken<'inp>,
    Ctx: ParseContext<'inp, L, Lang>,
    <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error: From<UnexpectedEot<L::Offset, Lang>>,
    Lang: ?Sized,
  {
    Ident::try_parse_of(inp).map(|ident| ident.map(PathSegment::new))
  }
}

/// A scaffold AST node for a Yul path.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Path<Segment, Span = SimpleSpan, Container = Vec<Segment>, Lang = Yul<SyntaxKind>> {
  span: Span,
  segments: Container,
  _m: PhantomData<Segment>,
  _lang: PhantomData<Lang>,
}

impl<Segment, Span, Container, Lang> AsSpan<Span> for Path<Segment, Span, Container, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    self.span_ref()
  }
}

impl<S, Span, Container, Lang> Path<PathSegment<S, Span, Lang>, Span, Container, Lang> {
  /// Returns `true` if all segments in the path are valid.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_valid(&self) -> bool
  where
    Container: AsRef<[PathSegment<S, Span, Lang>]>,
  {
    self.segments.as_ref().iter().all(|seg| seg.is_valid())
  }

  /// Returns `true` if any segment in the path is an error node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_error(&self) -> bool
  where
    Container: AsRef<[PathSegment<S, Span, Lang>]>,
  {
    self.segments.as_ref().iter().any(|seg| seg.is_error())
  }

  /// Returns `true` if any segment in the path is a missing node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_missing(&self) -> bool
  where
    Container: AsRef<[PathSegment<S, Span, Lang>]>,
  {
    self.segments.as_ref().iter().any(|seg| seg.is_missing())
  }
}

impl<Segment, Span, Container, Lang> Path<Segment, Span, Container, Lang> {
  /// Create a new path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, segments: Container) -> Self {
    Self {
      span,
      segments,
      _m: PhantomData,
      _lang: PhantomData,
    }
  }

  /// Get the span of the path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span
  where
    Span: Copy,
  {
    self.span
  }

  /// Get the reference to the span of the path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_ref(&self) -> &Span {
    &self.span
  }

  /// Get the mutable reference to the span of the path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Get the segments of the path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn segments(&self) -> &Container {
    &self.segments
  }

  /// Returns the slice of the path segments.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn segments_slice(&self) -> &[Segment]
  where
    Container: AsRef<[Segment]>,
  {
    self.segments.as_ref()
  }

  /// Returns `true` if the path has no segments.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_empty(&self) -> bool
  where
    Container: AsRef<[Segment]>,
  {
    self.segments.as_ref().is_empty()
  }
}

impl<S, Span, Container> Path<PathSegment<S, Span>, Span, Container> {
  /// Returns a parser for the Yul path.
  pub fn yul<'inp, L, Ctx>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
  ) -> Result<Option<Self>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
  where
    L: Lexer<'inp, Span = Span, Token = Token<S>>,
    L::Source: Source<L::Offset, Slice<'inp> = S>,
    L::Token: IdentifierToken<'inp>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>,
    Container: Default + tokit::container::Container<PathSegment<S, Span>>,
  {
    let cursor = inp.cursor().clone();
    let leading = Ident::try_parse_of(inp)?;

    match leading {
      None => Ok(None),
      Some(l) => {
        let mut container = Container::default();
        container.push(PathSegment::new(l));
        loop {
          let seg = PathSegment::try_parse_of(inp)?;
          match seg {
            None => return Ok(Some(Path::new(inp.span_since(&cursor), container))),
            Some(seg) => {
              container.push(seg);
            }
          }
        }
      }
    }
  }
}
