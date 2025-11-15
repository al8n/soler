use core::marker::PhantomData;

#[cfg(feature = "evm")]
use lexsol::yul::EvmBuiltinFunction;
#[cfg(feature = "evm")]
use logosky::{Require, syntax::Language, utils::Spanned};

use logosky::{
  IdentifierToken, Lexed, LogoStream, Logos, PunctuatorToken, Source, Token,
  chumsky::{
    Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra, prelude::*,
  },
  error::{UnexpectedEot, UnexpectedToken},
  types::Ident,
  utils::Span,
};

use crate::{SyntaxKind, YUL};

/// A segment of a path.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment<S, Lang = YUL> {
  ident: Ident<S, Lang>,
  _lang: PhantomData<Lang>,
}

impl<S, Lang> From<Ident<S, Lang>> for PathSegment<S, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from(ident: Ident<S, Lang>) -> Self {
    Self::new(ident)
  }
}

impl<S, Lang> PathSegment<S, Lang> {
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
}

#[cfg(not(feature = "evm"))]
impl<'a, S, I, T, Lang, Error> Parseable<'a, I, T, Error> for PathSegment<S, Lang>
where
  T: IdentifierToken<'a>,
  T::Logos: Logos<'a>,
  <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
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
    logosky::chumsky::token::identifier_slice(|| SyntaxKind::Identifier.into()).map(
      |ident: Spanned<S>| {
        let (span, ident) = ident.into_components();
        PathSegment::new(Ident::new(span, ident))
      },
    )
  }
}

#[cfg(feature = "evm")]
impl<'a, S, I, T, Lang, Error> Parseable<'a, I, T, Error> for PathSegment<S, Lang>
where
  T: IdentifierToken<'a> + Require<EvmBuiltinFunction, Err = T>,
  T::Logos: Logos<'a>,
  <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
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
      let tok: Option<Lexed<'_, T>> = inp.next();
      match tok {
        None => Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
        Some(Lexed::Error(err)) => Err(err.into()),
        Some(Lexed::Token(Spanned { span, data: tok })) => {
          let ident = match tok.try_into_identifier() {
            Ok(ident) => Ident::new(span, ident),
            Err(tok) => match tok.require() {
              Ok(_) => Ident::new(span, inp.slice(&before..&inp.cursor())),
              Err(tok) => {
                return Err(
                  UnexpectedToken::expected_one_with_found(
                    span,
                    tok,
                    SyntaxKind::PathSegment.into(),
                  )
                  .into(),
                );
              }
            },
          };

          Ok(PathSegment::new(ident))
        }
      }
    })
  }
}

/// A scaffold AST node for a Yul path.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Path<Segment, Container = Vec<Segment>, Lang = YUL> {
  span: Span,
  segments: Container,
  _m: PhantomData<Segment>,
  _lang: PhantomData<Lang>,
}

impl<Segment, Container, Lang> Path<Segment, Container, Lang> {
  /// Create a new path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  const fn new(span: Span, segments: Container) -> Self {
    Self {
      span,
      segments,
      _m: PhantomData,
      _lang: PhantomData,
    }
  }

  /// Get the span of the path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
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

  /// Returns a parser for the Path with the given segment parser.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn parser<'a, I, T, Error, E>(
    segment_parser: impl Parser<'a, I, Segment, E> + Clone + 'a,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: IdentifierToken<'a> + PunctuatorToken<'a>,
    T::Logos: Logos<'a>,
    Lang: Language,
    Lang::SyntaxKind: From<SyntaxKind> + 'a,
    Segment: From<Ident<<<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>, Lang>>,
    Error: From<<T::Logos as Logos<'a>>::Error>
      + From<UnexpectedToken<'a, T, Lang::SyntaxKind>>
      + From<UnexpectedEot>
      + 'a,
    Container: ChumskyContainer<Segment>,
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    custom(move |inp| {
      let before = inp.cursor();

      let tok: Option<Lexed<'_, T>> = inp.next();
      let seg = match tok {
        None => return Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
        Some(Lexed::Error(err)) => return Err(err.into()),
        Some(Lexed::Token(Spanned { span, data: tok })) => {
          let ident = match tok.try_into_identifier() {
            Ok(ident) => Ident::new(span, ident),
            Err(tok) => {
              return Err(
                UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::Identifier.into())
                  .into(),
              );
            }
          };

          Segment::from(ident)
        }
      };

      let mut segments = Container::default();
      segments.push(seg);

      loop {
        let tok: Option<Lexed<'_, T>> = inp.peek();
        match tok {
          Some(Lexed::Token(Spanned { data: tok, .. })) => {
            // if the next token is not a dot, return the path.
            if !tok.is_dot() {
              return Ok(Path::new(inp.span_since(&before), segments));
            }

            // consume the dot as we are building a AST not CST.
            inp.skip();

            segments.push(inp.parse(segment_parser.clone())?);
          }
          Some(Lexed::Error(_)) | None => return Ok(Path::new(inp.span_since(&before), segments)),
        }
      }
    })
  }
}

impl<'a, Segment, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for Path<Segment, Container, Lang>
where
  T: IdentifierToken<'a> + PunctuatorToken<'a>,
  T::Logos: Logos<'a>,
  Segment: Parseable<'a, I, T, Error>
    + From<Ident<<<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>, Lang>>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Error: From<<T::Logos as Logos<'a>>::Error>
    + From<UnexpectedToken<'a, T, Lang::SyntaxKind>>
    + From<UnexpectedEot>
    + 'a,
  Container: ChumskyContainer<Segment>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    Self::parser(Segment::parser())
  }
}
