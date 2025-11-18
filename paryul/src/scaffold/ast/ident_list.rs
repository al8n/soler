use core::marker::PhantomData;

#[cfg(feature = "evm")]
use lexsol::yul::EvmBuiltinFunction;
#[cfg(feature = "evm")]
use logosky::{Require, syntax::Language, utils::Spanned};

use logosky::{
  IdentifierToken, Lexed, LogoStream, Logos, PunctuatorToken, Source, Token,
  chumsky::{
    Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra, prelude::*,
    token::punct::comma,
  },
  error::{UnexpectedEot, UnexpectedToken},
  types::Ident,
  utils::{AsSpan, Span},
};

use crate::{SyntaxKind, YUL};

/// A scaffold AST node for a Yul identifier list.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct IdentList<Ident, Container = Vec<Ident>, Lang = YUL> {
  span: Span,
  identifiers: Container,
  _m: PhantomData<Ident>,
  _lang: PhantomData<Lang>,
}

impl<Ident, Container, Lang> AsSpan<Span> for IdentList<Ident, Container, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    self.span_ref()
  }
}

impl<S, Container, Lang> IdentList<Ident<S, Lang>, Container, Lang> {
  /// Returns `true` if all identifiers in the path are valid.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_valid(&self) -> bool
  where
    Container: AsRef<[Ident<S, Lang>]>,
  {
    self.identifiers.as_ref().iter().all(|seg| seg.is_valid())
  }

  /// Returns `true` if any segment in the path is an error node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_error(&self) -> bool
  where
    Container: AsRef<[Ident<S, Lang>]>,
  {
    self.identifiers.as_ref().iter().any(|seg| seg.is_error())
  }

  /// Returns `true` if any segment in the path is a missing node.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_missing(&self) -> bool
  where
    Container: AsRef<[Ident<S, Lang>]>,
  {
    self.identifiers.as_ref().iter().any(|seg| seg.is_missing())
  }
}

impl<Ident, Container, Lang> IdentList<Ident, Container, Lang> {
  /// Create a new path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, identifiers: Container) -> Self {
    Self {
      span,
      identifiers,
      _m: PhantomData,
      _lang: PhantomData,
    }
  }

  /// Get the span of the path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
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

  /// Get the identifiers of the path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn identifiers(&self) -> &Container {
    &self.identifiers
  }

  /// Returns the slice of the path identifiers.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn identifiers_slice(&self) -> &[Ident]
  where
    Container: AsRef<[Ident]>,
  {
    self.identifiers.as_ref()
  }

  /// Returns `true` if the path has no identifiers.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn is_empty(&self) -> bool
  where
    Container: AsRef<[Ident]>,
  {
    self.identifiers.as_ref().is_empty()
  }

  /// Returns a parser for the IdentList with the given segment parser.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn parser<'a, 'e: 'a, I, T, E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    T: IdentifierToken<'a> + PunctuatorToken<'a>,
    T::Logos: Logos<'a>,
    Lang: Language,
    Lang::SyntaxKind: From<SyntaxKind> + 'e,
    Ident:
      From<logosky::types::Ident<<<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>, Lang>>,
    E::Error: From<<T::Logos as Logos<'a>>::Error>
      + From<UnexpectedToken<'e, T, Lang::SyntaxKind>>
      + From<UnexpectedEot>
      + 'a,
    Container: ChumskyContainer<Ident>,
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    E: ParserExtra<'a, I> + 'a,
  {
    any()
      .try_map(|tok: Lexed<'_, T>, _| match tok {
        Lexed::Error(err) => Err(E::Error::from(err)),
        Lexed::Token(Spanned { span, data: tok }) => {
          let ident = match tok.try_into_identifier() {
            Ok(ident) => logosky::types::Ident::new(span, ident),
            Err(tok) => {
              return Err(
                UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::Identifier.into())
                  .into(),
              );
            }
          };

          Ok(Ident::from(ident))
        }
      })
      .separated_by(comma(|| SyntaxKind::Comma.into()))
      .at_least(1)
      .collect()
      .map_with(|identifiers, exa| Self::new(exa.span(), identifiers))
  }
}

impl<'a, Ident, Container, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for IdentList<Ident, Container, Lang>
where
  T: IdentifierToken<'a> + PunctuatorToken<'a>,
  T::Logos: Logos<'a>,
  Ident: From<logosky::types::Ident<<<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>, Lang>>,
  Lang: Language,
  Lang::SyntaxKind: From<SyntaxKind> + 'a,
  Error: From<<T::Logos as Logos<'a>>::Error>
    + From<UnexpectedToken<'a, T, Lang::SyntaxKind>>
    + From<UnexpectedEot>
    + 'a,
  Container: ChumskyContainer<Ident>,
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
    Self::parser()
  }
}
