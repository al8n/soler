use tokit::{
  Accumulator, Emitter, Lexer, ParseContext, ParseInput, Source, TryParseInput,
  emitter::{
    SeparatedEmitter, UnexpectedLeadingSeparatorEmitter, UnexpectedTrailingSeparatorEmitter,
  },
  error::UnexpectedEot,
  input::InputRef,
  parser::SeparatorHandler,
  punct::Dot,
  token::IdentifierToken,
  try_parse_input::{Accept, Decline, ParseAttempt},
  types::Ident,
};

use lexsol::yul::{
  Yul,
  syntactic::{SyntaxKind, Token},
};

use crate::{
  error::InvalidPathSegment,
  scaffold::ast::path::{Path, PathSegment},
};

impl<S, Span> PathSegment<S, Span> {
  /// Returns a parser for the Yul leading path segment.
  pub fn try_yul_leading<'inp, L, Ctx>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
  ) -> Result<ParseAttempt<Self>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
  where
    L: Lexer<'inp, Span = Span, Token = Token<S>>,
    L::Source: Source<L::Offset, Slice<'inp> = S>,
    L::Token: IdentifierToken<'inp>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>,
    S: 'inp,
    Span: tokit::Span<Offset = L::Offset>,
  {
    Self::try_yul(
      inp,
      #[cfg(feature = "evm")]
      false,
    )
  }

  /// Returns a parser for the Yul following path segment.
  pub fn try_yul_following<'inp, L, Ctx>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
  ) -> Result<ParseAttempt<Self>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
  where
    L: Lexer<'inp, Span = Span, Token = Token<S>>,
    L::Source: Source<L::Offset, Slice<'inp> = S>,
    L::Token: IdentifierToken<'inp>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>,
    S: 'inp,
    Span: tokit::Span<Offset = L::Offset>,
  {
    Self::try_yul(
      inp,
      #[cfg(feature = "evm")]
      true,
    )
  }

  /// Returns a parser for the Yul path.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn try_yul<'inp, L, Ctx>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
    #[cfg(feature = "evm")] allow_evm_builtin: bool,
  ) -> Result<ParseAttempt<Self>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
  where
    L: Lexer<'inp, Span = Span, Token = Token<S>>,
    L::Source: Source<L::Offset, Slice<'inp> = S>,
    L::Token: IdentifierToken<'inp>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>,
    S: 'inp,
    Span: tokit::Span<Offset = L::Offset>,
  {
    let tok = inp.try_expect_valid(|tok, _| {
      Ok(match tok.into_data() {
        Token::Identifier(_) => true,
        #[cfg(feature = "evm")]
        Token::EvmBuiltin(_) if allow_evm_builtin => true,
        _ => false,
      })
    })?;

    match tok {
      None => Ok(Decline),
      Some(t) => {
        let (span, tok) = t.into_components();

        Ok(Accept(match tok {
          Token::Identifier(ident) => PathSegment::new(Ident::new(span, ident)),
          #[cfg(feature = "evm")]
          Token::EvmBuiltin(evm_fn) if allow_evm_builtin => {
            PathSegment::new(Ident::new(span, evm_fn.into_inner()))
          }
          _ => unreachable!("token has been validated"),
        }))
      }
    }
  }
}

impl<S, Span, Container> Path<PathSegment<S, Span>, Span, Container> {
  /// Returns a parser for the Yul path.
  pub fn try_yul<'inp, L, Ctx>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
  ) -> Result<ParseAttempt<Self>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
  where
    L: Lexer<'inp, Span = Span, Token = Token<S>>,
    L::Source: Source<L::Offset, Slice<'inp> = S>,
    L::Token: IdentifierToken<'inp>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    Ctx::Emitter: SeparatedEmitter<'inp, Dot, L, Yul<SyntaxKind>>
      + UnexpectedLeadingSeparatorEmitter<'inp, Dot, L, Yul<SyntaxKind>>
      + UnexpectedTrailingSeparatorEmitter<'inp, Dot, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error: From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>
      + From<InvalidPathSegment<L::Span, Yul<SyntaxKind>>>,
    Container:
      Default + tokit::container::Container<PathSegment<S, Span>> + SeparatorHandler<'inp, L>,
    S: 'inp,
    Span: tokit::Span<Offset = L::Offset> + Clone,
  {
    let leading = PathSegment::try_yul_leading(inp)?;

    match leading {
      Decline => Ok(Decline),
      Accept(l) => {
        let start = l.span_ref().start();
        let mut container = Container::default();
        container.push(l);

        PathSegment::try_yul_following
          .separated_by_dot()
          .collect_with(container)
          .spanned()
          .parse_input(inp)
          .map(|seg| {
            let (span, container) = seg.into_components();
            let span = Span::new(start, span.into_end());
            Accept(Path::new(span, container))
          })
      }
    }
  }
}
