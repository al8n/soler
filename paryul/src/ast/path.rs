use tokit::{
  Accumulator, Emitter, Lexer, ParseContext, ParseInput, Source, TryParseInput,
  emitter::{
    SeparatedEmitter, UnexpectedLeadingSeparatorEmitter, UnexpectedTrailingSeparatorEmitter,
  },
  error::UnexpectedEot,
  input::InputRef,
  parser::SeparatorHandler,
  punct::Dot,
  span::Spanned,
  token::IdentifierToken,
  try_parse_input::{Accept, Decline, ParseAttempt},
  types::Ident,
};

use lexsol::yul::{
  Yul,
  syntactic::{SyntaxKind, Token},
};

use crate::{error::{InvalidPathSegment, InvalidPathSegmentData}, scaffold::ast::path::{Path, PathSegment}};


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
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>
      + From<InvalidPathSegment<L::Span, Yul<SyntaxKind>>>,
    S: 'inp,
    Span: tokit::Span<Offset = L::Offset> + Clone,
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
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>
      + From<InvalidPathSegment<L::Span, Yul<SyntaxKind>>>,
    S: 'inp,
    Span: tokit::Span<Offset = L::Offset> + Clone,
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
    #[cfg(feature = "evm")]
    allow_evm_builtin: bool,
  ) -> Result<ParseAttempt<Self>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
  where
    L: Lexer<'inp, Span = Span, Token = Token<S>>,
    L::Source: Source<L::Offset, Slice<'inp> = S>,
    L::Token: IdentifierToken<'inp>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>
      + From<InvalidPathSegment<L::Span, Yul<SyntaxKind>>>,
    S: 'inp,
    Span: tokit::Span<Offset = L::Offset> + Clone,
  {
    let tok = inp.try_expect_valid(|tok, emitter| {
      #[cfg_attr(not(tarpaulin), inline(always))]
      fn invalid_path_segment<Span>(span: Span, data: InvalidPathSegmentData,) -> InvalidPathSegment<Span> {
        InvalidPathSegment::with_data_of(span, data)
      }

      let (span, tok) = tok.into_components();

      macro_rules! emit_ret {
        ($data:expr) => {{
          emitter.emit_error(Spanned::new(span.clone(), invalid_path_segment(span.clone(), $data).into()))?;
          true
        }};
        (@kw($name:literal)) => {
          emit_ret!(InvalidPathSegmentData::Keyword($name))
        };
        (@lit_bool($val:expr)) => {
          emit_ret!(InvalidPathSegmentData::LitBool($val))
        };
        (@evm_builtin($val:expr)) => {
          emit_ret!(InvalidPathSegmentData::EvmBuiltinFunction($val))
        };
      }

      Ok(match tok {
        Token::Identifier(_) => true,
        Token::Leave => emit_ret!(@kw("leave")), 
        Token::Continue => emit_ret!(@kw("continue")),
        Token::Break => emit_ret!(@kw("break")),
        Token::Switch => emit_ret!(@kw("switch")),
        Token::Case => emit_ret!(@kw("case")),
        Token::Default => emit_ret!(@kw("default")),
        Token::Function => emit_ret!(@kw("function")),
        Token::Let => emit_ret!(@kw("let")),
        Token::If => emit_ret!(@kw("if")),
        Token::For => emit_ret!(@kw("for")),
        Token::Lit(lexsol::yul::Lit::Boolean(lit)) => emit_ret!(@lit_bool(lit.unit())),
        #[cfg(feature = "evm")]
        Token::EvmBuiltin(e) => if allow_evm_builtin {
          true
        } else {
          emit_ret!(@evm_builtin(e.unit()))
        },
        _ => false,
      })
    })?;

    match tok {
      None => Err(UnexpectedEot::eot_of(inp.span().end()).into()),
      Some(t) => {
        let (span, tok) = t.into_components();
        
        Ok(Accept(match tok {
          Token::Identifier(ident) => PathSegment::new(Ident::new(span, ident)),
          Token::Leave | Token::Continue | Token::Break | Token::Switch | Token::Case
          | Token::Default | Token::Function | Token::Let | Token::If | Token::For => {
            let mut ident = Ident::new(span, inp.slice());
            ident.mark_error();
            PathSegment::new(ident)
          }
          #[cfg(feature = "evm")]
          Token::EvmBuiltin(evm_fn) => {
            let mut ident = Ident::new(span, evm_fn.into_inner());
            if allow_evm_builtin {
              PathSegment::new(ident)
            } else {
              ident.mark_error();
              PathSegment::new(ident)
            }
          },
          Token::Lit(lit) => {
            let mut ident = Ident::new(span, lit.into_data());
            ident.mark_error();
            PathSegment::new(ident)
          },
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
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>
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
