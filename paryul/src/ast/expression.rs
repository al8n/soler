#[cfg(feature = "evm")]
use lexsol::yul::EvmBuiltinFunction;
use tokit::{
  Accumulator, Emitter, Lexer, ParseContext, ParseInput, ParseState, SimpleSpan, Source,
  Token as TokenT, TryParseInput,
  emitter::{
    DelimitedEmitter, SeparatedEmitter, UnexpectedLeadingSeparatorEmitter,
    UnexpectedTrailingSeparatorEmitter,
  },
  error::{UnexpectedEot, token::UnexpectedTrailingDot},
  input::InputRef,
  punct::{Brace, Comma, Dot},
  span::{AsSpan, Spanned},
  token::DelimiterToken,
  try_parse_input::{Accept, Decline, ParseAttempt},
  types::Ident,
};

use lexsol::yul::{
  Lit, Yul,
  syntactic::{SyntaxKind, Token},
};

use crate::error::{InvalidPathSegment, InvalidPathSegmentData};

use super::*;

/// The expression type for Yul.
///
/// Spec: [Yul Expression](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulExpression)
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Expression<S, Span = SimpleSpan, Lang: ?Sized = DefaultLang> {
  /// Yul path
  Path(Path<S, Span, Lang>),
  /// Yul function call
  FunctionCall(FunctionCall<S, Span, Lang>),
  /// Yul literal
  Literal(Spanned<Lit<S>, Span>),
}

impl<S, Span, Lang: ?Sized> AsSpan<Span> for Expression<S, Span, Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    match self {
      Self::Path(path) => path.as_span(),
      Self::FunctionCall(fn_call) => fn_call.as_span(),
      Self::Literal(lit) => lit.as_span(),
    }
  }
}

impl<S, Span> Expression<S, Span> {
  /// Returns a parser for the Yul path.
  pub fn try_parse_yul<'inp, L, Ctx>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
  ) -> Result<ParseAttempt<Self>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
  where
    L: Lexer<'inp, Span = Span, Token = Token<S>>,
    L::Source: Source<L::Offset, Slice<'inp> = S>,
    <L::Token as TokenT<'inp>>::Kind: From<SyntaxKind>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    Ctx::Emitter: DelimitedEmitter<'inp, Brace, L, Yul<SyntaxKind>>
      + SeparatedEmitter<'inp, Comma, L, Yul<SyntaxKind>>
      + UnexpectedLeadingSeparatorEmitter<'inp, Comma, L, Yul<SyntaxKind>>
      + UnexpectedTrailingSeparatorEmitter<'inp, Comma, L, Yul<SyntaxKind>>
      + SeparatedEmitter<'inp, Dot, L, Yul<SyntaxKind>>
      + UnexpectedLeadingSeparatorEmitter<'inp, Dot, L, Yul<SyntaxKind>>
      + UnexpectedTrailingSeparatorEmitter<'inp, Dot, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error: From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>
      + From<InvalidPathSegment<L::Span, Yul<SyntaxKind>>>,
    S: 'inp,
    Token<S>: DelimiterToken<'inp>,
    Span: tokit::Span<Offset = L::Offset> + Clone,
  {
    enum Hint<S> {
      Ident(S),
      #[cfg(feature = "evm")]
      EvmBuiltin(EvmBuiltinFunction<S>),
    }

    impl<S> Hint<S> {
      #[cfg_attr(not(tarpaulin), inline(always))]
      fn into_data(self) -> S {
        match self {
          Self::Ident(ident) => ident,
          #[cfg(feature = "evm")]
          Self::EvmBuiltin(f) => f.into_inner(),
        }
      }
    }

    match inp.try_expect_valid(|t, _| match t.into_data() {
      Token::Identifier(_) => Ok(true),
      #[cfg(feature = "evm")]
      Token::EvmBuiltin(_) => Ok(true),
      Token::Lit(_) => Ok(true),
      _ => Ok(false),
    })? {
      None => Ok(Decline),
      Some(t) => {
        let (first_span, tok) = t.into_components();
        let hint = match tok {
          Token::Lit(lit) => return Ok(Accept(Self::Literal(Spanned::new(first_span, lit)))),
          Token::Identifier(ident) => Hint::Ident(ident),
          #[cfg(feature = "evm")]
          Token::EvmBuiltin(f) => Hint::EvmBuiltin(f),
          _ => unreachable!("token has been validated"),
        };

        let ct = inp.try_expect_valid(|t, _| match t.into_data() {
          Token::Dot | Token::LParen => Ok(true),
          _ => Ok(false),
        })?;

        match ct {
          None => match hint {
            Hint::Ident(ident) => {
              let path_segment = PathSegment::new(Ident::new(first_span.clone(), ident));
              let path = Path::new(first_span, vec![path_segment]);
              Ok(Accept(Self::Path(path)))
            }
            #[cfg(feature = "evm")]
            Hint::EvmBuiltin(e) => Err(
              InvalidPathSegment::with_data_of(
                first_span,
                InvalidPathSegmentData::EvmBuiltinFunction(e.unit()),
              )
              .into(),
            ),
          },
          Some(ct) => {
            let tok = ct.into_data();

            match tok {
              Token::Dot => match hint {
                #[cfg(feature = "evm")]
                Hint::EvmBuiltin(e) => Err(
                  InvalidPathSegment::with_data_of(
                    first_span,
                    InvalidPathSegmentData::EvmBuiltinFunction(e.unit()),
                  )
                  .into(),
                ),
                Hint::Ident(ident) => {
                  let first_segment = PathSegment::new(Ident::new(first_span.clone(), ident));
                  let segments = vec![first_segment];

                  PathSegment::try_yul_following
                    .separated_by_dot()
                    .collect_with(segments)
                    .spanned()
                    .and_then_with(
                      |spanned: Spanned<_, L::Span>,
                       mut state: ParseState<'_, 'inp, '_, L, Ctx, _>| {
                        let (mut span, segs) = spanned.into_components();
                        *span.start_mut() = first_span.start();
                        if segs.len() == 1 {
                          state.emitter().emit_unexpected_trailing_separator(
                            UnexpectedTrailingDot::<L, _>::trailing_dot_of(
                              span.clone(),
                              tok.clone(),
                            ),
                          )?;
                          Ok(Accept(Self::Path(Path::new(span, segs))))
                        } else {
                          Ok(Accept(Self::Path(Path::new(span, segs))))
                        }
                      },
                    )
                    .parse_input(inp)
                }
              },
              Token::LParen => Self::try_parse_yul
                .separated_by_comma()
                .delimited_by(
                  |t: &L::Token| {
                    if t.is_open_paren() {
                      Ok(())
                    } else {
                      Err(SyntaxKind::LParen.into())
                    }
                  },
                  |t: &L::Token| {
                    if t.is_close_paren() {
                      Ok(())
                    } else {
                      Err(SyntaxKind::RParen.into())
                    }
                  },
                  Brace::PHANTOM,
                )
                .collect()
                .parse_input(inp)
                .map(|exprs: Vec<Self>| {
                  let end = inp.span().end();
                  let start = first_span.start();
                  let fn_name = FunctionName::new(Ident::new(first_span, hint.into_data()));

                  Accept(Self::FunctionCall(FunctionCall::new(
                    Span::new(start, end),
                    fn_name,
                    exprs,
                  )))
                }),
              _ => match hint {
                Hint::Ident(ident) => {
                  let path_segment = PathSegment::new(Ident::new(first_span.clone(), ident));
                  let path = Path::new(first_span, vec![path_segment]);
                  Ok(Accept(Self::Path(path)))
                }
                #[cfg(feature = "evm")]
                Hint::EvmBuiltin(e) => Err(
                  InvalidPathSegment::with_data_of(
                    first_span,
                    InvalidPathSegmentData::EvmBuiltinFunction(e.unit()),
                  )
                  .into(),
                ),
              },
            }
          }
        }
      }
    }
  }
}
