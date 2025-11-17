use lexsol::types::punct::Dot;
use logosky::{
  chumsky::separated::separated_by,
  error::{ErrorNode, Missing},
  types::Keyword,
  utils::Span,
};

use crate::{
  error::{AstLexerErrors, InvalidPathSegment, SemiIdentifierKnowledge, TrailingDot},
  scaffold::ast::path::PathSegment,
};

use super::*;

#[cfg_attr(not(tarpaulin), inline(always))]
const fn is_path_segment_token<S>(tok: &AstToken<S>) -> bool {
  match tok {
    AstToken::Identifier(_) => true,
    #[cfg(feature = "evm")]
    AstToken::EvmBuiltin(_) => true,

    _ => false,
  }
}

impl<S> PathSegment<S> {
  pub fn leading_segment_parser_with_recovery<'a, E>()
  -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone + 'a
  where
    S: Clone
      + ErrorNode
      + From<<<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>
      + 'a,
    AstToken<S>: Token<'a>,
    <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
    AstTokenizer<'a, S>: LogoStream<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstParserError<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
  {
    Self::parser_with_recovery_inner(true)
  }

  pub fn following_segment_parser_with_recovery<'a, E>()
  -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone + 'a
  where
    S: Clone
      + ErrorNode
      + From<<<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>
      + 'a,
    AstToken<S>: Token<'a>,
    <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
    AstTokenizer<'a, S>: LogoStream<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstParserError<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
  {
    Self::parser_with_recovery_inner(false)
  }

  fn parser_with_recovery_inner<'a, E>(
    is_leading: bool,
  ) -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone + 'a
  where
    S: Clone
      + ErrorNode
      + From<<<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>
      + 'a,
    AstToken<S>: Token<'a>,
    <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
    AstTokenizer<'a, S>: LogoStream<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstParserError<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
  {
    let parser = any().validate(move |tok: Lexed<'_, AstToken<S>>, exa, emitter| match tok {
      Lexed::Error(err) => {
        emitter.emit(AstParserError::from(err));
        Err((false, Self::new(Ident::error(exa.span()))))
      }
      Lexed::Token(Spanned { span, data: tok }) => {
        match <AstToken<S> as Require<SemiIdentifierToken<S>>>::require(tok) {
          Ok(seg) => {
            let err = match seg {
              SemiIdentifierToken::LitBool(val) => {
                SemiIdentifierKnowledge::LitBool(Spanned::new(span, val)).into()
              }
              SemiIdentifierToken::LitDecimal(val) => {
                SemiIdentifierKnowledge::LitDecimal(Spanned::new(span, val)).into()
              }
              SemiIdentifierToken::LitHexadecimal(val) => {
                SemiIdentifierKnowledge::LitHexadecimal(Spanned::new(span, val)).into()
              }
              #[cfg(feature = "evm")]
              SemiIdentifierToken::EvmBuiltin(val) => {
                // if this is not a leading path segment, then evm builtin fn can be used as a path segment
                if !is_leading {
                  return Ok((false, Self::new(Ident::new(span, S::from(exa.slice())))));
                }
                SemiIdentifierKnowledge::EvmBuiltinFunction(Spanned::new(span, val)).into()
              }

              // valid path segment token, nothing to do.
              SemiIdentifierToken::Identifier(ident) => {
                return Ok((false, Self::new(Ident::new(span, ident))));
              }
              _ => {
                SemiIdentifierKnowledge::Keyword(Keyword::new(span, S::from(exa.slice()))).into()
              }
            };

            let err = InvalidPathSegment::with_knowledge(span, err);
            emitter.emit(err.into());

            Ok((false, Self::new(Ident::new(span, S::from(exa.slice())))))
          }
          Err(tok) => {
            emitter.emit(
              UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::Identifier).into(),
            );
            Err((
              true,
              Self::new(Ident::missing(Span::new(span.start(), span.start()))),
            ))
          }
        }
      }
    });

    custom(move |inp| {
      let ckp = inp.save();

      match inp.parse(parser)? {
        Err((rewind, seg)) => {
          if rewind {
            inp.rewind(ckp);
          }
          Ok(seg)
        }
        // on ok case, we never need rewind.
        Ok((_, seg)) => Ok(seg),
      }
    })
  }
}

impl<S> Path<S> {
  pub fn parser_with_recovery<'a, E>() -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone + 'a
  where
    S: Clone
      + ErrorNode
      + From<<<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>
      + 'a,
    AstToken<S>: Token<'a>,
    <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
    AstTokenizer<'a, S>: LogoStream<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstParserError<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
  {
    custom(move |inp| {
      let valid_start = inp.cursor();

      let seg = inp.parse(PathSegment::leading_segment_parser_with_recovery())?;

      let seg_end = inp.save();
      let seg_span = seg.span();
      // check if the next token is a dot, if so continue parsing segments
      let peeked = inp.parse(any().validate(|tok: Lexed<'_, AstToken<S>>, exa, emitter| {
        Some(match tok {
          Lexed::Token(Spanned { data: tok, .. }) if tok.is_dot() => {
            // rewind = false, consume the dot
            false
          }
          Lexed::Token(Spanned { span, data: tok }) => {
            match <AstToken<S> as Require<SemiIdentifierToken<S>>>::require(tok) {
              Err(_) => {
                // not possible to be a path segment token, we should return the single segment path
                return None;
              }
              Ok(_) => {
                emitter.emit(Missing::<Dot, YUL>::between(seg_span, span).into());
                // rewind = true, missing dot, and the token is a bad path segment token
                true
              }
            }
          }
          Lexed::Error(_) => {
            emitter.emit(Missing::<Dot, YUL>::between(seg_span, exa.span()).into());
            // rewind = true, missing dot, and the token is a bad path segment token
            true
          }
        })
      }));

      match peeked {
        // consume the dot, nothing to do
        Ok(Some(false)) => {}
        // not a dot, missing dot error has been emitted
        Ok(Some(true)) => {
          inp.rewind(seg_end);
        }
        // not a dot, just return the path with single segment
        Ok(None) => {
          inp.rewind(seg_end);
          return Ok(Path::new(
            inp.span_since(&valid_start),
            [seg].into_iter().collect(),
          ));
        }
        // lexer error, just return the path with single segment
        Err(_) => {
          return Ok(Path::new(
            inp.span_since(&valid_start),
            [seg].into_iter().collect(),
          ));
        }
      }

      let ckp = inp.save();
      let remaining_segs = inp.parse(separated_by::<_, _, _, _, Vec<_>, Dot, _>(
        PathSegment::following_segment_parser_with_recovery(),
        |t: &AstToken<S>| t.is_dot(),
        |t| !is_path_segment_token(t),
        || SyntaxKind::Dot,
        |tok, sep, emitter| {
          emitter.emit(TrailingDot::from_suffix(tok, *sep.span()).into());
        },
      ));

      Ok(match remaining_segs {
        Err(_) => {
          inp.rewind(ckp);
          Path::new(inp.span_since(&valid_start), [seg].into_iter().collect())
        }
        Ok(remaining_segs) => {
          let (_, remaining) = remaining_segs.into_components();
          let cap = 1 + remaining.len();

          let mut segments = Vec::with_capacity(cap);
          segments.push(seg);
          segments.extend(remaining);
          Path::new(inp.span_since(&valid_start), segments)
        }
      })
    })
  }
}
