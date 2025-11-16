use lexsol::types::punct::Dot;
use logosky::{chumsky::separated::separated_by, error::Missing, types::Recoverable};

use crate::{error::{AstLexerErrors, TrailingDot}, scaffold::ast::path::PathSegment};

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

impl<'a, S> Parseable<'a, AstTokenizer<'a, S>, AstToken<S>, AstParserError<'a, S>> for Recoverable<Path<S>>
where
  S: Clone + 'a,
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
  AstTokenizer<'a, S>: LogoStream<'a, AstToken<S>>,
{
  fn parser<E>() -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone
  where
    Self: Sized + 'a,
    AstTokenizer<'a, S>: LogoStream<'a, AstToken<S>, Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    AstToken<S>: Token<'a>,
    AstParserError<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a
  {
    custom(move |inp| {
      let before = inp.cursor();

      let seg: Option<Ident<S>> = inp.parse(any().validate(|tok: Lexed<'_, AstToken<S>>, _, emitter| {
        match tok {
          Lexed::Error(err) => {
            emitter.emit(AstParserError::from(err));
            None
          }
          Lexed::Token(Spanned { span, data: tok }) => {
            match tok.try_unwrap_identifier() {
              Ok(ident) => Some(Ident::new(span, ident)),
              Err(tok) => {
                emitter.emit(
                  UnexpectedToken::expected_one_with_found(
                    span,
                    tok.input,
                    SyntaxKind::Identifier,
                  )
                  .into(),
                );
                None
              }
            }
          }
        }
      }))?;

      Ok(Self::Node(match seg {
        None => {
          return Ok(Recoverable::Missing(inp.span_since(&before)));
        },
        Some(seg) => {
          let mut segments = Vec::with_capacity(4);
          let first_segment_span = seg.span();
          segments.push(PathSegment::new(seg));

          // check if the next token is a dot, if so continue parsing segments
          let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
          match tok {
            Some(Lexed::Token(Spanned { data: tok, .. })) if tok.is_dot() => {
              inp.skip(); // consume the dot
            }
            Some(Lexed::Token(Spanned { span, data: tok })) => {
              if !is_path_segment_token(&tok) {
                return Ok(Self::Node(Path::new(inp.span_since(&before), segments)));
              }

              let _ = inp.parse(any().or_not().validate(|_, _, emitter| {
                emitter.emit(
                  Missing::<Dot, YUL>::between(
                    first_segment_span,
                    span
                  )
                  .into(),
                );
              }).rewind());
            }
            _ => {}
          }

          // attempt to parse following segments separated by dots
          let ckp = inp.save();
          let remaining_segs = inp.parse(separated_by::<_, _, _, _, Vec<_>, Dot, _>(any().try_map_with(|t: Lexed<'_, AstToken<S>>, exa| {
            match t {
              Lexed::Token(Spanned { span, data: tok }) => {
                match tok {
                  AstToken::Identifier(ident) => {
                    Ok(PathSegment::new(Ident::new(span, ident)))
                  }
                  #[cfg(feature = "evm")]
                  AstToken::EvmBuiltin(_) => {
                    Ok(PathSegment::new(Ident::new(span, exa.slice())))
                  }
                  tok => Err(
                    UnexpectedToken::expected_one_with_found(
                      span,
                      tok,
                      SyntaxKind::PathSegment,
                    )
                    .into(),
                  ),
                }
              },
              Lexed::Error(e) => Err(e.into()),
            }
          }),
          |t: &AstToken<S>| t.is_dot(),
          |t| !is_path_segment_token(t),
          || SyntaxKind::Dot,
          |tok, sep, emitter| {
            emitter.emit(TrailingDot::from_suffix(tok, *sep.span()).into()); 
          }
        ));

          match remaining_segs {
            Err(_) => {
              inp.rewind(ckp);
              Path::new(inp.span_since(&before), segments)
            }
            Ok(remaining_segs) => {
              segments.extend(remaining_segs.data);
              Path::new(inp.span_since(&before), segments)
            }
          }
        }
      }))
    })
  }
}
