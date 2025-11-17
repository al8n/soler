use lexsol::types::punct::Comma;
use logosky::{
  chumsky::{
    delimited::DelimitedByParen,
    input::{Cursor, InputRef},
    separated::separated_by,
    token::recovery::emit_error_until_token,
  },
  error::{ErrorNode, UnexpectedToken},
  types::Recoverable,
  utils::Span,
};

use crate::{
  error::{AstLexerErrors, TrailingComma},
  scaffold::ast::statement::function_call::FunctionCallName,
};

use super::*;

/// The tokens that can be used as synchronization points for Yul expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
enum FunctionCallSyncPointToken<S> {
  /// Yul identifier, infer to path or a function call expression
  Identifier(S),
  /// Left parenthesis '(', infer to function call expression, but missing identifier
  #[try_unwrap(ignore)]
  #[unwrap(ignore)]
  LParen,
  /// EVM builtin function, infer to function call expression
  #[cfg(feature = "evm")]
  EvmBuiltin(lexsol::yul::EvmBuiltinFunction),
}

impl<S> Require<FunctionCallSyncPointToken<S>> for AstToken<S> {
  type Err = Self;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn require(self) -> Result<FunctionCallSyncPointToken<S>, Self::Err>
  where
    Self: Sized,
  {
    match self {
      AstToken::Identifier(ident) => Ok(FunctionCallSyncPointToken::Identifier(ident)),
      AstToken::LParen => Ok(FunctionCallSyncPointToken::LParen),
      #[cfg(feature = "evm")]
      AstToken::EvmBuiltin(name) => Ok(FunctionCallSyncPointToken::EvmBuiltin(name)),
      other => Err(other),
    }
  }
}

impl<'a, S> Parseable<'a, AstTokenizer<'a, S>, AstToken<S>, AstParserError<'a, S>>
  for Recoverable<FunctionCall<S>>
where
  S: Clone + 'a,
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
  AstTokenizer<'a, S>: LogoStream<'a, AstToken<S>>,
  Ident<S>: ErrorNode,
{
  fn parser<E>() -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone
  where
    Self: Sized + 'a,
    AstTokenizer<'a, S>: LogoStream<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstParserError<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
  {
    recursive(|expr| {
      custom(move |inp| {
        // peek the first valid token, errors will be emitted until we find a valid token
        let (_, result) = inp.parse(emit_error_until_token(|tok, _| Some(tok)))?;

        // save the position, this is the position of the first valid token.
        let valid_start = inp.cursor();
        let valid_ckp = inp.save();

        Ok(Self::Node(match result {
          // end of token stream
          None => return Err(UnexpectedEot::eot(inp.span_since(&valid_start)).into()),
          Some(Spanned { span, data: tok }) => {
            // try to downcast the token into a sync point token
            match <AstToken<S> as Require<FunctionCallSyncPointToken<S>>>::require(tok) {
              // not a sync point token, we should do nothing to avoid consume tokens belonging to the following
              Err(_) => {
                return Ok(Self::Missing(inp.span_since(&valid_start)));
              }
              // find an identifier
              Ok(FunctionCallSyncPointToken::Identifier(ident)) => {
                let ident_start = inp.cursor();
                let ident_ckp = inp.save();

                // consume the first token
                inp.skip();

                // peek the next token to determine if it's a function call or just a path
                let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
                match tok {
                  // there is no next token or a lexer error.
                  None | Some(Lexed::Error(_)) => {
                    // we should rewind to the start point, do not consume anything for good recovery
                    inp.rewind(valid_ckp);
                    return Ok(Self::Missing(inp.span_since(&valid_start)));
                  }
                  Some(Lexed::Token(tok)) => {
                    // if the next token is a dot, then it's a path, not a function call
                    // still rewind to the start point for good recovery, and return Missing
                    if !tok.is_paren_open() {
                      inp.rewind(valid_ckp);
                      return Ok(Self::Missing(inp.span_since(&valid_start)));
                    }

                    let name = FunctionCallName::new(Ident::new(span, ident));
                    parse_arguments(inp, valid_start, name, || SyntaxKind::Comma)?
                  }
                }
              }
              #[cfg(feature = "evm")]
              Ok(FunctionCallSyncPointToken::EvmBuiltin(_)) => {
                inp.skip();
                let ident_end = inp.cursor();
                let name =
                  FunctionCallName::new(Ident::new(span, inp.slice(&valid_start..&ident_end)));

                let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
                match tok {
                  None => return Err(UnexpectedEot::eot(inp.span_since(&valid_start)).into()),
                  Some(Lexed::Error(_)) => {
                    inp.rewind(valid_ckp);
                    return Ok(Self::Missing(inp.span_since(&valid_start)));
                  }
                  Some(Lexed::Token(tok)) => {
                    // TODO(al8n): If this is a builtin function name, should we directly return Missing?
                    // or report an error? and consume it?
                    if !tok.is_paren_open() {
                      inp.rewind(valid_ckp);
                      return Ok(Self::Missing(inp.span_since(&valid_start)));
                    }

                    // must be a function call then, parse arguments
                    parse_arguments(inp, valid_start, name, || SyntaxKind::Comma)?
                  }
                }
              }
              // find a left parenthesis, infer to function call expression but missing identifier
              Ok(FunctionCallSyncPointToken::LParen) => {
                // Create an empty identifier for the function call
                let start = span.start();
                let ident = Ident::missing(Span::new(start, start));

                inp.skip();

                let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
                match tok {
                  None => {
                    return Err(UnexpectedEot::eot(inp.span_since(&valid_start)).into());
                  }
                  // Fast path: check if the next token is a right parenthesis
                  Some(Lexed::Token(tok)) if tok.is_r_paren() => {
                    // empty arguments
                    inp.skip();
                    // construct a function call expression but missing identifier
                    FunctionCall::new(
                      inp.span_since(&valid_start),
                      FunctionCallName::new(ident),
                      Vec::new(),
                    )
                  }
                  // Slow path: parse arguments
                  _ => {
                    inp.rewind(valid_ckp);
                    parse_arguments(inp, valid_start, FunctionCallName::new(ident), || {
                      SyntaxKind::Comma
                    })?
                  }
                }
              }
            }
          }
        }))
      })
    })
  }
}

fn parse_arguments<'a, 'p, S, E>(
  inp: &mut InputRef<'a, 'p, AstTokenizer<'a, S>, E>,
  expr: impl Parser<'a, AstTokenizer<'a, S>, Recoverable<Expression<S>>, E> + Clone + 'a,
  start: Cursor<'a, 'p, AstTokenizer<'a, S>>,
  name: FunctionCallName<S>,
  sep_kind: impl Fn() -> SyntaxKind + Clone + 'a,
) -> Result<FunctionCall<S>, AstParserError<'a, S>>
where
  S: Clone + 'a,
  E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
  AstTokenizer<'a, S>: LogoStream<
      'a,
      AstToken<S>,
      Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
    >,
  AstParserError<'a, S>: 'a,
  Ident<S>: ErrorNode,
{
  let exprs = inp.parse(DelimitedByParen::recoverable_parser(separated_by::<
    _,
    _,
    _,
    _,
    Vec<_>,
    Comma,
    _,
  >(
    expr.map(|expr| expr.unwrap_node()),
    |t: &AstToken<S>| t.is_comma(),
    |t: &AstToken<S>| t.is_r_paren(),
    sep_kind,
    |tok, sep, emitter| {
      emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into());
    },
  )))?;

  match exprs {
    Ok(exprs) => {
      let (_, exprs) = exprs.into_components();
      Ok(FunctionCall::new(inp.span_since(&start), name, exprs.data))
    }
    Err(_) => Ok(FunctionCall::new(inp.span_since(&start), name, Vec::new())),
  }
}
