use lexsol::types::punct::Comma;
use logosky::{
  chumsky::{
    delimited::DelimitedByParen,
    separated::separated_by,
    token::{punct::comma, recovery::{
      emit_error_until_token, emit_error_until_token_inclusive, emit_until_token, emit_until_token_inclusive, emit_with
    }},
  }, error::{ErrorNode, UnexpectedToken, UnknownLexeme}, syntax::Syntax, types::Recoverable, utils::{AsSpan, Span}
};

use crate::{
  error::{AstLexerErrors, UnknownExpression, TrailingComma},
  scaffold::ast::statement::function_call::FunctionCallName,
};

use super::*;

/// The tokens that can be used as synchronization points for Yul expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
enum ExpressionSyncPointToken<S> {
  /// Yul identifier, infer to path or a function call expression
  Identifier(S),
  /// Yul literal
  ///
  /// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
  Lit(Lit<S>),
  /// Left parenthesis '(', infer to function call expression, but missing identifier
  #[try_unwrap(ignore)]
  #[unwrap(ignore)]
  LParen,
  /// EVM builtin function, infer to function call expression
  #[cfg(feature = "evm")]
  EvmBuiltin(lexsol::yul::EvmBuiltinFunction),
}

impl<S> Require<ExpressionSyncPointToken<S>> for AstToken<S> {
  type Err = Self;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn require(self) -> Result<ExpressionSyncPointToken<S>, Self::Err>
  where
    Self: Sized,
  {
    match self {
      AstToken::Identifier(ident) => Ok(ExpressionSyncPointToken::Identifier(ident)),
      AstToken::Lit(lit) => Ok(ExpressionSyncPointToken::Lit(lit)),
      AstToken::LParen => Ok(ExpressionSyncPointToken::LParen),
      #[cfg(feature = "evm")]
      AstToken::EvmBuiltin(name) => Ok(ExpressionSyncPointToken::EvmBuiltin(name)),
      other => Err(other),
    }
  }
}

impl<S: Clone> ExpressionSyncPointToken<S> {
  fn to_token(&self) -> AstToken<S> {
    match self {
      Self::Identifier(ident) => AstToken::Identifier(ident.clone()),
      Self::Lit(lit) => AstToken::Lit(lit.clone()),
      Self::LParen => AstToken::LParen,
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(name) => AstToken::EvmBuiltin(*name),
    }
  }
}



/// The expression type for Yul.
///
/// Spec: [Yul Expression](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulExpression)
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Expression<S> {
  /// Yul path
  Path(Path<S>),
  /// Yul function call
  FunctionCall(FunctionCall<S>),
  /// Yul literal
  Literal(Spanned<Lit<S>>),
}

impl<S> AsSpan<Span> for Expression<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    match self {
      Self::Path(path) => path.as_span(),
      Self::FunctionCall(fn_call) => fn_call.as_span(),
      Self::Literal(lit) => lit.as_span(),
    }
  }
}

impl<'a, S> Parseable<'a, AstTokenizer<'a, S>, AstToken<S>, AstParserError<'a, S>>
  for Recoverable<Expression<S>>
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
    AstToken<S>: Token<'a>,
    AstParserError<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
  {
    recursive(|expr| {
      custom(move |inp| {
        let before = inp.cursor();

        let result = inp.parse(
          emit_error_until_token(|Spanned { span, data: tok }, emitter| {
            match <AstToken<S> as Require<ExpressionSyncPointToken<S>>>::require(tok) {
              Ok(sync_point) => Some(Spanned::new(span, sync_point)),
              Err(other) => {
                emitter.emit(
                  UnexpectedToken::expected_one_of_with_found(
                    span,
                    other,
                    &[SyntaxKind::Identifier, SyntaxKind::Lit],
                  )
                  .into(),
                );
                None
              }
            }
          })
          .validate(|(skipped, t), exa, emitter| {
            if skipped > 0 {
              emitter.emit(UnknownExpression::from_range(exa.span(), Default::default()).into());
            }
            if let Some(ref t) = t {
              if t.is_l_paren() {
                emitter.emit(
                  UnexpectedToken::expected_one_with_found(
                    t.span,
                    t.data().to_token(),
                    SyntaxKind::Identifier,
                  )
                  .into(),
                );
              }
            }
            t
          }),
        )?;

        Ok(Self::Node(match result {
          None => return Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
          Some(Spanned { span, data: tok }) => match tok {
            ExpressionSyncPointToken::Lit(lit) => {
              inp.skip();
              Expression::Literal(Spanned::new(span, lit))
            },
            ExpressionSyncPointToken::Identifier(ident) => {
              let start = inp.cursor();
              let ckp = inp.save();
              let ident = Ident::new(span, ident);

              inp.skip();

              let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
              match tok {
                Some(Lexed::Token(tok)) if tok.is_paren_open() => {
                  // parse the exprs
                  let exprs = inp.parse(DelimitedByParen::recoverable_parser(separated_by::<_, _, _, _, Vec<_>, Comma, _>(
                    expr.clone().map(|expr: Self| expr.unwrap_node()),
                    |t: &AstToken<S>| t.is_comma(),
                    |t: &AstToken<S>| t.is_r_paren(),
                    || SyntaxKind::Comma,
                    |tok, sep, emitter| {
                      emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into()); 
                    }
                  )))?;

                  match exprs {
                    Ok(exprs) => {
                      let (_, exprs) = exprs.into_components();
                      Expression::FunctionCall(FunctionCall::new(
                        inp.span_since(&start),
                        FunctionCallName::new(ident),
                        exprs.data,
                      ))
                    }
                    Err(span) => {
                      Expression::FunctionCall(FunctionCall::new(
                        inp.span_since(&start),
                        FunctionCallName::new(ident),
                        Vec::new(),
                      ))
                    }
                  }
                }
                Some(Lexed::Token(tok)) if tok.is_dot() => {
                  inp.rewind(ckp);

                  let segments = inp.parse(any().separated_by(comma(|| SyntaxKind::Comma)).map(|t| {
                    
                  }).repeated());
                  // // rewind to before identifier so the path parser can consume it
                  // inp.rewind(before_ident_checkpoint);
                  // Expression::Path(parse_path(expr.clone())?)


                  todo!()
                }
                _ => Expression::Path(Path::new(span, [PathSegment::new(ident)].into_iter().collect())),
              }
            }
            #[cfg(feature = "evm")]
            ExpressionSyncPointToken::EvmBuiltin(_) => {
              let start = inp.cursor();
              inp.skip();
              let end = inp.cursor();
              let name = FunctionCallName::new(Ident::new(span, inp.slice(&start..&end)));
              // parse the exprs
              let exprs = inp.parse(DelimitedByParen::recoverable_parser(separated_by::<_, _, _, _, Vec<_>, Comma, _>(
                expr.clone().map(|expr: Self| expr.unwrap_node()),
                |t: &AstToken<S>| t.is_comma(),
                |t: &AstToken<S>| t.is_r_paren(),
                || SyntaxKind::Comma,
                |tok, sep, emitter| {
                  emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into()); 
                }
              )))?;

              match exprs {
                Ok(exprs) => {
                  let (_, exprs) = exprs.into_components();
                  Expression::FunctionCall(FunctionCall::new(
                    inp.span_since(&start),
                    name,
                    exprs.data,
                  ))
                }
                Err(span) => {
                  Expression::FunctionCall(FunctionCall::new(
                    inp.span_since(&start),
                    name,
                    Vec::new(),
                  ))
                }
              }
            }
            // infer to function call, missing identifier case
            ExpressionSyncPointToken::LParen => {
              let ident = Ident::missing(span);

              let cur = inp.cursor();
              let ck = inp.save();
              inp.skip();

              // Fast path: check if the next token is a right parenthesis
              let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
              match tok {
                None => {
                  inp.skip();
                  return Err(UnexpectedEot::eot(inp.span_since(&cur)).into());
                }
                Some(Lexed::Token(tok)) if tok.is_r_paren() => {
                  // empty arguments
                  inp.skip();
                  // construct a function call expression but missing identifier
                  return Ok(Self::Node(Expression::FunctionCall(FunctionCall::new(
                    inp.span_since(&cur),
                    FunctionCallName::new(ident),
                    Vec::new(),
                  ))));
                }
                _ => {
                  inp.rewind(ck);
                }
              }

              // parse the exprs
              let exprs = inp.parse(DelimitedByParen::recoverable_parser(separated_by::<_, _, _, _, Vec<_>, Comma, _>(
                expr.clone().map(|expr: Self| expr.unwrap_node()),
                |t: &AstToken<S>| t.is_comma(),
                |t: &AstToken<S>| t.is_r_paren(),
                || SyntaxKind::Comma,
                |tok, sep, emitter| {
                  emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into()); 
                }
              )))?;

              match exprs {
                Ok(exprs) => {
                  let (_, exprs) = exprs.into_components();
                  Expression::FunctionCall(FunctionCall::new(
                    inp.span_since(&cur),
                    FunctionCallName::new(ident),
                    exprs.data,
                  ))
                }
                Err(span) => {
                  Expression::FunctionCall(FunctionCall::new(
                    inp.span_since(&cur),
                    FunctionCallName::new(ident),
                    Vec::new(),
                  ))
                }
              }
            }
          },
        }))
      })
    })
  }
}
