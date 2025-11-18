use lexsol::types::punct::Comma;
use logosky::{
  chumsky::{
    delimited::DelimitedByParen, separated::separated_by, token::recovery::emit_error_until_token,
  },
  error::{ErrorNode, Malformed, UnexpectedToken},
  utils::Span,
};

use crate::{
  error::{AstLexerErrors, TrailingComma},
  scaffold::ast::statement::function_call::FunctionName,
};

use super::*;

/// The tokens that can be used as synchronization points for Yul expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
enum FunctionCallSyncPointToken<S> {
  /// Left parenthesis '(', infer to function call expression, but missing identifier
  #[try_unwrap(ignore)]
  #[unwrap(ignore)]
  #[from(skip)]
  LParen,
  SemiIdentifier(SemiIdentifierToken<S>),
}

impl<S: Clone> FunctionCallSyncPointToken<S> {
  fn to_token(&self) -> AstToken<S> {
    match self {
      Self::LParen => AstToken::LParen,
      Self::SemiIdentifier(semi) => match semi {
        SemiIdentifierToken::Leave => AstToken::Leave,
        SemiIdentifierToken::Continue => AstToken::Continue,
        SemiIdentifierToken::Break => AstToken::Break,
        SemiIdentifierToken::Switch => AstToken::Switch,
        SemiIdentifierToken::Case => AstToken::Case,
        SemiIdentifierToken::Default => AstToken::Default,
        SemiIdentifierToken::Function => AstToken::Function,
        SemiIdentifierToken::Let => AstToken::Let,
        SemiIdentifierToken::If => AstToken::If,
        SemiIdentifierToken::For => AstToken::For,
        SemiIdentifierToken::LitBool(lit) => AstToken::Lit(Lit::Boolean(lit.clone())),
        SemiIdentifierToken::LitDecimal(lit) => {
          AstToken::Lit(Lit::Number(LitNumber::Decimal(lit.clone())))
        }
        SemiIdentifierToken::LitHexadecimal(lit) => {
          AstToken::Lit(Lit::Number(LitNumber::Hexadecimal(lit.clone())))
        }
        #[cfg(feature = "evm")]
        SemiIdentifierToken::EvmBuiltin(name) => AstToken::EvmBuiltin(*name),
        SemiIdentifierToken::Identifier(ident) => AstToken::Identifier(ident.clone()),
      },
    }
  }
}

impl<S> Require<FunctionCallSyncPointToken<S>> for AstToken<S> {
  type Err = Self;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn require(self) -> Result<FunctionCallSyncPointToken<S>, Self::Err>
  where
    Self: Sized,
  {
    Ok(match self {
      Self::Leave => SemiIdentifierToken::Leave.into(),
      Self::Continue => SemiIdentifierToken::Continue.into(),
      Self::Break => SemiIdentifierToken::Break.into(),
      Self::Switch => SemiIdentifierToken::Switch.into(),
      Self::Case => SemiIdentifierToken::Case.into(),
      Self::Default => SemiIdentifierToken::Default.into(),
      Self::Function => SemiIdentifierToken::Function.into(),
      Self::Let => SemiIdentifierToken::Let.into(),
      Self::If => SemiIdentifierToken::If.into(),
      Self::For => SemiIdentifierToken::For.into(),
      Self::Lit(lit) => match lit {
        Lit::Boolean(val) => SemiIdentifierToken::LitBool(val),
        Lit::Number(val) => match val {
          LitNumber::Decimal(val) => SemiIdentifierToken::LitDecimal(val),
          LitNumber::Hexadecimal(val) => SemiIdentifierToken::LitHexadecimal(val),
          lit => return Err(Self::Lit(Lit::Number(lit))),
        },
        lit => return Err(Self::Lit(lit)),
      }
      .into(),
      Self::Identifier(ident) => SemiIdentifierToken::Identifier(ident).into(),
      Self::LParen => FunctionCallSyncPointToken::LParen,
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(name) => {
        FunctionCallSyncPointToken::SemiIdentifier(SemiIdentifierToken::EvmBuiltin(name))
      }
      other => return Err(other),
    })
  }
}

impl<S> FunctionCall<S> {
  /// Attempts to parse a Yul function call expression with error recovery.
  ///
  /// If the content is not possible to be a function call, returns `None`, and no valid token is consumed.
  pub fn parser_with_recovery<'a, E>()
  -> impl Parser<'a, AstTokenizer<'a, S>, Option<Self>, E> + Clone + 'a
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
    Self::parser_with_recovery_inner(Expression::parser_with_recovery())
  }

  pub(super) fn parser_with_recovery_inner<'a, E>(
    expr_parser: impl Parser<'a, AstTokenizer<'a, S>, Option<Expression<S>>, E> + Clone + 'a,
  ) -> impl Parser<'a, AstTokenizer<'a, S>, Option<Self>, E> + Clone + 'a
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
      let (_, result) = inp.parse(emit_error_until_token(
        |Spanned { span, data: tok }, _, emitter| {
          Some(
            match <AstToken<S> as Require<FunctionCallSyncPointToken<S>>>::require(tok) {
              Ok(tok) => {
                if tok.is_l_paren() {
                  emitter.emit(
                    UnexpectedToken::expected_one_with_found(
                      span,
                      tok.to_token(),
                      SyntaxKind::Identifier,
                    )
                    .into(),
                  );
                }
                Ok(Spanned::new(span, tok))
              }
              Err(tok) => Err(Spanned::new(span, tok)),
            },
          )
        },
      ))?;

      // At this point, we have emitted all lexer errors, so record the position of the first valid token
      let valid_ckp = inp.save();
      let valid_start = inp.cursor();

      match result {
        // no valid token found, return Eot error
        None => Err(UnexpectedEot::eot(inp.span_since(&valid_start)).into()),
        // the first valid token cannot be a possible or semi function name token, return None
        // As the emit_error_until_token will not consume the matched token,
        // so no need to rewind here, and just return None here.
        Some(Err(_)) => Ok(None),
        // the first valid token is a possible function name token
        Some(Ok(tok)) => {
          let expr_parser = || {
            let p = expr_parser.clone();
            custom(move |expr_inp| {
              let expr_start = expr_inp.cursor();
              let expr = expr_inp.parse(p.clone())?;
              match expr {
                Some(expr) => Ok(expr),
                // we meet an invalid expression token within the function call arguments
                // so, we need to find the next synchronization point to construct a malformed expression node
                // and continue parsing the function call arguments
                None => {
                  // loop until we find the next synchronization point
                  let mut ckp = expr_inp.save();
                  loop {
                    // TODO(al8n): avoid peeking when inp.emit() stable
                    let next_tok: Option<Lexed<'_, AstToken<S>>> = expr_inp.peek();
                    match next_tok {
                      // reach the end of tokens, emit Eot error and construct malformed expression node
                      None => {
                        let cur = ckp.cursor();
                        let end = expr_inp.span_since(cur).end();
                        return Err(UnexpectedEot::eot(Span::new(end, end)).into());
                      }
                      Some(Lexed::Token(next_tok)) => {
                        if !(next_tok.is_comma() || next_tok.is_paren_close()) {
                          expr_inp.skip();
                          ckp = expr_inp.save();
                          continue;
                        }

                        // found the next synchronization point, construct malformed expression node
                        return Ok(Expression::Error(Malformed::with_knowledge(
                          expr_inp.span_since(&expr_start),
                          crate::syntax::Expression::default(),
                        )));
                      }
                      Some(Lexed::Error(_)) => {
                        let _ = expr_inp.parse(any().validate(|t, _, emitter| {
                          if let Lexed::Error(err) = t {
                            emitter.emit(AstParserError::from(err));
                          }
                        }));
                        ckp = expr_inp.save();
                      }
                    }
                  }
                }
              }
            })
          };
          let args_parser = || {
            DelimitedByParen::recoverable_parser(separated_by::<_, _, _, _, Vec<_>, Comma, _>(
              expr_parser(),
              |t| t.is_comma(),
              |t| t.is_some_and(|t| t.is_paren_close()),
              || SyntaxKind::Comma,
              |tok, sep, emitter| {
                emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into());
              },
            ))
          };

          let fncall = |span, name, args: Result<DelimitedByParen<Spanned<Vec<_>>>, _>| match args {
            Ok(exprs) => {
              let (_, exprs) = exprs.into_components();
              FunctionCall::new(span, name, exprs.data)
            }
            Err(_) => FunctionCall::new(span, name, Vec::new()),
          };

          // missing function name case
          if tok.is_l_paren() {
            let name = FunctionName::new(Ident::missing(Span::new(
              tok.span().start(),
              tok.span().start(),
            )));

            let args = inp.parse(args_parser())?;
            return Ok(Some(fncall(inp.span_since(&valid_start), name, args)));
          }

          // skip the token as we already have it
          inp.skip();
          // check the next token to determine if it's a function call
          let nxt: Option<Lexed<'_, AstToken<S>>> = inp.peek();
          match nxt {
            // check if the next token is a left parenthesis
            Some(Lexed::Token(tok)) if tok.is_l_paren() => {
              // rewind to the valid token start
              inp.rewind(valid_ckp);
              // parse the function call expression
              let (name, args) =
                inp.parse(FunctionName::parser_with_recovery().then(args_parser()))?;

              Ok(Some(fncall(inp.span_since(&valid_start), name, args)))
            }
            // not a function call, rewind and return None
            _ => {
              inp.rewind(valid_ckp);
              Ok(None)
            }
          }
        }
      }
    })
  }
}
