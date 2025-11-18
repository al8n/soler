use lexsol::types::punct::Comma;
use logosky::{
  chumsky::{
    separated::separated_by,
    token::{operator::colon_eq_assign, recovery::emit_error_until_token},
  },
  error::{ErrorNode, UnexpectedToken},
  types::Keyword,
  utils::Span,
};

use crate::{
  error::{
    AstLexerErrors, IncompleteMultipleVariablesDeclaration, IncompleteSingleVariableDeclaration,
    IncompleteVariableDeclaration, InvalidVariableName, SemiIdentifierKnowledge, TrailingComma,
  },
  syntax::{
    MultipleVariablesDeclarationComponent, SingleVariableDeclarationComponent,
    VariableDeclarationComponent,
  },
};

use super::*;

enum Kind {
  Single,
  Multiple,
}

impl<S> VariableDeclaration<S> {
  /// Attempts to parse a Yul variable declaration with error recovery.
  ///
  /// If the content is not possible to be a variable declaration, returns `None`, and no valid token is consumed.
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
    custom(|inp| {
      let (_, let_kw): (_, Option<()>) = inp.parse(
        emit_error_until_token::<_, AstToken<S>, _, _>(|tok, _, _| tok.is_let().then_some(())),
      )?;

      if let_kw.is_none() {
        return Ok(None);
      }

      let node_start = inp.cursor();

      inp.skip();
      let let_span: Span = inp.span_since(&node_start);

      // find and peek the variable name token and emit all lexer errors along the way.
      let nxt = inp.parse(
        emit_error_until_token(|tok, exa, emitter| {
          let span = tok.span;
          match <AstToken<S> as Require<SemiIdentifierToken<S>>>::require(tok.data) {
            Ok(tok) => {
              let (src, err) = match tok {
                SemiIdentifierToken::LitBool(lit_bool) => (
                  None,
                  SemiIdentifierKnowledge::LitBool(Spanned::new(span, lit_bool.clone())),
                ),
                SemiIdentifierToken::LitDecimal(lit_decimal) => (
                  None,
                  SemiIdentifierKnowledge::LitDecimal(Spanned::new(span, lit_decimal.clone())),
                ),
                SemiIdentifierToken::LitHexadecimal(lit_hexadecimal) => (
                  None,
                  SemiIdentifierKnowledge::LitHexadecimal(Spanned::new(
                    span,
                    lit_hexadecimal.clone(),
                  )),
                ),
                #[cfg(feature = "evm")]
                SemiIdentifierToken::EvmBuiltin(evm_builtin_function) => (
                  Some(S::from(exa.slice())),
                  SemiIdentifierKnowledge::EvmBuiltinFunction(Spanned::new(
                    span,
                    evm_builtin_function,
                  )),
                ),
                SemiIdentifierToken::Identifier(ident) => {
                  return Some(Ok((
                    None,
                    SemiIdentifierKnowledge::Identifier(Ident::new(span, ident)),
                  )));
                }
                _ => (
                  None,
                  SemiIdentifierKnowledge::Keyword(Keyword::new(span, S::from(exa.slice()))),
                ),
              };

              let tok = err.clone();
              emitter.emit(InvalidVariableName::with_knowledge(span, err.into()).into());
              Some(Ok((src, tok)))
            }
            Err(tok) => {
              let mut emit = |tok| {
                emitter.emit(
                  UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::Identifier)
                    .into(),
                )
              };

              match tok {
                AstToken::ColonAssign => {
                  emit(tok);
                  Some(Err(Ok(Spanned::new(span, Kind::Single))))
                }
                AstToken::Comma => {
                  emit(tok);
                  Some(Err(Ok(Spanned::new(span, Kind::Multiple))))
                }
                _ => Some(Err(Err(span))),
              }
            }
          }
        })
        .validate(|(_, tok), _, emitter| {
          if tok.is_none() {
            emitter.emit(
              IncompleteVariableDeclaration::new(let_span, VariableDeclarationComponent::Lhs)
                .into(),
            );
          }
          tok
        }),
      )?;

      // TODO: variables_parser treats any non-semi-identifier as an end token; if that
      // predicate changes, revisit the signature/usage here.
      let variables_parser = separated_by::<_, _, _, _, Vec<_>, Comma, _>(
        VariableName::parser_with_recovery(),
        |t| t.is_comma(),
        |t| match t {
          None => true,
          Some(tok) => !tok.is_semi_identifier(),
        },
        || SyntaxKind::Comma,
        |tok, sep, emitter| {
          emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into());
        },
      );

      // NB: the nxt is peeked token
      Ok(Some(match nxt {
        // no next token found, but we have seen 'let' keyword, so incomplete variable declaration
        None => SingleVariableDeclaration::new(
          inp.span_since(&node_start),
          VariableName::new(Ident::missing(Span::new(let_span.end(), let_span.end()))),
          None,
        )
        .into(),
        // found a token neither identifier nor colon assign nor comma
        Some(Err(Err(span))) => {
          // the token is peeked, so we do not need to rewind
          let end = span.start();
          let name = VariableName::new(Ident::missing(Span::new(end, end)));
          SingleVariableDeclaration::new(let_span, name, None).into()
        }
        // found colon assign, but missing variable name
        Some(Err(Ok(Spanned {
          span,
          data: Kind::Single,
        }))) => {
          // nxt is peeked, so we always have a colon assign token.
          let res = inp.parse(
            colon_eq_assign(|| SyntaxKind::ColonAssign)
              .ignore_then(Expression::parser_with_recovery())
              .validate(|expr, exa, emitter| {
                // we must have a colon assign, but missing expression, emit error
                if expr.is_none() {
                  let tail_span = exa.span();
                  let span = Span::new(let_span.start(), tail_span.end());
                  let mut err = IncompleteSingleVariableDeclaration::new(
                    span,
                    SingleVariableDeclarationComponent::Name,
                  );
                  err.push(SingleVariableDeclarationComponent::Expression);
                  emitter.emit(err.into());
                }
                expr
              }),
          )?;

          let end = span.start();
          let name = VariableName::new(Ident::missing(Span::new(end, end)));
          match res {
            None => SingleVariableDeclaration::new(inp.span_since(&node_start), name, None),
            Some(expr) => {
              SingleVariableDeclaration::new(inp.span_since(&node_start), name, Some(expr))
            }
          }
          .into()
        }
        // found comma, but missing variable name.
        Some(Err(Ok(Spanned {
          span,
          data: Kind::Multiple,
        }))) => {
          // create a placeholder variable name
          let variable = {
            let end = span.start();
            VariableName::new(Ident::missing(Span::new(end, end)))
          };
          // Skip the comma
          inp.skip();

          // parse the remaining variable names
          let variables = inp.parse(variables_parser);
          match variables {
            // failed to parse remaining variable names
            Err(_) => {
              // TODO: emit an IncompleteVariableDeclaration for missing names/initializer here
              MultipleVariablesDeclaration::new(inp.span_since(&node_start), vec![variable], None)
                .into()
            }
            Ok(mut variables) => {
              // insert the placeholder variable name at the front
              variables.data.insert(0, variable);
              let num_variables = variables.data.len();

              let fncall = inp.parse(
                colon_eq_assign(|| SyntaxKind::ColonAssign)
                  .ignore_then(FunctionCall::parser_with_recovery())
                  .or_not()
                  .validate(|fncall, exa, emitter| {
                    match fncall {
                      None => None,
                      Some(Some(fncall)) => Some(fncall),
                      // has colon assign but missing expression
                      Some(None) => {
                        let tail_span = exa.span();
                        let span = Span::new(let_span.start(), tail_span.end());
                        let mut err = IncompleteMultipleVariablesDeclaration::new(
                          span,
                          MultipleVariablesDeclarationComponent::FunctionCall,
                        );
                        if num_variables == 1 {
                          err.push_front(MultipleVariablesDeclarationComponent::Names);
                        }
                        emitter.emit(err.into());
                        None
                      }
                    }
                  }),
              )?;
              match fncall {
                None => MultipleVariablesDeclaration::new(
                  inp.span_since(&node_start),
                  variables.data,
                  None,
                )
                .into(),
                Some(fncall) => MultipleVariablesDeclaration::new(
                  inp.span_since(&node_start),
                  variables.data,
                  Some(fncall),
                )
                .into(),
              }
            }
          }
        }
        // found an identifier
        Some(Ok((src, tok))) => {
          let variables = inp.parse(variables_parser);
          match variables {
            Err(_) => {
              // TODO: Emit an explicit IncompleteVariableDeclaration here so users
              // know we bailed out parsing the name list and initializer.
              let span = inp.span_since(&node_start);
              let name = VariableName::new(match tok {
                #[cfg(feature = "evm")]
                SemiIdentifierKnowledge::EvmBuiltinFunction(spanned) => {
                  Ident::new(spanned.span, src.unwrap())
                }
                SemiIdentifierKnowledge::Identifier(ident) => ident,
                SemiIdentifierKnowledge::Keyword(keyword) => keyword.into(),
                SemiIdentifierKnowledge::LitBool(spanned) => LitBool::into_identifier(spanned),
                SemiIdentifierKnowledge::LitDecimal(spanned) => {
                  LitDecimal::into_identifier(spanned)
                }
                SemiIdentifierKnowledge::LitHexadecimal(spanned) => {
                  LitHexadecimal::into_identifier(spanned)
                }
              });
              SingleVariableDeclaration::new(span, name, None).into()
            }
            // single variable declaration
            Ok(variables) if variables.len() == 1 => {
              let expr = inp.parse(
                colon_eq_assign(|| SyntaxKind::ColonAssign)
                  .ignore_then(Expression::parser_with_recovery())
                  .or_not()
                  .validate(|expr, exa, emitter| {
                    match expr {
                      None => None,
                      Some(Some(expr)) => Some(expr),
                      // has colon assign but missing expression
                      Some(None) => {
                        emitter.emit(
                          IncompleteSingleVariableDeclaration::new(
                            Span::new(let_span.start(), exa.span().end()),
                            SingleVariableDeclarationComponent::Expression,
                          )
                          .into(),
                        );
                        None
                      }
                    }
                  }),
              );

              let span = inp.span_since(&node_start);
              let name = variables.data.into_iter().next().unwrap();

              match expr {
                Err(_) => SingleVariableDeclaration::new(span, name, None).into(),
                Ok(expr) => SingleVariableDeclaration::new(span, name, expr).into(),
              }
            }
            // multiple variables declaration
            Ok(variables) => {
              let expr = inp.parse(
                colon_eq_assign(|| SyntaxKind::ColonAssign)
                  .ignore_then(FunctionCall::parser_with_recovery())
                  .or_not()
                  .validate(|expr, exa, emitter| {
                    match expr {
                      None => None,
                      Some(Some(expr)) => Some(expr),
                      // has colon assign but missing expression
                      Some(None) => {
                        emitter.emit(
                          IncompleteMultipleVariablesDeclaration::new(
                            Span::new(let_span.start(), exa.span().end()),
                            MultipleVariablesDeclarationComponent::FunctionCall,
                          )
                          .into(),
                        );
                        None
                      }
                    }
                  }),
              );

              let span = inp.span_since(&node_start);

              match expr {
                Err(_) => MultipleVariablesDeclaration::new(span, variables.data, None).into(),
                Ok(expr) => MultipleVariablesDeclaration::new(span, variables.data, expr).into(),
              }
            }
          }
        }
      }))
    })
  }
}
