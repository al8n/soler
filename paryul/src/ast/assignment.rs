use lexsol::types::punct::Comma;
use logosky::{
  chumsky::{separated::separated_by, token::operator::colon_eq_assign},
  error::ErrorNode,
  utils::Span,
};

use crate::{
  error::{
    AstLexerErrors, IncompleteMultipleTargetsAssignment, IncompleteSingleTargetAssignment,
    TrailingComma,
  },
  syntax::{MultipleTargetsAssignmentComponent, SingleTargetAssignmentComponent},
};

use super::*;

impl<S> Assignment<S> {
  /// Attempts to parse a Yul assignment expression with error recovery.
  ///
  /// If the content is not possible to be an assignment, returns `None`, and no valid token is consumed.
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
    Self::parser_with_recovery_inner(false)
  }

  /// Attempts to parse a Yul assignment expression with error recovery.
  ///
  /// If the content is not possible to be an assignment, returns `None`, and no valid token is consumed.
  fn parser_with_recovery_inner<'a, E>(
    rewind_on_missing_colon_assign: bool,
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
    let paths = || {
      separated_by::<_, _, _, _, _, Comma, _>(
        Path::<S>::parser_with_recovery::<E>(),
        |t| t.is_comma(),
        // TODO: if the next token can start a Path without a comma, emit a missing-comma diagnostic
        |t| match t {
          Some(tok) => !tok.is_dot(),
          None => true,
        },
        || SyntaxKind::Comma,
        |tok, sep, emitter| {
          emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into());
        },
      )
    };
    custom(move |inp| {
      let cursor = inp.cursor();
      let start = inp.save();
      let paths: Spanned<Vec<Path<_>>> = inp.parse(paths())?;
      let path_span = paths.span;

      Ok(Some(match paths.len() {
        0 => {
          // we do not have any valid path, cannot be an assignment
          inp.rewind(start);
          return Ok(None);
        }
        // single assignment
        1 => {
          // expect :=
          let expr = inp.parse(
            colon_eq_assign(|| SyntaxKind::ColonAssign)
              .ignore_then(
                Expression::parser_with_recovery()
                  .map_with(|t, exa| t.map(|t| Spanned::new(exa.span(), t))),
              )
              .or_not()
              .validate(|expr, exa, emitter| match expr {
                None => {
                  if !rewind_on_missing_colon_assign {
                    let mut err = IncompleteSingleTargetAssignment::new(
                      Span::new(path_span.start(), exa.span().end()),
                      SingleTargetAssignmentComponent::ColonAssign,
                    );
                    err.push(SingleTargetAssignmentComponent::Expression);
                    emitter.emit(err.into());
                  }

                  None
                }
                Some(None) => {
                  emitter.emit(
                    IncompleteSingleTargetAssignment::new(
                      Span::new(path_span.start(), exa.span().end()),
                      SingleTargetAssignmentComponent::Expression,
                    )
                    .into(),
                  );
                  Some(None)
                }
                Some(Some(expr)) => Some(Some(expr)),
              }),
          )?;

          match expr {
            None if rewind_on_missing_colon_assign => {
              inp.rewind(start);
              return Ok(None);
            }
            None => SingleTargetAssignment::new_partial(
              inp.span_since(&cursor),
              paths.data.into_iter().next().unwrap(),
            )
            .into(),
            Some(Some(expr)) => SingleTargetAssignment::new(
              inp.span_since(&cursor),
              paths.data.into_iter().next().unwrap(),
              expr.into_data(),
            )
            .into(),
            Some(None) => SingleTargetAssignment::new_partial(
              inp.span_since(&cursor),
              paths.data.into_iter().next().unwrap(),
            )
            .into(),
          }
        }
        _ => {
          // multiple targets assignment
          // expect :=
          let fncall = inp.parse(
            colon_eq_assign(|| SyntaxKind::ColonAssign)
              .ignore_then(
                FunctionCall::parser_with_recovery()
                  .map_with(|t, exa| t.map(|t| Spanned::new(exa.span(), t))),
              )
              .or_not()
              .validate(|fncall, exa, emitter| match fncall {
                None => {
                  if !rewind_on_missing_colon_assign {
                    let mut err = IncompleteMultipleTargetsAssignment::new(
                      Span::new(path_span.start(), exa.span().end()),
                      MultipleTargetsAssignmentComponent::ColonAssign,
                    );
                    err.push(MultipleTargetsAssignmentComponent::FunctionCall);
                    emitter.emit(err.into());
                  }

                  None
                }
                Some(None) => {
                  emitter.emit(
                    IncompleteMultipleTargetsAssignment::new(
                      Span::new(path_span.start(), exa.span().end()),
                      MultipleTargetsAssignmentComponent::FunctionCall,
                    )
                    .into(),
                  );
                  Some(None)
                }
                Some(Some(fncall)) => Some(Some(fncall)),
              }),
          )?;

          match fncall {
            None if rewind_on_missing_colon_assign => {
              inp.rewind(start);
              return Ok(None);
            }
            None => {
              MultipleTargetsAssignment::new_partial(inp.span_since(&cursor), paths.data).into()
            }
            Some(Some(fncall)) => MultipleTargetsAssignment::new(
              inp.span_since(&cursor),
              paths.data,
              fncall.into_data(),
            )
            .into(),
            Some(None) => {
              MultipleTargetsAssignment::new_partial(inp.span_since(&cursor), paths.data).into()
            }
          }
        }
      }))
    })
  }
}
