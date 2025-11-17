use logosky::{
  chumsky::token::recovery::emit_error_until_token,
  error::{ErrorNode, Malformed},
  utils::{AsSpan, Span},
};

use crate::error::AstLexerErrors;

use super::*;

/// The tokens that can be used as synchronization points for Yul expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
enum ExpressionSyncPointToken<S> {
  /// Yul literal
  ///
  /// Spec: [Yul literals](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulLiteral)
  Lit(Lit<S>),
  /// Left parenthesis '(', infer to function call expression, but missing identifier
  #[try_unwrap(ignore)]
  #[unwrap(ignore)]
  #[from(skip)]
  LParen,
  SemiIdentifier(SemiIdentifierToken<S>),
}

impl<S> Require<ExpressionSyncPointToken<S>> for AstToken<S> {
  type Err = Self;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn require(self) -> Result<ExpressionSyncPointToken<S>, Self::Err>
  where
    Self: Sized,
  {
    Ok(match self {
      AstToken::Identifier(ident) => SemiIdentifierToken::Identifier(ident).into(),
      AstToken::Lit(lit) => lit.into(),
      AstToken::LParen => ExpressionSyncPointToken::LParen,
      #[cfg(feature = "evm")]
      AstToken::EvmBuiltin(name) => SemiIdentifierToken::EvmBuiltin(name).into(),
      other => return Err(other),
    })
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
  /// Malformed expression
  Error(Malformed<crate::syntax::Expression>),
}

impl<S> AsSpan<Span> for Expression<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_span(&self) -> &Span {
    match self {
      Self::Path(path) => path.as_span(),
      Self::FunctionCall(fn_call) => fn_call.as_span(),
      Self::Literal(lit) => lit.as_span(),
      Self::Error(err) => err.span_ref(),
    }
  }
}

impl<S> Expression<S> {
  /// Attempts to parse a Yul expression with error recovery.
  ///
  /// If the content is not possible to be an expression, returns `None`, and no valid token is consumed.
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
    recursive(move |expr| {
      custom(move |inp| {
        let (_, result) = inp.parse(emit_error_until_token(|Spanned { span, data: tok }, _| {
          Some(
            match <AstToken<S> as Require<ExpressionSyncPointToken<S>>>::require(tok) {
              Ok(tok) => Ok(Spanned::new(span, tok)),
              Err(tok) => Err(Spanned::new(span, tok)),
            },
          )
        }))?;

        let valid_start = inp.cursor();
        let valid_ckp = inp.save();

        Ok(Some(match result {
          None => return Err(UnexpectedEot::eot(inp.span_since(&valid_start)).into()),
          // not a valid expression sync point token, so, we do not consume it.
          // The reason why we do not construct a malformed expression node here is
          // that we want the parser to avoid stealing tokens from higher level constructs.
          // If we construct a malformed expression node here, the expression parser
          // may consume tokens that belong to the higher level constructs, causing cascading errors.
          // Hence, we let the higher level parser decide how to handle the unexpected token.
          Some(Err(_)) => return Ok(None),
          Some(Ok(Spanned { span, data: tok })) => match tok {
            ExpressionSyncPointToken::Lit(lit) => {
              inp.skip();
              Self::Literal(Spanned::new(span, lit))
            }
            _ => {
              let fncall = inp.parse(FunctionCall::parser_with_recovery_inner(expr.clone()))?;
              match fncall {
                Some(fncall) => Self::FunctionCall(fncall),
                None => {
                  inp.rewind(valid_ckp);
                  let path: Path<S> = inp.parse(Path::parser_with_recovery())?;
                  Self::Path(path)
                }
              }
            }
          },
        }))
      })
    })
  }
}
