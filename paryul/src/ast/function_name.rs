use logosky::{
  error::ErrorNode,
  types::Keyword,
  utils::Span,
};

use crate::error::{AstLexerErrors, InvalidFunctionName, SemiIdentifierKnowledge};

use super::*;

impl<S> FunctionName<S> {
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
                SemiIdentifierKnowledge::EvmBuiltinFunction(Spanned::new(span, val)).into()
              }
              // valid function name token, nothing to do.
              SemiIdentifierToken::Identifier(ident) => {
                return Ok((false, Self::new(Ident::new(span, ident))));
              }
              _ => SemiIdentifierKnowledge::Keyword(Keyword::new(span, S::from(exa.slice()))).into(),
            };

            let err = InvalidFunctionName::with_knowledge(span, err);
            emitter.emit(err.into());

            Ok((
              false,
              Self::new(Ident::new(span, S::from(exa.slice()))),
            ))
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
