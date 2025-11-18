use logosky::error::ErrorNode;

use crate::{
  error::{AstLexerErrors, InvalidVariableName},
  scaffold::ast::name::Name,
};

use super::*;

impl<S> VariableName<S> {
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
    Name::<S>::parser_with_recovery(|span, k| {
      InvalidVariableName::with_knowledge(span, k.into()).into()
    })
    .map(|name| Self::new(name.into()))
  }
}
