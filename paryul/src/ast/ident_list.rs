use lexsol::types::punct::Comma;
use logosky::{chumsky::separated::separated_by, error::ErrorNode, utils::Span};

use crate::{
  error::{AstLexerErrors, SemiIdentifierKnowledge, TrailingComma},
  scaffold::ast::{ident_list::IdentList, name::Name},
};

use super::*;

impl<S> IdentList<Ident<S>> {
  /// Creates a parser for an identifier list with error recovery.
  pub fn parser_with_recovery<'a, E>(
    f: impl Fn(Span, SemiIdentifierKnowledge<S>) -> AstParserError<'a, S> + Copy + 'a,
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
    separated_by::<_, _, _, _, _, Comma, _>(
      Name::<S>::parser_with_recovery(f).map(Into::into),
      |t| t.is_comma(),
      |t| match t {
        Some(tok) => !tok.is_semi_identifier(),
        None => true,
      },
      || SyntaxKind::Comma,
      |tok, sep, emitter| {
        emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into());
      },
    )
    .map(|idents| IdentList::new(idents.span, idents.data))
  }
}
