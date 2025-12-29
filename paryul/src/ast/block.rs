use logosky::{chumsky::delimited::DelimitedByBrace, error::ErrorNode};

use crate::error::AstLexerErrors;

use super::*;

impl<S> Block<S> {
  /// Attempts to parse a Yul block with error recovery.
  ///
  /// If the content is not possible to be an block, returns `None`, and no valid token is consumed.
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
    Self::parser_with_recovery_inner(Statement::parser_with_recovery())
  }

  /// Attempts to parse a Yul block with error recovery.
  ///
  /// If the content is not possible to be an block, returns `None`, and no valid token is consumed.
  pub(super) fn parser_with_recovery_inner<'a, E>(
    statement_parser: impl Parser<'a, AstTokenizer<'a, S>, Statement<S>, E> + Clone + 'a,
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
    DelimitedByBrace::recoverable_parser(statement_parser.repeated().collect()).map(|db| match db {
      Err(_) => None,
      Ok(block) => {
        let (span, statements) = block.into_components();
        Some(Block::new(span, statements))
      }
    })
  }
}
