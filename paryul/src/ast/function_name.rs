use tokit::{
  Emitter, Lexer, ParseContext, Source, Token as TokenT, error::UnexpectedEot, input::InputRef,
  try_parse_input::ParseAttempt,
};

use lexsol::yul::{
  Yul,
  syntactic::{SyntaxKind, Token},
};

use super::*;

impl<S, Span> FunctionName<S, Span> {
  /// A parser for the Yul function name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn try_parse_yul<'inp, L, Ctx>(
    inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
  ) -> Result<ParseAttempt<Self>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
  where
    L: Lexer<'inp, Span = Span, Token = Token<S>>,
    L::Source: Source<L::Offset, Slice<'inp> = S>,
    <L::Token as TokenT<'inp>>::Kind: From<SyntaxKind>,
    Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
    <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
      From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>,
    S: 'inp,
    Span: tokit::Span<Offset = L::Offset>,
    Token<S>: TokenT<'inp>,
  {
    PathSegment::try_yul_following(inp).map(|res| res.map(|seg| Self::new(seg.into_ident())))
  }
}
