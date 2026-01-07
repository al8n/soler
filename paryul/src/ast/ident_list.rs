use tokit::{error::ErrorNode, parser::Expect, span::Span, types::IdentList};

use crate::{
  error::{AstLexerErrors, AstParserError, SemiIdentifierKnowledge, TrailingComma},
  scaffold::ast::name::Name,
};

use super::*;

use tokit::{
  Accumulator, Emitter, Lexer, ParseContext, ParseInput, Source, TryParseInput,
  emitter::{
    SeparatedEmitter, UnexpectedLeadingSeparatorEmitter, UnexpectedTrailingSeparatorEmitter,
  },
  error::UnexpectedEot,
  input::InputRef,
  parser::SeparatorHandler,
  punct::Dot,
  span::{AsSpan, SimpleSpan},
  token::IdentifierToken,
  try_parse_input::{Accept, Decline, ParseAttempt},
  types::Ident,
  utils::{Owned, Ref},
};

use lexsol::yul::{
  Yul,
  syntactic::{SyntaxKind, Token},
};

// pub fn try_parse_yul_ident<'inp, S, L, Ctx>(
//   inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
// ) -> Result<
//   ParseAttempt<Ident<S, L::Span, Yul<SyntaxKind>>>,
//   <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error,
// >
// where
//   L: Lexer<'inp, Token = Token<S>>,
//   L::Source: Source<L::Offset, Slice<'inp> = S>,
//   L::Token: IdentifierToken<'inp>,
//   Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
//   Ctx::Emitter: SeparatedEmitter<'inp, Dot, L, Yul<SyntaxKind>>
//     + UnexpectedLeadingSeparatorEmitter<'inp, Dot, L, Yul<SyntaxKind>>
//     + UnexpectedTrailingSeparatorEmitter<'inp, Dot, L, Yul<SyntaxKind>>,
//   <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
//     From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>,
//   S: 'inp,
// {
//   inp.try_expect_valid(|t, _| {
//     t.data().is_semi_identifier()
//   })
//   .and_then(|tok| match tok {
//     None => Ok(Decline),
//     Some(tok) => {
//       let (span, tok) = tok.into_components();
//       Ok(Accept(match tok {
//         Token::Identifier(ident) => Ident::new(span, ident),
//         // #[cfg(feature = "evm")]
//         // Token::EvmBuiltin(evm_fn) => {
//         //   let _ = inp.emitter().emit_error(AstParserError::)?;
//         //   Ident::new(
//         //     span,
//         //     evm_fn.into_inner(),
//         //   )
//         // },
//         _ => {
//           let _ = inp.emitter().emit_error(err)?;
//           Ident::error(
//             span,
//           )
//         }
//       }))
//     },
//   })
// }

// pub fn try_parse_yul_ident_list<'inp, S, L, Container, Ctx>(
//   inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
// ) -> Result<ParseAttempt<IdentList<S, L::Span, Container, Yul<SyntaxKind>>>, <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error>
// where
//   L: Lexer<'inp, Token = Token<S>>,
//   L::Source: Source<L::Offset, Slice<'inp> = S>,
//   L::Token: IdentifierToken<'inp>,
//   Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
//   Ctx::Emitter: SeparatedEmitter<'inp, Dot, L, Yul<SyntaxKind>>
//     + UnexpectedLeadingSeparatorEmitter<'inp, Dot, L, Yul<SyntaxKind>>
//     + UnexpectedTrailingSeparatorEmitter<'inp, Dot, L, Yul<SyntaxKind>>,
//   <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
//     From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>>,
//   Container:
//     Default + tokit::container::Container<IdentList<S, L::Span, Container, Yul<SyntaxKind>>> + SeparatorHandler<'inp, L>,
//   S: 'inp,
// {
//   let leading = Ident::try_parse_of(inp)?;

//   match leading {
//     Decline => Ok(Decline),
//     Accept(l) => {
//       let start = l.span_ref().start();
//       let mut container = Container::default();
//       container.push(PathSegment::new(l));

//       PathSegment::try_parse_of
//         .separated_by_dot()
//         .collect_with(container)
//         .spanned()
//         .parse_input(inp)
//         .map(|seg| {
//           let (span, container) = seg.into_components();
//           let span = Span::new(start, span.into_end());
//           Accept(Path::new(span, container))
//         })
//     }
//   }
// }

// impl<S> IdentList<Ident<S>> {
//   /// Creates a parser for an identifier list with error recovery.
//   pub fn parser_with_recovery<'a, E>(
//     f: impl Fn(Span, SemiIdentifierKnowledge<S>) -> AstParserError<'a, S> + Copy + 'a,
//   ) -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone + 'a
//   where
//     S: Clone
//       + ErrorNode
//       + From<<<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>
//       + 'a,
//     AstToken<S>: Token<'a>,
//     <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
//     AstTokenizer<'a, S>: LogoStream<
//         'a,
//         AstToken<S>,
//         Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
//       >,
//     AstParserError<'a, S>: 'a,
//     E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
//   {
//     separated_by::<_, _, _, _, _, Comma, _>(
//       Name::<S>::parser_with_recovery(f).map(Into::into),
//       |t| t.is_comma(),
//       |t| match t {
//         Some(tok) => !tok.is_semi_identifier(),
//         None => true,
//       },
//       || SyntaxKind::Comma,
//       |tok, sep, emitter| {
//         emitter.emit(TrailingComma::from_suffix(tok, *sep.span()).into());
//       },
//     )
//     .map(|idents| IdentList::new(idents.span, idents.data))
//   }
// }
