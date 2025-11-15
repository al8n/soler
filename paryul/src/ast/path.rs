// use derive_more::TryUnwrapError;
// use either::Either;
// use logosky::{chumsky::token::recovery::emit_with, types::Recoverable};


// use crate::error::AstLexerErrors;

// use super::*;

// impl<'a, S> Parseable<'a, AstTokenizer<'a, S>, AstToken<S>, AstParserError<'a, S>> for Recoverable<Path<S>>
// where
//   AstToken<S>: Token<'a>,
//   <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
//   AstTokenizer<'a, S>: LogoStream<'a, AstToken<S>>,
// {
//   fn parser<E>() -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone
//   where
//     Self: Sized + 'a,
//     AstTokenizer<'a, S>: LogoStream<'a, AstToken<S>, Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//     AstToken<S>: Token<'a>,
//     AstParserError<'a, S>: 'a,
//     E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a
//   {
//     custom(|inp| {
//       let before = inp.cursor();
//       loop {
//         let res = inp.parse(any().validate(|tok: Lexed<'_, AstToken<S>>, exa, emitter| {
//           match tok {
//             Lexed::Error(e) => {
//               emitter.emit(AstParserError::Lexer(e));
//               None
//             }
//             Lexed::Token(Spanned { span, data: tok}) => {
//               // match tok.try_unwrap_identifier() {
//               //   Ok(ident) => Either::Left(Ident::new(span, ident)),
//               //   Err(TryUnwrapError { input: tok, .. }) => {
//               //     emitter.emit(UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::Identifier).into());
//               //     Either::Right(false)
//               //   },
//               // }
//               match tok {
//                 AstToken::Identifier(ident) => Some(Ident::new(span, ident)),
//                 AstToken::Dot => {
//                   emitter.emit(UnexpectedToken::expected_one_with_found(span, tok, SyntaxKind::Identifier).into());
//                   None
//                 }
//               }
//             }
//           }
//         }))?;
//         match res {
//           // we got an error, try again
//           Either::Right(true) => continue,
//           Either::Right(false) => {},
//           Either::Left(ident) => todo!(),
//         }
//       }
//     })
//   }
// }