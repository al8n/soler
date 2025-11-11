use core::marker::PhantomData;

use lexsol::{types::punct::Dot, yul::{lossless, syntactic}};
#[cfg(feature = "evm")]
use lexsol::yul::EvmBuiltinFunction;
#[cfg(feature = "evm")]
use logosky::utils::Spanned;

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};

use logosky::{
  IdentifierToken, KeywordToken, Lexed, LogoStream, Logos, PunctuatorToken, Source, State, Token, chumsky::{
    self, IterParser, Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra, prelude::*,
  }, utils::{Span, recursion_tracker::RecursionLimiter}
};

use super::super::Ident;

use crate::{SyntaxKind, error::{self, *}};

/// A segment of a path.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment<S>(Ident<S>);

impl<'a, S, I, T, Error> Parseable<'a, I, T, Error> for PathSegment<S>
where
  T: IdentifierToken<'a> + KeywordToken<'a>,
  T::Logos: Logos<'a, Error = LexerErrors<T::Char, <<T::Logos as Logos<'a>>::Extras as State>::Error>>,
  <T::Logos as Logos<'a>>::Extras: State,
  <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
  Error: From<error::Error<T, SyntaxKind, T::Char, <<T::Logos as Logos<'a>>::Extras as State>::Error>> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a
  {
    custom(|inp| {
      let before = inp.cursor();
      let tok: Option<Lexed<'_, T>> = inp.next();
      match tok {
        None => Err(error::Error::eot(inp.span_since(&before)).into()),
        Some(Lexed::Error(err)) => Err(error::Error::Lexer(err).into()),
        Some(Lexed::Token(Spanned { span, data: tok, })) => {
          let ident = match tok.try_into_identifier() {
            Ok(ident) => Ident::new(span, ident),
            Err(tok) => {
              return Err(error::Error::unexpected_token(span, tok, SyntaxKind::Identifier).into());
            }
          };


            // #[cfg(feature = "evm")]
            // syntactic::Token::EvmBuiltin(_) => {
            //   Ident::new(inp.span_since(&before), inp.slice(&before..&inp.cursor()))
            // }

          Ok(PathSegment(ident))
        },
      }
    })
  }
}



/// A path.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Path<S, Container = Vec<Ident<S>>> {
  span: Span,
  segments: Container,
  _m: PhantomData<Ident<S>>,
}

// impl<'a, S, Container, I> Parseable<'a, I, syntactic::Token<S>, SyntacticParserError<'a, S>> for Path<S, Container>
// where
//   Container: ChumskyContainer<Ident<S>> + 'a,
//   syntactic::Token<S>: Token<'a>,
//   <syntactic::Token<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>, Extras = RecursionLimiter>,
//   <<syntactic::Token<S> as Token<'a>>::Logos as Logos<'a>>::Source: Source<Slice<'a> = S>,
// {
//   fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
//   where
//     Self: Sized + 'a,
//     I: LogoStream<'a, syntactic::Token<S>, Slice = <<<syntactic::Token<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//     E: ParserExtra<'a, I, Error = SyntacticParserError<'a, S>> + 'a,
//   {
//     custom(|inp| {
//       let before = inp.cursor();
      
//       match inp.next() {
//         None => Err(SyntacticParserError::eot(inp.span_since(&before))),
//         Some(Lexed::Error(err)) => Err(SyntacticParserError::Lexer(err)),
//         Some(Lexed::Token(Spanned { span, data: tok, })) => {
//           let ident = match tok {
//             syntactic::Token::Identifier(ident) => {
//               Ident::new(span, ident)
//             },
//             _ => {
//               return Err(SyntacticParserError::unexpected_token(span, tok, SyntaxKind::Identifier));
//             }
//           };

//             // #[cfg(feature = "evm")]
//             // syntactic::Token::EvmBuiltin(_) => {
//             //   Ident::new(inp.span_since(&before), inp.slice(&before..&inp.cursor()))
//             // }

//           todo!()
//         },
//       }
//     })
//   }
// }
