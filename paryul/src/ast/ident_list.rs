use tokit::{
  error::{ErrorNode, Invalid},
  parser::Expect,
  span::Span,
  types::IdentList,
};

use crate::{
  error::{AstLexerErrors, AstParserError, InvalidIdentifierData, TrailingComma},
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
  span::Spanned,
  token::IdentifierToken,
  try_parse_input::{Accept, Decline, ParseAttempt},
  types::Ident,
  utils::{Owned, Ref},
};

use lexsol::yul::{
  Yul,
  syntactic::{SyntaxKind, Token},
};

/// m
pub(crate) fn try_parse_yul_ident<'inp, S, E, L, Ctx>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Yul<SyntaxKind>>,
) -> Result<
  ParseAttempt<Ident<S, L::Span, Yul<SyntaxKind>>>,
  <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error,
>
where
  L: Lexer<'inp, Token = Token<S>>,
  L::Source: Source<L::Offset, Slice<'inp> = S>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseContext<'inp, L, Yul<SyntaxKind>>,
  <Ctx::Emitter as Emitter<'inp, L, Yul<SyntaxKind>>>::Error:
    From<UnexpectedEot<L::Offset, Yul<SyntaxKind>>> + From<Invalid<E, L::Span, Yul<SyntaxKind>>>,
  E: From<InvalidIdentifierData>,
  S: 'inp,
{
  let tok = inp.try_expect_valid(|tok, emitter| {
    #[cfg_attr(not(tarpaulin), inline(always))]
    fn invalid_path_segment<Span, E>(span: Span, data: E) -> Invalid<E, Span, Yul<SyntaxKind>> {
      Invalid::with_data_of(span, data)
    }

    let (span, tok) = tok.into_components();

    macro_rules! emit_ret {
      ($data:expr) => {{
        emitter.emit_error(Spanned::new(
          span.clone(),
          Invalid::with_data_of(span.clone(), $data).into(),
        ))?;
        true
      }};
      (@kw($name:literal)) => {
        emit_ret!(E::from(InvalidIdentifierData::Keyword($name)))
      };
      (@lit_bool($val:expr)) => {
        emit_ret!(E::from(InvalidIdentifierData::LitBool($val)))
      };
      (@evm_builtin($val:expr)) => {
        emit_ret!(E::from(InvalidIdentifierData::EvmBuiltinFunction($val)))
      };
    }

    Ok(match tok {
      Token::Identifier(_) => true,
      Token::Leave => emit_ret!(@kw("leave")),
      Token::Continue => emit_ret!(@kw("continue")),
      Token::Break => emit_ret!(@kw("break")),
      Token::Switch => emit_ret!(@kw("switch")),
      Token::Case => emit_ret!(@kw("case")),
      Token::Default => emit_ret!(@kw("default")),
      Token::Function => emit_ret!(@kw("function")),
      Token::Let => emit_ret!(@kw("let")),
      Token::If => emit_ret!(@kw("if")),
      Token::For => emit_ret!(@kw("for")),
      Token::Lit(lexsol::yul::Lit::Boolean(lit)) => emit_ret!(@lit_bool(lit.unit())),
      #[cfg(feature = "evm")]
      Token::EvmBuiltin(e) => emit_ret!(@evm_builtin(e.unit())),
      _ => false,
    })
  })?;

  match tok {
    None => Ok(Decline),
    Some(t) => {
      let (span, tok) = t.into_components();

      Ok(Accept(match tok {
        Token::Identifier(ident) => Ident::new(span, ident),
        Token::Leave
        | Token::Continue
        | Token::Break
        | Token::Switch
        | Token::Case
        | Token::Default
        | Token::Function
        | Token::Let
        | Token::If
        | Token::For => {
          let mut ident = Ident::new(span, inp.slice());
          ident.mark_error();
          ident
        }
        #[cfg(feature = "evm")]
        Token::EvmBuiltin(evm_fn) => {
          let mut ident = Ident::new(span, evm_fn.into_inner());
          ident.mark_error();
          ident
        }
        Token::Lit(lit) => {
          let mut ident = Ident::new(span, lit.into_data());
          ident.mark_error();
          ident
        }
        _ => unreachable!("token has been validated"),
      }))
    }
  }
}

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

#[test]
fn t() {}
