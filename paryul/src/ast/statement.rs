#[cfg(feature = "evm")]
use lexsol::yul::EvmBuiltinFunction;
use logosky::{
  chumsky::{delimited::DelimitedByBrace, token::recovery::{emit_error_until_token, emit_until_token}},
  error::{ErrorNode, UnexpectedToken, UnknownLexeme},
  types::Recoverable,
};

use crate::error::{AstLexerErrors, UnknownExpression, UnknownStatement};

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, From)]
enum StatementSyncPointToken<S> {
  #[from(skip)]
  ColonAssign,
  #[from(skip)]
  LBrace,
  #[from(skip)]
  LParen,
  Identifier(SemiIdentifierToken<S>),
}

impl<S> Require<StatementSyncPointToken<S>> for AstToken<S> {
  type Err = Self;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn require(self) -> Result<StatementSyncPointToken<S>, Self::Err>
  where
    Self: Sized,
  {
    Ok(match self {
      AstToken::ColonAssign => StatementSyncPointToken::ColonAssign,
      AstToken::LBrace => StatementSyncPointToken::LBrace,
      AstToken::LParen => StatementSyncPointToken::LParen,
      other => {
        return <AstToken<S> as Require<SemiIdentifierToken<S>>>::require(other)
          .map(From::from);
      },
    })
  }
}


/// The AST for Yul statements.
///
/// See [Yul Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulStatement)
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Statement<S> {
  /// A leave keyword statement
  Leave(Leave),
  /// A break keyword statement
  Break(Break),
  /// A continue keyword statement
  Continue(Continue),
  /// A block statement
  Block(Block<S>),
  /// A variable declaration statement
  VariableDeclaration(VariableDeclaration<S>),
  /// An assignment statement
  Assignment(Assignment<S>),
  /// A function call statement
  FunctionCall(FunctionCall<S>),
  /// An if statement
  IfStatement(IfStatement<S>),
  /// A for statement
  ForStatement(ForStatement<S>),
  /// A switch statement
  SwitchStatement(SwitchStatement<S>),
  /// A function definition statement
  FunctionDefinition(FunctionDefinition<S>),
}

impl<S> Statement<S> {
  /// Attempts to parse a Yul expression with error recovery.
  ///
  /// If the content is not possible to be an expression, returns `None`, and no valid token is consumed.
  pub fn parser_with_recovery<'a, E>()
  -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone + 'a
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
    recursive(|stmt| {
      custom(move |inp| {
        let before = inp.cursor();

        // skip any lexer errors tokens
        let result = inp.parse(
          emit_error_until_token(|Spanned { span, data: tok }, _, emitter| {
            match <AstToken<S> as Require<StatementSyncPointToken<S>>>::require(tok) {
              Ok(sync_tok) => Some(Spanned { span, data: sync_tok }),
              Err(tok) => {
                emitter.emit(
                  UnexpectedToken::expected_one_of_with_found(
                    span,
                    tok,
                    &[
                      SyntaxKind::leave_KW,
                      SyntaxKind::continue_KW,
                      SyntaxKind::break_KW,
                      SyntaxKind::switch_KW,
                      SyntaxKind::function_KW,
                      SyntaxKind::let_KW,
                      SyntaxKind::if_KW,
                      SyntaxKind::for_KW,
                      SyntaxKind::Identifier,
                      SyntaxKind::LBrace,
                      #[cfg(feature = "evm")]
                      SyntaxKind::EvmBuiltinFunctionName,
                    ],
                  )
                  .into(),
                );
                None
              },
            }
          })
          .validate(|(skipped, t), exa, emitter| {
            if skipped > 0 {
              emitter.emit(
                UnknownStatement::from_range(
                  exa.span(),
                  Default::default(), // placeholder
                )
                .into(),
              );
            }
            t
          }),
        )?;

        let start_ckp = inp.save();
        let start_cursor = inp.cursor();

        Ok(match result {
          None => return Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
          Some(Spanned { span, data: tok }) => {
            match tok {
              StatementSyncPointToken::ColonAssign => todo!(),
              StatementSyncPointToken::LParen => todo!(),
              StatementSyncPointToken::LBrace => {
                let block = inp.parse(DelimitedByBrace::recoverable_parser(stmt.clone().repeated().collect()))?.unwrap();
                let (span, statements) = block.into_components();
                Statement::Block(Block::new(span, statements))
              },
              StatementSyncPointToken::Identifier(tok) => match tok {
                SemiIdentifierToken::Leave => {
                  inp.skip();
                  Statement::Leave(Leave::new(span))
                },
                SemiIdentifierToken::Continue => {
                  inp.skip();
                  Statement::Continue(Continue::new(span))
                },
                SemiIdentifierToken::Break => {
                  inp.skip();
                  Statement::Break(Break::new(span))
                },
                SemiIdentifierToken::Switch => todo!(),
                SemiIdentifierToken::Case => todo!(),
                SemiIdentifierToken::Default => todo!(),
                SemiIdentifierToken::Function => todo!(),
                SemiIdentifierToken::Let => todo!(),
                SemiIdentifierToken::If => todo!(),
                SemiIdentifierToken::For => todo!(),
                SemiIdentifierToken::LitBool(lit_bool) => todo!(),
                SemiIdentifierToken::LitDecimal(lit_decimal) => todo!(),
                SemiIdentifierToken::LitHexadecimal(lit_hexadecimal) => todo!(),
                SemiIdentifierToken::Identifier(_) => todo!(),
                #[cfg(feature = "evm")]
                SemiIdentifierToken::EvmBuiltin(evm_builtin_function) => todo!(),
              },
            }
          },
        })
      })
    })
  }
}

// impl<'a, S> Parseable<'a, AstTokenizer<'a, S>, AstToken<S>, AstParserError<'a, S>>
//   for Recoverable<Statement<S>>
// where
//   AstToken<S>: Token<'a>,
//   <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
//   AstTokenizer<'a, S>: LogoStream<'a, AstToken<S>>,
// {
//   fn parser<E>() -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone
//   where
//     Self: Sized + 'a,
//     AstTokenizer<'a, S>: LogoStream<
//         'a,
//         AstToken<S>,
//         Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
//       >,
//     AstToken<S>: Token<'a>,
//     AstParserError<'a, S>: 'a,
//     E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a,
//   {
//     custom(move |inp| {
//       let before = inp.cursor();

//       // skip any lexer errors tokens
//       let result = inp.parse(
//         emit_error_until_token(|tok: Spanned<AstToken<S>>, _, _| {
//           Some(tok)
//         })
//         .validate(|(skipped, t), exa, emitter| {
//           if skipped > 0 {
//             emitter.emit(
//               UnknownStatement::from_range(
//                 exa.span(),
//                 Default::default(), // placeholder
//               )
//               .into(),
//             );
//           }
//           t
//         }),
//       )?;

//       Ok(Self::Node(match result {
//         None => return Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
//         Some(Spanned { span, data: tok }) => {
//           match tok {
//             AstToken::Leave => {
//               inp.skip();
//               Statement::Leave(Leave::new(span))
//             }
//             AstToken::Break => {
//               inp.skip();
//               Statement::Break(Break::new(span))
//             }
//             AstToken::Continue => {
//               inp.skip();
//               Statement::Continue(Continue::new(span))
//             }
//             // function definition
//             AstToken::Function => {
//               // delegate to function definition parser
//               todo!()
//             }
//             // if statement
//             AstToken::If => {
//               // delegate to if statement parser
//               todo!()
//             }
//             // for statement
//             AstToken::For => {
//               // delegate to for statement parser
//               todo!()
//             }
//             // switch statement
//             AstToken::Switch => {
//               // delegate to switch statement parser
//               todo!()
//             }
//             // variable declaration
//             AstToken::Let => {
//               // delegate to variable declaration parser
//               todo!()
//             }
//             // assignment or function call
//             AstToken::Identifier(ident) => {
//               let ck = inp.save();
//               inp.skip();
//               let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
//               // check the next token to determine if it's an assignment or function call
//               match tok {
//                 Some(Lexed::Token(t)) if t.is_colon_assign() || t.is_comma() => {
//                   // assignment
//                   todo!()
//                 }
//                 Some(Lexed::Token(t)) if t.is_l_paren() => {
//                   // function call
//                   todo!()
//                 }
//                 Some(tok) => {
//                   // lexer error or unexpected token
//                   // what to do here?
//                   // try to find a valid function call or assignment start token?
//                   todo!()
//                 }
//                 // eot reached
//                 None => {
//                   let cur = inp.cursor();
//                   inp.skip();
//                   // Change to return a incomplete function call or assignment error?
//                   return Err(UnexpectedEot::eot(inp.span_since(&cur)).into());
//                 }
//               }
//             }
//             // evm function call
//             #[cfg(feature = "evm")]
//             AstToken::EvmBuiltin(fun) => {
//               let ck = inp.save();
//               inp.skip();
//               let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
//               match tok {
//                 Some(Lexed::Token(t)) if t.is_dot() => {
//                   let cur = inp.cursor();
//                   inp.skip();
//                   // this is a path, and path cannot start with evm builtin function
//                   let span = inp.span_since(&cur);
//                   return Err(todo!());
//                 }
//                 Some(Lexed::Token(t)) if t.is_colon_assign() || t.is_comma() => {
//                   // this is an assignment, and evm builtin function cannot be assigned to
//                   let cur = inp.cursor();
//                   inp.skip();
//                   let span = inp.span_since(&cur);
//                   return Err(todo!());
//                 }
//                 Some(t) => {
//                   use logosky::chumsky::delimited::DelimitedByParen;
//                   // this may be a function call, let's the FunctionCall parser handle it.
//                   todo!("delegate to function call parser")
//                 }
//                 None => {
//                   let cur = inp.cursor();
//                   inp.skip();
//                   // Change to return a incomplete function call error?
//                   return Err(UnexpectedEot::eot(inp.span_since(&cur)).into());
//                 }
//               }
//             }
//             // block
//             AstToken::LBrace => {
//               // delegate to block parser
//               todo!()
//             }

//             // Unexpected token
//             _ => unreachable!("unexpected token cannot happen due to is_statement_start filter"),
//           }
//         }
//       }))
//     })
//   }
// }
