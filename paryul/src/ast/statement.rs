use logosky::{
  chumsky::token::recovery::emit_until_token,
  error::UnexpectedToken,
  types::Recoverable,
};

use crate::error::AstLexerErrors;

use super::*;

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



impl<'a, S> Parseable<'a, AstTokenizer<'a, S>, AstToken<S>, AstParserError<'a, S>> for Recoverable<Statement<S>>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  AstTokenizer<'a, S>: LogoStream<'a, AstToken<S>>,
{
  fn parser<E>() -> impl Parser<'a, AstTokenizer<'a, S>, Self, E> + Clone
  where
    Self: Sized + 'a,
    AstTokenizer<'a, S>: LogoStream<'a, AstToken<S>, Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    AstToken<S>: Token<'a>,
    AstParserError<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenizer<'a, S>, Error = AstParserError<'a, S>> + 'a
  {
    custom(move |inp| {
      let before = inp.cursor();

      // skip any lexer errors tokens
      let result = inp.parse(emit_until_token(|tok: &Spanned<AstToken<S>>| {
        if tok.is_statement_start() {
          Ok(())
        } else {
          Err(UnexpectedToken::expected_one_of_with_found(tok.span, tok.data.clone(), &[
            SyntaxKind::leave_KW,
            SyntaxKind::break_KW,
            SyntaxKind::continue_KW,
            SyntaxKind::LBrace,
            SyntaxKind::let_KW,
            SyntaxKind::Identifier,
            SyntaxKind::if_KW,
            SyntaxKind::for_KW,
            SyntaxKind::switch_KW,
            SyntaxKind::function_KW,
          ]).into())
        }
      }))?;

      Ok(match result {
        None => Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
        Some(Spanned { span, data: tok }) => {
          match tok {
            AstToken::Leave => Statement::Leave(Leave::new(span)),
            AstToken::Break => Statement::Break(Break::new(span)),
            AstToken::Continue => Statement::Continue(Continue::new(span)),
            // function definition
            AstToken::Function => {
              // delegate to function definition parser
            },
            // if statement
            AstToken::If => {
              // delegate to if statement parser
            },
            // for statement
            AstToken::For => {
              // delegate to for statement parser
            },
            // switch statement
            AstToken::Switch => {
              // delegate to switch statement parser
            },
            // variable declaration
            AstToken::Let => {
              // delegate to variable declaration parser
            },
            // assignment or function call
            AstToken::Identifier(ident) => {
              let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
              // check the next token to determine if it's an assignment or function call
              match tok {
                Some(Lexed::Token(t)) if t.is_colon_assign() || t.is_comma() => {
                  // assignment
                },
                Some(Lexed::Token(t)) if t.is_l_paren() => {
                  // function call
                },
                Some(tok) => {
                  // lexer error or unexpected token
                  // what to do here?
                  // try to find a valid function call or assignment start token?
                },
                // eot reached
                None => {
                  let cur = inp.cursor();
                  inp.skip();
                  // Change to return a incomplete function call or assignment error?
                  return Err(UnexpectedEot::eot(inp.span_since(&cur)).into());
                },
              }
            },
            // evm function call
            #[cfg(feature = "evm")]
            AstToken::EvmBuiltin(fun) => {
              let tok: Option<Lexed<'_, AstToken<S>>> = inp.peek();
              match tok {
                Some(Lexed::Token(t)) if t.is_dot() => {
                  let cur = inp.cursor();
                  inp.skip();
                  // this is a path, and path cannot start with evm builtin function
                  let span = inp.span_since(&cur);
                  return Err(todo!());
                },
                Some(Lexed::Token(t)) if t.is_colon_assign() || t.is_comma() => {
                  // this is an assignment, and evm builtin function cannot be assigned to
                  let cur = inp.cursor();
                  inp.skip();
                  let span = inp.span_since(&cur);
                  return Err(todo!());
                },
                Some(t) => {
                  // this may be a function call, let's the FunctionCall parser handle it.
                  todo!("delegate to function call parser")
                },
                None => {
                  let cur = inp.cursor();
                  inp.skip();
                  // Change to return a incomplete function call error?
                  return Err(UnexpectedEot::eot(inp.span_since(&cur)).into());
                },
              }
            },
            // block
            AstToken::LBrace => {
              // delegate to block parser
            },

            // Unexpected token
            _ => unreachable!("unexpected token cannot happen due to is_statement_start filter"),
          }
        }
      })
    })
  }
}
