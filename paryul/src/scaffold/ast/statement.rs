use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use lexsol::{yul::YUL, types::keywords::{Break, Continue, Leave}};
use logosky::{
  KeywordToken, Lexed, LogoStream, Logos, PunctuatorToken, Source, Token, chumsky::{
    IterParser, Parseable, Parser, container::Container as ChumskyContainer, extra::ParserExtra, prelude::*, punctuator
  }, error::{UnexpectedEot, UnexpectedToken}, utils::{Spanned, cmp::Equivalent}
};

use crate::SyntaxKind;

/// The AST for [yul-for-statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulForStatement)
pub mod for_statement;
/// The AST for [yul-if-statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulIfStatement)
pub mod if_statement;
/// The AST for [yul-switch-statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulSwitchStatement)
pub mod switch_statement;

/// The AST for [yul-function-call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
pub mod function_call;

/// The AST for [yul-function-definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionDefinition)
pub mod function_definition;

/// The AST for [yul-variable-definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDefinition)
pub mod variable_definition;

/// The AST for [yul-assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
pub mod assignment;

/// The AST for [yul-block](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBlock)
pub mod block;


/// A scaffold AST for Yul statements.
/// 
/// See [Yul Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulStatement)
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, IsVariant, TryUnwrap, Unwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum Statement<VariableDeclaration, Assignment, FunctionCall, IfStatement, ForStatement, SwitchStatement, FunctionDefinition, Container, Lang = YUL> {
  /// Yul leave statement
  Leave(Leave),
  /// Yul break statement
  Break(Break),
  /// Yul continue statement
  Continue(Continue),
  /// Yul block
  Block(block::Block<Self, Container, Lang>),
  /// Yul variable declaration
  #[from(skip)]
  VariableDeclaration(VariableDeclaration),
  /// Yul assignment
  #[from(skip)]
  Assignment(Assignment),
  /// Yul function call
  #[from(skip)]
  FunctionCall(FunctionCall),
  /// Yul if statement
  #[from(skip)]
  IfStatement(IfStatement),
  /// Yul for statement
  #[from(skip)]
  ForStatement(ForStatement),
  /// Yul switch statement
  #[from(skip)]
  SwitchStatement(SwitchStatement),
  /// Yul function definition
  #[from(skip)]
  FunctionDefinition(FunctionDefinition),
}

impl<'a, VariableDeclaration, Assignment, FunctionCall, IfStatement, ForStatement, SwitchStatement, FunctionDefinition, Container, Lang, I, T, Error>
  Parseable<'a, I, T, Error>
  for Statement<VariableDeclaration, Assignment, FunctionCall, IfStatement, ForStatement, SwitchStatement, FunctionDefinition, Container, Lang>
where
  T: KeywordToken<'a> + PunctuatorToken<'a>,
  str: Equivalent<T>,
  VariableDeclaration: Parseable<'a, I, T, Error>,
  Assignment: Parseable<'a, I, T, Error>,
  FunctionCall: Parseable<'a, I, T, Error>,
  IfStatement: Parseable<'a, I, T, Error>,
  ForStatement: Parseable<'a, I, T, Error>,
  SwitchStatement: Parseable<'a, I, T, Error>,
  FunctionDefinition: Parseable<'a, I, T, Error>,
  Container: ChumskyContainer<Self> + 'a,
  Error: From<<T::Logos as Logos<'a>>::Error> + From<UnexpectedEot> + From<UnexpectedToken<'a, T, SyntaxKind>> + 'a,
{
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    recursive(|statement| {
      custom(move |inp| {
        let before = inp.cursor();

        match inp.peek() {
          None => Err(UnexpectedEot::eot(inp.span_since(&before)).into()),
          Some(Lexed::Error(e)) => Err(<Error as core::convert::From<_>>::from(e)),
          Some(Lexed::Token(Spanned { span, data: tok })) => {
            Ok(match () {
              () if Equivalent::equivalent("leave", &tok) => {
                inp.skip();
                Self::Leave(Leave::new(span))
              }
              () if Equivalent::equivalent("break", &tok) => {
                inp.skip();
                Self::Break(Break::new(span))
              }
              () if Equivalent::equivalent("continue", &tok) => {
                inp.skip();
                Self::Continue(Continue::new(span))
              }
              () if tok.is_brace_open() => {
                inp.skip();
                let statements = inp.parse(statement.clone().repeated().collect().then_ignore(punctuator("}", || SyntaxKind::RBrace)))?;
                Self::Block(block::Block::new(inp.span_since(&before), statements))
              }
              _ => {
                inp.parse(
                  choice((
                    VariableDeclaration::parser().map(Self::VariableDeclaration),
                    Assignment::parser().map(Self::Assignment),
                    FunctionCall::parser().map(Self::FunctionCall),
                    IfStatement::parser().map(Self::IfStatement),
                    ForStatement::parser().map(Self::ForStatement),
                    SwitchStatement::parser().map(Self::SwitchStatement),
                    FunctionDefinition::parser().map(Self::FunctionDefinition),
                  ))
                )?
              },
            })
          }
        }
      })
    })
  }
}
