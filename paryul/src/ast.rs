use crate::{SyntaxKind, YUL, error::AstParserError, scaffold::ast};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};

use lexsol::types::{
  LitBool, LitDecimal, LitHexadecimal, LitNumber,
  keywords::{Break, Continue, Leave},
};
use logosky::{
  IdentifierToken, KeywordToken, Lexed, LitToken, LogoStream, Logos, PunctuatorToken, Require,
  Source, Token,
  chumsky::{Parseable, Parser, extra::ParserExtra, prelude::*, token::punct::brace_close},
  error::{UnexpectedEot, UnexpectedToken},
  utils::{Spanned, cmp::Equivalent},
};

pub use expression::Expression;
pub use lexsol::yul::Lit;
pub use statement::Statement;

mod assignment;
mod expression;
mod function_call;
mod function_name;
mod ident_list;
mod name;
mod path;
mod statement;
mod variable_declaration;
mod variable_name;

/// The token type for Yul AST nodes.
pub type AstToken<S> = lexsol::yul::syntactic::Token<S>;
/// The tokenizer type for Yul AST nodes.
pub type AstTokenizer<'a, S> = lexsol::yul::syntactic::Lexer<'a, S>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, TryUnwrap)]
enum SemiIdentifierToken<S> {
  Leave,
  Continue,
  Break,
  Switch,
  Case,
  Default,
  Function,
  Let,
  If,
  For,
  LitBool(LitBool<S>),
  LitDecimal(LitDecimal<S>),
  LitHexadecimal(LitHexadecimal<S>),
  #[cfg(feature = "evm")]
  EvmBuiltin(lexsol::yul::EvmBuiltinFunction),
  Identifier(S),
}

impl<S: Clone> SemiIdentifierToken<S> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn to_token(&self) -> AstToken<S> {
    match self {
      Self::Leave => AstToken::Leave,
      Self::Continue => AstToken::Continue,
      Self::Break => AstToken::Break,
      Self::Switch => AstToken::Switch,
      Self::Case => AstToken::Case,
      Self::Default => AstToken::Default,
      Self::Function => AstToken::Function,
      Self::Let => AstToken::Let,
      Self::If => AstToken::If,
      Self::For => AstToken::For,
      Self::LitBool(lit) => AstToken::Lit(Lit::Boolean(lit.clone())),
      Self::LitDecimal(lit) => AstToken::Lit(Lit::Number(LitNumber::Decimal(lit.clone()))),
      Self::LitHexadecimal(lit) => AstToken::Lit(Lit::Number(LitNumber::Hexadecimal(lit.clone()))),
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(name) => AstToken::EvmBuiltin(*name),
      Self::Identifier(ident) => AstToken::Identifier(ident.clone()),
    }
  }
}

impl<S> Require<SemiIdentifierToken<S>> for AstToken<S> {
  type Err = Self;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn require(self) -> Result<SemiIdentifierToken<S>, Self::Err>
  where
    Self: Sized,
  {
    Ok(match self {
      Self::Leave => SemiIdentifierToken::Leave,
      Self::Continue => SemiIdentifierToken::Continue,
      Self::Break => SemiIdentifierToken::Break,
      Self::Switch => SemiIdentifierToken::Switch,
      Self::Case => SemiIdentifierToken::Case,
      Self::Default => SemiIdentifierToken::Default,
      Self::Function => SemiIdentifierToken::Function,
      Self::Let => SemiIdentifierToken::Let,
      Self::If => SemiIdentifierToken::If,
      Self::For => SemiIdentifierToken::For,
      Self::Lit(lit) => match lit {
        Lit::Boolean(val) => SemiIdentifierToken::LitBool(val),
        Lit::Number(val) => match val {
          LitNumber::Decimal(val) => SemiIdentifierToken::LitDecimal(val),
          LitNumber::Hexadecimal(val) => SemiIdentifierToken::LitHexadecimal(val),
          lit => return Err(Self::Lit(Lit::Number(lit))),
        },
        lit => return Err(Self::Lit(lit)),
      },
      Self::Identifier(ident) => SemiIdentifierToken::Identifier(ident),
      #[cfg(feature = "evm")]
      Self::EvmBuiltin(val) => SemiIdentifierToken::EvmBuiltin(val),
      tok => return Err(tok),
    })
  }
}

/// The identifier type for Yul.
///
/// Spec: [Yul Identifier](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityLexer.YulIdentifier)
pub type Ident<S> = logosky::types::Ident<S, YUL>;

/// The path segment type for Yul.
pub type PathSegment<S> = ast::path::PathSegment<S>;

/// The path type of Yul.
///
/// Spec: [Yul Path](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulPath)
pub type Path<S> = ast::path::Path<PathSegment<S>>;

/// The function call name type for Yul.
///
/// Spec: [Yul Function Call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
pub type FunctionName<S> = ast::statement::function_call::FunctionName<S>;

/// The function call type for Yul.
///
/// Spec: [Yul Function Call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
pub type FunctionCall<S> =
  ast::statement::function_call::FunctionCall<FunctionName<S>, Expression<S>>;

/// The single-target assignment type for Yul.
///
/// Spec: [Yul Single Target Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
pub type SingleTargetAssignment<S> =
  ast::statement::assignment::SingleTargetAssignment<Path<S>, Expression<S>>;

/// The multi-target assignment type for Yul.
///
/// Spec: [Yul Multiple Target Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
pub type MultipleTargetsAssignment<S> =
  ast::statement::assignment::MultipleTargetsAssignment<Path<S>, FunctionCall<S>>;

/// The assignment type for Yul.
///
/// Spec: [Yul Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
pub type Assignment<S> =
  ast::statement::assignment::Assignment<Path<S>, Expression<S>, FunctionCall<S>>;

/// The variable name type for Yul.
///
/// Spec: [Yul Variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
pub type VariableName<S> = ast::statement::variable_declaration::VariableName<S>;

/// The single variable declaration type for Yul.
///
/// Spec: [Yul Variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
pub type SingleVariableDeclaration<S> =
  ast::statement::variable_declaration::SingleVariableDeclaration<VariableName<S>, Expression<S>>;

/// The multiple variable declaration type for Yul.
///
/// Spec: [Yul Variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
pub type MultipleVariablesDeclaration<S> =
  ast::statement::variable_declaration::MultipleVariablesDeclaration<
    VariableName<S>,
    FunctionCall<S>,
  >;

/// The variable declaration type for Yul.
///
/// Spec: [Yul Variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
pub type VariableDeclaration<S> = ast::statement::variable_declaration::VariableDeclaration<
  VariableName<S>,
  Expression<S>,
  FunctionCall<S>,
>;

/// The block type for Yul.
///
/// Spec: [Yul Block](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBlock)
pub type Block<S> = ast::statement::block::Block<Statement<S>>;

/// The if statement type for Yul.
///
/// Spec: [Yul If Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulIfStatement)
pub type IfStatement<S> = ast::statement::if_statement::IfStatement<Expression<S>, Block<S>>;

/// The for statement type for Yul.
///
/// Spec: [Yul For Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulForStatement)
pub type ForStatement<S> =
  ast::statement::for_statement::ForStatement<Block<S>, Expression<S>, Block<S>, Block<S>>;

/// The switch case type of a switch statement for Yul.
///
/// Spec: [Yul Switch Case](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulSwitchStatement)
pub type SwitchCase<S> = ast::statement::switch_statement::SwitchCase<Lit<S>, Block<S>>;

/// The default case type of a switch statement for Yul.
///
/// Spec: [Yul Default Case](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulSwitchStatement)
pub type DefaultCase<S> = ast::statement::switch_statement::DefaultCase<Block<S>>;

/// The case type of a switch statement for Yul.
///
/// Spec: [Yul Switch Case](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulSwitchStatement)
pub type Case<S> = ast::statement::switch_statement::Case<Lit<S>, Block<S>>;

/// The switch statement type for Yul.
///
/// Spec: [Yul Switch Statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulSwitchStatement)
pub type SwitchStatement<S> =
  ast::statement::switch_statement::SwitchStatement<Expression<S>, Case<S>>;

/// The arguments type for Yul function definition.
///
/// Spec: [Yul Function Definition Arguments](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionDefinition)
pub type FunctionDefinitionArguments<S> =
  ast::statement::function_definition::FunctionDefinitionArguments<Ident<S>>;

/// The return parameters type for Yul function definition.
///
/// Spec: [Yul Function Definition Return Parameters](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionDefinition)
pub type FunctionDefinitionReturnParameters<S> =
  ast::statement::function_definition::FunctionDefinitionReturnParameters<Ident<S>>;

/// The function definition type for Yul.
///
/// Spec: [Yul Function Definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionDefinition)
pub type FunctionDefinition<S> = ast::statement::function_definition::FunctionDefinition<
  FunctionName<S>,
  FunctionDefinitionArguments<S>,
  FunctionDefinitionReturnParameters<S>,
  Block<S>,
>;
