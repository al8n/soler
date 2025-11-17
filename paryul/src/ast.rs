use crate::{SyntaxKind, YUL, error::AstParserError, scaffold::ast};

use derive_more::{From, IsVariant, TryUnwrap, Unwrap};

use lexsol::types::keywords::{Break, Continue, Leave};
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
mod path;
mod statement;

/// The token type for Yul AST nodes.
pub type AstToken<S> = lexsol::yul::syntactic::Token<S>;
/// The tokenizer type for Yul AST nodes.
pub type AstTokenizer<'a, S> = lexsol::yul::syntactic::Lexer<'a, S>;

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
pub type FunctionCallName<S> = ast::statement::function_call::FunctionCallName<S>;

/// The function call type for Yul.
///
/// Spec: [Yul Function Call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
pub type FunctionCall<S> =
  ast::statement::function_call::FunctionCall<FunctionCallName<S>, Expression<S>>;

/// The single-target assignment type for Yul.
///
/// Spec: [Yul Single Target Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
pub type SingleTargetAssignment<S> =
  ast::statement::assignment::SingleTargetAssignment<Path<S>, Expression<S>>;

/// The multi-target assignment type for Yul.
///
/// Spec: [Yul Multiple Target Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
pub type MultipleTargetAssignment<S> =
  ast::statement::assignment::MultipleTargetAssignment<Path<S>, Expression<S>>;

/// The assignment type for Yul.
///
/// Spec: [Yul Assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
pub type Assignment<S> =
  ast::statement::assignment::Assignment<Path<S>, Expression<S>, FunctionCall<S>>;

/// The single variable declaration type for Yul.
///
/// Spec: [Yul Variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
pub type SingleVariableDeclaration<S> =
  ast::statement::variable_declaration::SingleVariableDeclaration<Ident<S>, Expression<S>>;

/// The multiple variable declaration type for Yul.
///
/// Spec: [Yul Variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
pub type MultipleVariableDeclaration<S> =
  ast::statement::variable_declaration::MultipleVariableDeclaration<
    FunctionCallName<S>,
    FunctionCall<S>,
  >;

/// The variable declaration type for Yul.
///
/// Spec: [Yul Variable Declaration](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDeclaration)
pub type VariableDeclaration<S> = ast::statement::variable_declaration::VariableDeclaration<
  FunctionCallName<S>,
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
  FunctionCallName<S>,
  FunctionDefinitionArguments<S>,
  FunctionDefinitionReturnParameters<S>,
  Block<S>,
>;
