/// The AST for [yul-path](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulPath)
pub mod path;

/// The AST for [yul-function-call](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionCall)
pub mod function_call;

/// The AST for [yul-function-definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulFunctionDefinition)
pub mod function_definition;

/// The AST for [yul-expression](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulExpression)
pub mod expression;

/// The AST for [yul-statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulSwitchStatement)
pub mod statement;

/// The AST for [yul-variable-definition](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulVariableDefinition)
pub mod variable_definition;

/// The AST for [yul-assignment](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulAssignment)
pub mod assignment;
