use logosky::Logos;

use super::Lit;

#[cfg(feature = "evm")]
use super::super::EvmBuiltinFunction;

#[derive(Logos)]
#[logos(crate = logosky::logos)]
#[logos(skip r"[ \t\r\n\u{000C}]+|//[^\r\n]*|/\*([^*]|\*+[^*/])*\*+/")]
#[logos(subpattern escape_sequence = r#"\\['"\\nrt\n\r]|\\u[0-9a-fA-F]{4}|\\x[0-9a-fA-F]{2}"#)]
#[logos(subpattern double_quoted_printable = "[\u{0020}-\u{0021}\u{0023}-\u{005B}\u{005D}-\u{007E}]")]
#[logos(subpattern single_quoted_printable = "[\u{0020}-\u{0026}\u{0028}-\u{005B}\u{005D}-\u{007E}]")]
#[logos(subpattern double_quoted_char = "(?&double_quoted_printable)|(?&escape_sequence)")]
#[logos(subpattern single_quoted_char = "(?&single_quoted_printable)|(?&escape_sequence)")]
#[logos(subpattern double_quoted_chars = r#"(?&double_quoted_char)+"#)]
#[logos(subpattern single_quoted_chars = r#"(?&single_quoted_char)+"#)]
#[logos(subpattern decimal = "0|[1-9][0-9]*")]
#[logos(subpattern hexadecimal = "0x[0-9a-fA-F]+")]
pub enum Token<'a> {
  #[token(":=")]
  Assign,
  #[token("->")]
  ThinArrow,
  #[token("{")]
  LBrace,
  #[token("}")]
  RBrace,
  #[token("(")]
  LParen,
  #[token(")")]
  RParen,
  #[token(".")]
  Dot,
  #[token(",")]
  Comma,

  #[token("leave")]
  Leave,
  #[token("continue")]
  Continue,
  #[token("break")]
  Break,
  #[token("switch")]
  Switch,
  #[token("case")]
  Case,
  #[token("default")]
  Default,
  #[token("function")]
  Function,
  #[token("let")]
  Let,
  #[token("if")]
  If,
  #[token("for")]
  For,

  #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*")]
  Identifier(&'a str),

  #[token("true", |lexer| Lit::lit_true(lexer.slice()))]
  #[token("false", |lexer| Lit::lit_false(lexer.slice()))]
  #[regex("(?&decimal)", |lexer| Lit::lit_decimal(lexer.slice()))]
  #[regex("(?&hexadecimal)", |lexer| Lit::lit_hexadecimal(lexer.slice()))]
  #[regex("hex\"(?:[0-9A-Fa-f]{2}(?:_[0-9A-Fa-f]{2})*)\"", |lexer| Lit::lit_double_quoted_hex_string(lexer.slice()))]
  #[regex("hex'(?:[0-9A-Fa-f]{2}(?:_[0-9A-Fa-f]{2})*)'", |lexer| Lit::lit_single_quoted_hex_string(lexer.slice()))]
  #[regex(r#""(?&double_quoted_chars)""#, |lexer| Lit::lit_double_quoted_non_empty_string(lexer.slice()))]
  #[regex(r"'(?&single_quoted_chars)'", |lexer| Lit::lit_single_quoted_non_empty_string(lexer.slice()))]
  Lit(Lit<&'a str>),

  #[cfg(feature = "evm")]
  #[token("stop", |_| EvmBuiltinFunction::Stop)]
  #[token("add", |_| EvmBuiltinFunction::Add)]
  #[token("sub", |_| EvmBuiltinFunction::Sub)]
  #[token("mul", |_| EvmBuiltinFunction::Mul)]
  #[token("div", |_| EvmBuiltinFunction::Div)]
  #[token("sdiv", |_| EvmBuiltinFunction::Sdiv)]
  #[token("mod", |_| EvmBuiltinFunction::Mod)]
  #[token("smod", |_| EvmBuiltinFunction::Smod)]
  #[token("exp", |_| EvmBuiltinFunction::Exp)]
  #[token("not", |_| EvmBuiltinFunction::Not)]
  #[token("lt", |_| EvmBuiltinFunction::Lt)]
  #[token("gt", |_| EvmBuiltinFunction::Gt)]
  #[token("slt", |_| EvmBuiltinFunction::Slt)]
  #[token("sgt", |_| EvmBuiltinFunction::Sgt)]
  #[token("eq", |_| EvmBuiltinFunction::Eq)]
  #[token("iszero", |_| EvmBuiltinFunction::Iszero)]
  #[token("and", |_| EvmBuiltinFunction::And)]
  #[token("or", |_| EvmBuiltinFunction::Or)]
  #[token("xor", |_| EvmBuiltinFunction::Xor)]
  #[token("byte", |_| EvmBuiltinFunction::Byte)]
  #[token("shl", |_| EvmBuiltinFunction::Shl)]
  #[token("shr", |_| EvmBuiltinFunction::Shr)]
  #[token("sar", |_| EvmBuiltinFunction::Sar)]
  #[token("clz", |_| EvmBuiltinFunction::Clz)]
  #[token("addmod", |_| EvmBuiltinFunction::Addmod)]
  #[token("mulmod", |_| EvmBuiltinFunction::Mulmod)]
  #[token("signextend", |_| EvmBuiltinFunction::Signextend)]
  #[token("keccak256", |_| EvmBuiltinFunction::Keccak256)]
  #[token("pop", |_| EvmBuiltinFunction::Pop)]
  #[token("mload", |_| EvmBuiltinFunction::Mload)]
  #[token("mstore", |_| EvmBuiltinFunction::Mstore)]
  #[token("mstore8", |_| EvmBuiltinFunction::Mstore8)]
  #[token("sload", |_| EvmBuiltinFunction::Sload)]
  #[token("sstore", |_| EvmBuiltinFunction::Sstore)]
  #[token("tload", |_| EvmBuiltinFunction::Tload)]
  #[token("tstore", |_| EvmBuiltinFunction::Tstore)]
  #[token("msize", |_| EvmBuiltinFunction::Msize)]
  #[token("gas", |_| EvmBuiltinFunction::Gas)]
  #[token("address", |_| EvmBuiltinFunction::Address)]
  #[token("balance", |_| EvmBuiltinFunction::Balance)]
  #[token("selfbalance", |_| EvmBuiltinFunction::Selfbalance)]
  #[token("caller", |_| EvmBuiltinFunction::Caller)]
  #[token("callvalue", |_| EvmBuiltinFunction::Callvalue)]
  #[token("calldataload", |_| EvmBuiltinFunction::Calldataload)]
  #[token("calldatasize", |_| EvmBuiltinFunction::Calldatasize)]
  #[token("calldatacopy", |_| EvmBuiltinFunction::Calldatacopy)]
  #[token("extcodesize", |_| EvmBuiltinFunction::Extcodesize)]
  #[token("extcodecopy", |_| EvmBuiltinFunction::Extcodecopy)]
  #[token("returndatasize", |_| EvmBuiltinFunction::Returndatasize)]
  #[token("returndatacopy", |_| EvmBuiltinFunction::Returndatacopy)]
  #[token("mcopy", |_| EvmBuiltinFunction::Mcopy)]
  #[token("extcodehash", |_| EvmBuiltinFunction::Extcodehash)]
  #[token("create", |_| EvmBuiltinFunction::Create)]
  #[token("create2", |_| EvmBuiltinFunction::Create2)]
  #[token("call", |_| EvmBuiltinFunction::Call)]
  #[token("callcode", |_| EvmBuiltinFunction::Callcode)]
  #[token("delegatecall", |_| EvmBuiltinFunction::Delegatecall)]
  #[token("staticcall", |_| EvmBuiltinFunction::Staticcall)]
  #[token("return", |_| EvmBuiltinFunction::Return)]
  #[token("revert", |_| EvmBuiltinFunction::Revert)]
  #[token("selfdestruct", |_| EvmBuiltinFunction::Selfdestruct)]
  #[token("invalid", |_| EvmBuiltinFunction::Invalid)]
  #[token("log0", |_| EvmBuiltinFunction::Log0)]
  #[token("log1", |_| EvmBuiltinFunction::Log1)]
  #[token("log2", |_| EvmBuiltinFunction::Log2)]
  #[token("log3", |_| EvmBuiltinFunction::Log3)]
  #[token("log4", |_| EvmBuiltinFunction::Log4)]
  #[token("chainid", |_| EvmBuiltinFunction::Chainid)]
  #[token("origin", |_| EvmBuiltinFunction::Origin)]
  #[token("gasprice", |_| EvmBuiltinFunction::Gasprice)]
  #[token("blockhash", |_| EvmBuiltinFunction::Blockhash)]
  #[token("blobhash", |_| EvmBuiltinFunction::Blobhash)]
  #[token("coinbase", |_| EvmBuiltinFunction::Coinbase)]
  #[token("timestamp", |_| EvmBuiltinFunction::Timestamp)]
  #[token("number", |_| EvmBuiltinFunction::Number)]
  #[token("difficulty", |_| EvmBuiltinFunction::Difficulty)]
  #[token("prevrandao", |_| EvmBuiltinFunction::Prevrandao)]
  #[token("gaslimit", |_| EvmBuiltinFunction::Gaslimit)]
  #[token("basefee", |_| EvmBuiltinFunction::Basefee)]
  #[token("blobbasefee", |_| EvmBuiltinFunction::Blobbasefee)]
  EvmBuilin(EvmBuiltinFunction),
}
