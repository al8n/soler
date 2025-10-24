use logosky::Logos;

use super::super::{BuiltinFunction, Lit, LitBool, LitHexStr, LitNonEmptyStr, LitNumber, LitStr};

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
pub enum Token<'a> {
  #[token(":=")]
  Assign,
  #[token("->")]
  Arrow,
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
  #[regex("0|[1-9][0-9]*", |lexer| Lit::lit_decimal(lexer.slice()))]
  #[regex("0x[0-9a-fA-F]+", |lexer| Lit::lit_hexadecimal(lexer.slice()))]
  #[regex("hex\"(?:[0-9A-Fa-f]{2}(?:_[0-9A-Fa-f]{2})*)\"", |lexer| Lit::lit_double_quoted_hex_string(lexer.slice()))]
  #[regex("hex'(?:[0-9A-Fa-f]{2}(?:_[0-9A-Fa-f]{2})*)'", |lexer| Lit::lit_single_quoted_hex_string(lexer.slice()))]
  #[regex(r#""(?&double_quoted_chars)""#, |lexer| Lit::lit_double_quoted_non_empty_string(lexer.slice()))]
  #[regex(r"'(?&single_quoted_chars)'", |lexer| Lit::lit_single_quoted_non_empty_string(lexer.slice()))]
  Lit(Lit<&'a str>),

  #[token("stop", |_| BuiltinFunction::Stop)]
  #[token("add", |_| BuiltinFunction::Add)]
  #[token("sub", |_| BuiltinFunction::Sub)]
  #[token("mul", |_| BuiltinFunction::Mul)]
  #[token("div", |_| BuiltinFunction::Div)]
  #[token("sdiv", |_| BuiltinFunction::Sdiv)]
  #[token("mod", |_| BuiltinFunction::Mod)]
  #[token("smod", |_| BuiltinFunction::Smod)]
  #[token("exp", |_| BuiltinFunction::Exp)]
  #[token("not", |_| BuiltinFunction::Not)]
  #[token("lt", |_| BuiltinFunction::Lt)]
  #[token("gt", |_| BuiltinFunction::Gt)]
  #[token("slt", |_| BuiltinFunction::Slt)]
  #[token("sgt", |_| BuiltinFunction::Sgt)]
  #[token("eq", |_| BuiltinFunction::Eq)]
  #[token("iszero", |_| BuiltinFunction::Iszero)]
  #[token("and", |_| BuiltinFunction::And)]
  #[token("or", |_| BuiltinFunction::Or)]
  #[token("xor", |_| BuiltinFunction::Xor)]
  #[token("byte", |_| BuiltinFunction::Byte)]
  #[token("shl", |_| BuiltinFunction::Shl)]
  #[token("shr", |_| BuiltinFunction::Shr)]
  #[token("sar", |_| BuiltinFunction::Sar)]
  #[token("clz", |_| BuiltinFunction::Clz)]
  #[token("addmod", |_| BuiltinFunction::Addmod)]
  #[token("mulmod", |_| BuiltinFunction::Mulmod)]
  #[token("signextend", |_| BuiltinFunction::Signextend)]
  #[token("keccak256", |_| BuiltinFunction::Keccak256)]
  #[token("pop", |_| BuiltinFunction::Pop)]
  #[token("mload", |_| BuiltinFunction::Mload)]
  #[token("mstore", |_| BuiltinFunction::Mstore)]
  #[token("mstore8", |_| BuiltinFunction::Mstore8)]
  #[token("sload", |_| BuiltinFunction::Sload)]
  #[token("sstore", |_| BuiltinFunction::Sstore)]
  #[token("tload", |_| BuiltinFunction::Tload)]
  #[token("tstore", |_| BuiltinFunction::Tstore)]
  #[token("msize", |_| BuiltinFunction::Msize)]
  #[token("gas", |_| BuiltinFunction::Gas)]
  #[token("address", |_| BuiltinFunction::Address)]
  #[token("balance", |_| BuiltinFunction::Balance)]
  #[token("selfbalance", |_| BuiltinFunction::Selfbalance)]
  #[token("caller", |_| BuiltinFunction::Caller)]
  #[token("callvalue", |_| BuiltinFunction::Callvalue)]
  #[token("calldataload", |_| BuiltinFunction::Calldataload)]
  #[token("calldatasize", |_| BuiltinFunction::Calldatasize)]
  #[token("calldatacopy", |_| BuiltinFunction::Calldatacopy)]
  #[token("extcodesize", |_| BuiltinFunction::Extcodesize)]
  #[token("extcodecopy", |_| BuiltinFunction::Extcodecopy)]
  #[token("returndatasize", |_| BuiltinFunction::Returndatasize)]
  #[token("returndatacopy", |_| BuiltinFunction::Returndatacopy)]
  #[token("mcopy", |_| BuiltinFunction::Mcopy)]
  #[token("extcodehash", |_| BuiltinFunction::Extcodehash)]
  #[token("create", |_| BuiltinFunction::Create)]
  #[token("create2", |_| BuiltinFunction::Create2)]
  #[token("call", |_| BuiltinFunction::Call)]
  #[token("callcode", |_| BuiltinFunction::Callcode)]
  #[token("delegatecall", |_| BuiltinFunction::Delegatecall)]
  #[token("staticcall", |_| BuiltinFunction::Staticcall)]
  #[token("return", |_| BuiltinFunction::Return)]
  #[token("revert", |_| BuiltinFunction::Revert)]
  #[token("selfdestruct", |_| BuiltinFunction::Selfdestruct)]
  #[token("invalid", |_| BuiltinFunction::Invalid)]
  #[token("log0", |_| BuiltinFunction::Log0)]
  #[token("log1", |_| BuiltinFunction::Log1)]
  #[token("log2", |_| BuiltinFunction::Log2)]
  #[token("log3", |_| BuiltinFunction::Log3)]
  #[token("log4", |_| BuiltinFunction::Log4)]
  #[token("chainid", |_| BuiltinFunction::Chainid)]
  #[token("origin", |_| BuiltinFunction::Origin)]
  #[token("gasprice", |_| BuiltinFunction::Gasprice)]
  #[token("blockhash", |_| BuiltinFunction::Blockhash)]
  #[token("blobhash", |_| BuiltinFunction::Blobhash)]
  #[token("coinbase", |_| BuiltinFunction::Coinbase)]
  #[token("timestamp", |_| BuiltinFunction::Timestamp)]
  #[token("number", |_| BuiltinFunction::Number)]
  #[token("difficulty", |_| BuiltinFunction::Difficulty)]
  #[token("prevrandao", |_| BuiltinFunction::Prevrandao)]
  #[token("gaslimit", |_| BuiltinFunction::Gaslimit)]
  #[token("basefee", |_| BuiltinFunction::Basefee)]
  #[token("blobbasefee", |_| BuiltinFunction::Blobbasefee)]
  Builin(BuiltinFunction),
}
