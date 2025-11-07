use logosky::{
  Lexable,
  logos::Logos,
  utils::{Span, recursion_tracker::{RecursionLimitExceeded, RecursionLimiter}},
};

use crate::{
  error::sol as error,
  sol::{Denomination, FixedBytes, Int, Lit, Uint, handlers, syntactic},
  types::{LitHexStr, LitRegularStr},
  utils::{
    Wrapper,
    sealed::{
      DoubleQuotedHexStrLexer, DoubleQuotedRegularStrLexer, SingleQuotedHexStrLexer,
      SingleQuotedRegularStrLexer,
    },
  },
};

type StringError = crate::error::StringError<char>;
type HexStringError = crate::error::HexStringError<char>;
type Error = error::Error<char, RecursionLimitExceeded>;
type Errors = error::Errors<char, RecursionLimitExceeded>;
type UnderlyingErrorContainer = <Errors as Wrapper>::Underlying;

/// Token
#[derive(Logos, Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[logos(
  crate = logosky::logos,
  source = str,
  extras = RecursionLimiter,
  error(Errors, |l| Errors::from(handlers::str::default_error(l)))
)]
#[logos(skip r"[ \t\r\n\u{000C}]+|//[^\r\n]*|/\*([^*]|\*+[^*/])*\*+/")]
#[logos(subpattern double_quoted_printable = "[\u{0020}-\u{0021}\u{0023}-\u{005B}\u{005D}-\u{007E}]")]
#[logos(subpattern single_quoted_printable = "[\u{0020}-\u{0026}\u{0028}-\u{005B}\u{005D}-\u{007E}]")]
#[logos(subpattern double_quoted_unicode = r#"[^"\r\n\\]"#)]
#[logos(subpattern single_quoted_unicode = r"[^'\r\n\\]")]
#[logos(subpattern escape_sequence = r#"\\['"\\nrt\n\r]|\\u[0-9a-fA-F]{4}|\\x[0-9a-fA-F]{2}"#)]
#[logos(subpattern double_quoted_char = "(?&double_quoted_printable)|(?&escape_sequence)")]
#[logos(subpattern single_quoted_char = "(?&single_quoted_printable)|(?&escape_sequence)")]
#[logos(subpattern unicode_double_quoted_char = "(?&double_quoted_unicode)|(?&escape_sequence)")]
#[logos(subpattern unicode_single_quoted_char = "(?&single_quoted_unicode)|(?&escape_sequence)")]
#[logos(subpattern double_quoted_chars = r#"(?&double_quoted_char)+"#)]
#[logos(subpattern single_quoted_chars = r#"(?&single_quoted_char)+"#)]
#[logos(subpattern unicode_double_quoted_chars = r#"(?&unicode_double_quoted_char)*"#)]
#[logos(subpattern unicode_single_quoted_chars = r#"(?&unicode_single_quoted_char)*"#)]
#[logos(subpattern hex_digit = "[0-9A-Fa-f]")]
#[logos(subpattern hex_digit_pair = "(?&hex_digit){2}")]
#[logos(subpattern hex_string_content = "(?&hex_digit_pair)(?:_(?&hex_digit_pair))*")]
enum Token<'a> {
  #[token("abstract")]
  Abstract,
  #[token("address")]
  Address,
  #[token("anonymous")]
  Anonymous,
  #[token("as")]
  As,
  #[token("assembly")]
  Assembly,
  #[token("bool")]
  Bool,
  #[token("break")]
  Break,
  #[token("bytes")]
  Bytes,
  #[token("calldata")]
  Calldata,
  #[token("catch")]
  Catch,
  #[token("constant")]
  Constant,
  #[token("constructor")]
  Constructor,
  #[token("continue")]
  Continue,
  #[token("contract")]
  Contract,
  #[token("delete")]
  Delete,
  #[token("do")]
  Do,
  #[token("else")]
  Else,
  #[token("emit")]
  Emit,
  #[token("enum")]
  Enum,
  #[token("event")]
  Event,
  #[token("external")]
  External,
  #[token("fallback")]
  Fallback,
  #[token("for")]
  For,
  #[token("function")]
  Function,
  #[token("if")]
  If,
  #[token("immutable")]
  Immutable,
  #[token("import")]
  Import,
  #[token("indexed")]
  Indexed,
  #[token("interface")]
  Interface,
  #[token("internal")]
  Internal,
  #[token("is")]
  Is,
  #[token("library")]
  Library,
  #[token("mapping")]
  Mapping,
  #[token("memory")]
  Memory,
  #[token("modifier")]
  Modifier,
  #[token("new")]
  New,
  #[token("override")]
  Override,
  #[token("payable")]
  Payable,
  #[token("private")]
  Private,
  #[token("public")]
  Public,
  #[token("pure")]
  Pure,
  #[token("receive")]
  Receive,
  #[token("return")]
  Return,
  #[token("returns")]
  Returns,
  #[token("storage")]
  Storage,
  #[token("string")]
  String,
  #[token("struct")]
  Struct,
  #[token("try")]
  Try,
  #[token("type")]
  Type,
  #[token("unchecked")]
  Unchecked,
  #[token("using")]
  Using,
  #[token("view")]
  View,
  #[token("virtual")]
  Virtual,
  #[token("while")]
  While,

  #[token("(")]
  LParen,
  #[token(")")]
  RParen,
  #[token("[")]
  LBracket,
  #[token("]")]
  RBracket,
  #[token("{")]
  LBrace,
  #[token("}")]
  RBrace,
  #[token(":")]
  Colon,
  #[token(";")]
  Semicolon,
  #[token(".")]
  Dot,
  #[token("?")]
  Question,
  #[token("=>")]
  FatArrow,
  #[token("->")]
  ThinArrow,
  #[token("=")]
  Assign,
  #[token("|=")]
  BitOrAssign,
  #[token("&=")]
  BitAndAssign,
  #[token("^=")]
  BitXorAssign,
  #[token("<<=")]
  ShlAssign,
  #[token(">>=")]
  SarAssign,
  #[token(">>>=")]
  ShrAssign,
  #[token("+=")]
  AddAssign,
  #[token("-=")]
  SubAssign,
  #[token("*=")]
  MulAssign,
  #[token("/=")]
  DivAssign,
  #[token("%=")]
  ModAssign,
  #[token(",")]
  Comma,
  #[token("||")]
  Or,
  #[token("&&")]
  And,
  #[token("|")]
  BitOr,
  #[token("&")]
  BitAnd,
  #[token("^")]
  BitXor,
  #[token("<<")]
  Shl,
  #[token(">>")]
  Sar,
  #[token(">>>")]
  Shr,
  #[token("+")]
  Add,
  #[token("-")]
  Sub,
  #[token("*")]
  Mul,
  #[token("/")]
  Div,
  #[token("%")]
  Mod,
  #[token("**")]
  Exp,
  #[token("==")]
  Eq,
  #[token("!=")]
  Ne,
  #[token("<")]
  Lt,
  #[token("<=")]
  Le,
  #[token(">")]
  Gt,
  #[token(">=")]
  Ge,
  #[token("!")]
  Not,
  #[token("~")]
  BitNot,
  #[token("++")]
  Inc,
  #[token("--")]
  Dec,

  #[token("bytes1", |_| FixedBytes::BYTES1)]
  #[token("bytes2", |_| FixedBytes::BYTES2)]
  #[token("bytes3", |_| FixedBytes::BYTES3)]
  #[token("bytes4", |_| FixedBytes::BYTES4)]
  #[token("bytes5", |_| FixedBytes::BYTES5)]
  #[token("bytes6", |_| FixedBytes::BYTES6)]
  #[token("bytes7", |_| FixedBytes::BYTES7)]
  #[token("bytes8", |_| FixedBytes::BYTES8)]
  #[token("bytes9", |_| FixedBytes::BYTES9)]
  #[token("bytes10", |_| FixedBytes::BYTES10)]
  #[token("bytes11", |_| FixedBytes::BYTES11)]
  #[token("bytes12", |_| FixedBytes::BYTES12)]
  #[token("bytes13", |_| FixedBytes::BYTES13)]
  #[token("bytes14", |_| FixedBytes::BYTES14)]
  #[token("bytes15", |_| FixedBytes::BYTES15)]
  #[token("bytes16", |_| FixedBytes::BYTES16)]
  #[token("bytes17", |_| FixedBytes::BYTES17)]
  #[token("bytes18", |_| FixedBytes::BYTES18)]
  #[token("bytes19", |_| FixedBytes::BYTES19)]
  #[token("bytes20", |_| FixedBytes::BYTES20)]
  #[token("bytes21", |_| FixedBytes::BYTES21)]
  #[token("bytes22", |_| FixedBytes::BYTES22)]
  #[token("bytes23", |_| FixedBytes::BYTES23)]
  #[token("bytes24", |_| FixedBytes::BYTES24)]
  #[token("bytes25", |_| FixedBytes::BYTES25)]
  #[token("bytes26", |_| FixedBytes::BYTES26)]
  #[token("bytes27", |_| FixedBytes::BYTES27)]
  #[token("bytes28", |_| FixedBytes::BYTES28)]
  #[token("bytes29", |_| FixedBytes::BYTES29)]
  #[token("bytes30", |_| FixedBytes::BYTES30)]
  #[token("bytes31", |_| FixedBytes::BYTES31)]
  #[token("bytes32", |_| FixedBytes::BYTES32)]
  FixedBytes(FixedBytes),

  #[token("wei", |_| Denomination::Wei)]
  #[token("gwei", |_| Denomination::Gwei)]
  #[token("ether", |_| Denomination::Ether)]
  #[token("seconds", |_| Denomination::Seconds)]
  #[token("minutes", |_| Denomination::Minutes)]
  #[token("hours", |_| Denomination::Hours)]
  #[token("days", |_| Denomination::Days)]
  #[token("weeks", |_| Denomination::Weeks)]
  #[token("years", |_| Denomination::Years)]
  Denomination(Denomination),

  #[token("int8", |_| Int::I8)]
  #[token("int16", |_| Int::I16)]
  #[token("int24", |_| Int::I24)]
  #[token("int32", |_| Int::I32)]
  #[token("int40", |_| Int::I40)]
  #[token("int48", |_| Int::I48)]
  #[token("int56", |_| Int::I56)]
  #[token("int64", |_| Int::I64)]
  #[token("int72", |_| Int::I72)]
  #[token("int80", |_| Int::I80)]
  #[token("int88", |_| Int::I88)]
  #[token("int96", |_| Int::I96)]
  #[token("int104", |_| Int::I104)]
  #[token("int112", |_| Int::I112)]
  #[token("int120", |_| Int::I120)]
  #[token("int128", |_| Int::I128)]
  #[token("int136", |_| Int::I136)]
  #[token("int144", |_| Int::I144)]
  #[token("int152", |_| Int::I152)]
  #[token("int160", |_| Int::I160)]
  #[token("int168", |_| Int::I168)]
  #[token("int176", |_| Int::I176)]
  #[token("int184", |_| Int::I184)]
  #[token("int192", |_| Int::I192)]
  #[token("int200", |_| Int::I200)]
  #[token("int208", |_| Int::I208)]
  #[token("int216", |_| Int::I216)]
  #[token("int224", |_| Int::I224)]
  #[token("int232", |_| Int::I232)]
  #[token("int240", |_| Int::I240)]
  #[token("int248", |_| Int::I248)]
  #[token("int256", |_| Int::I256)]
  #[token("int", |_| Int::I256)]
  Int(Int),

  #[token("uint8", |_| Uint::U8)]
  #[token("uint16", |_| Uint::U16)]
  #[token("uint24", |_| Uint::U24)]
  #[token("uint32", |_| Uint::U32)]
  #[token("uint40", |_| Uint::U40)]
  #[token("uint48", |_| Uint::U48)]
  #[token("uint56", |_| Uint::U56)]
  #[token("uint64", |_| Uint::U64)]
  #[token("uint72", |_| Uint::U72)]
  #[token("uint80", |_| Uint::U80)]
  #[token("uint88", |_| Uint::U88)]
  #[token("uint96", |_| Uint::U96)]
  #[token("uint104", |_| Uint::U104)]
  #[token("uint112", |_| Uint::U112)]
  #[token("uint120", |_| Uint::U120)]
  #[token("uint128", |_| Uint::U128)]
  #[token("uint136", |_| Uint::U136)]
  #[token("uint144", |_| Uint::U144)]
  #[token("uint152", |_| Uint::U152)]
  #[token("uint160", |_| Uint::U160)]
  #[token("uint168", |_| Uint::U168)]
  #[token("uint176", |_| Uint::U176)]
  #[token("uint184", |_| Uint::U184)]
  #[token("uint192", |_| Uint::U192)]
  #[token("uint200", |_| Uint::U200)]
  #[token("uint208", |_| Uint::U208)]
  #[token("uint216", |_| Uint::U216)]
  #[token("uint224", |_| Uint::U224)]
  #[token("uint232", |_| Uint::U232)]
  #[token("uint240", |_| Uint::U240)]
  #[token("uint248", |_| Uint::U248)]
  #[token("uint256", |_| Uint::U256)]
  #[token("uint", |_| Uint::U256)]
  Uint(Uint),

  #[token("fixed")]
  #[regex("fixed[1-9][0-9]*x[1-9][0-9]*")]
  Fixed(&'a str),

  #[token("ufixed")]
  #[regex("ufixed[1-9][0-9]*x[1-9][0-9]*")]
  UFixed(&'a str),

  // ------- Boolean literals -------
  #[token("true", |lexer| Lit::lit_true(lexer.slice()))]
  #[token("false", |lexer| Lit::lit_false(lexer.slice()))]

  // ------- Empty quoted string literals -------
  #[token("\"\"", |lexer| Lit::lit_empty_double_quoted_string(lexer.slice()))]
  #[token("''", |lexer| Lit::lit_empty_single_quoted_string(lexer.slice()))]

  // ------- Regular string literals -------
  // Double quoted non-empty string literal lexing
  #[regex(r#""(?&double_quoted_chars)""#, |lexer| Lit::lit_double_quoted_regular_string(lexer.slice()))]
  // Error handling branches for double quoted non-empty string literal lexing
  #[regex(r#""(?&double_quoted_chars)"#, |lexer| unclosed_double_quoted_regular_string_error(lexer.span().into()))]
  #[token("\"", |lexer| {
      <LitRegularStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedRegularStrLexer::<logosky::logos::Lexer<'_, _>, char, StringError, Error>::from_mut(lexer))
        .map(Into::into)
        .map_err(Errors::from_underlying)
  })]
  // Single quoted non-empty string literal lexing
  #[regex(r"'(?&single_quoted_chars)'", |lexer| Lit::lit_single_quoted_regular_string(lexer.slice()))]
  // Error handling branches for single quoted non-empty string literal lexing
  #[regex(r"'(?&single_quoted_chars)", |lexer| unclosed_single_quoted_regular_string_error(lexer.span().into()))]
  #[token("\'", |lexer| {
      <LitRegularStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedRegularStrLexer::<logosky::logos::Lexer<'_, _>, char, StringError, Error>::from_mut(lexer))
        .map(Into::into)
        .map_err(Errors::from_underlying)
  })]

  // ------- Hex string literals -------
  // Double quoted hex string literal lexing
  #[regex("hex\"(?&hex_string_content)\"", |lexer| Lit::lit_double_quoted_hex_string(lexer.slice()))]
  // Error handling branches for double quoted hex string literal lexing
  #[regex("hex\"(?&hex_string_content)", |lexer| unclosed_double_quoted_hex_string_error(lexer.span().into()))]
  #[token("hex\"", |lexer| {
    <LitHexStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedHexStrLexer::<logosky::logos::Lexer<'_, _>, char, HexStringError, Error>::from_mut(lexer))
      .map(Into::into)
      .map_err(Errors::from_underlying)
  })]
  // Single quoted hex string literal lexing
  #[regex("hex'(?&hex_string_content)'", |lexer| Lit::lit_single_quoted_hex_string(lexer.slice()))]
  // Error handling branches for single quoted hex string literal lexing
  #[regex("hex'(?&hex_string_content)", |lexer| unclosed_single_quoted_hex_string_error(lexer.span().into()))]
  #[token("hex'", |lexer| {
    <LitHexStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedHexStrLexer::<logosky::logos::Lexer<'_, _>, char, HexStringError, Error>::from_mut(lexer))
      .map(Into::into)
      .map_err(Errors::from_underlying)
  })]


  // #[regex("hex\"(?:[0-9A-Fa-f]{2}(?:_[0-9A-Fa-f]{2})*)\"", |lexer| LitStr::DoubleQuotedHex(lexer.slice()))]
  // #[regex("hex'(?:[0-9A-Fa-f]{2}(?:_[0-9A-Fa-f]{2})*)'", |lexer| LitStr::SingleQuotedHex(lexer.slice()))]
  // #[regex("\"(?&double_quoted_chars)\"", |lexer| LitStr::DoubleQuoted(lexer.slice()))]
  // #[regex("'(?&single_quoted_chars)'", |lexer| LitStr::SingleQuoted(lexer.slice()))]
  // #[regex("unicode\"(?&unicode_double_quoted_chars)\"", |lexer| Lit::lit_double_quoted_unicode_string(lexer.slice()))]
  // #[regex("unicode'(?&unicode_single_quoted_chars)'", |lexer| Lit::lit_single_quoted_unicode_string(lexer.slice()))]
  Lit(Lit<&'a str>),

  #[regex("0x[0-9a-fA-F_]+")]
  HexNumber(&'a str),

  #[regex("(?:[0-9](?:_?[0-9])*)(?:\\.(?:[0-9](?:_?[0-9])*))?(?:[eE][-]?(?:[0-9](?:_?[0-9])*))?")]
  DecimalNumber(&'a str),

  #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*")]
  Identifier(&'a str),
}

#[cfg_attr(not(tarpaulin), inline(always))]
fn unclosed_double_quoted_regular_string_error<S>(span: Span) -> Result<Lit<S>, Errors> {
  Err(Errors::from(Error::String(
    crate::error::StringError::unclosed_double_quote(span),
  )))
}

#[cfg_attr(not(tarpaulin), inline(always))]
fn unclosed_single_quoted_regular_string_error<S>(span: Span) -> Result<Lit<S>, Errors> {
  Err(Errors::from(Error::String(
    crate::error::StringError::unclosed_single_quote(span),
  )))
}

#[cfg_attr(not(tarpaulin), inline(always))]
fn unclosed_double_quoted_hex_string_error<S>(span: Span) -> Result<Lit<S>, Errors> {
  Err(Errors::from(Error::HexString(
    crate::error::HexStringError::unclosed_double_quote(span),
  )))
}

#[cfg_attr(not(tarpaulin), inline(always))]
fn unclosed_single_quoted_hex_string_error<S>(span: Span) -> Result<Lit<S>, Errors> {
  Err(Errors::from(Error::HexString(
    crate::error::HexStringError::unclosed_single_quote(span),
  )))
}
