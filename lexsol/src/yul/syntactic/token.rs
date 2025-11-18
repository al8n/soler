macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty $(,)?)) => {
    #[allow(single_use_lifetimes)]
    mod $mod {
      use logosky::{
        Lexable, Logos,
        utils::{Span, recursion_tracker::{RecursionLimitExceeded, RecursionLimiter, RecursionTracker}},
      };

      use crate::{
        utils::{Wrapper, sealed::{
          DoubleQuotedHexStrLexer, DoubleQuotedRegularStrLexer, SingleQuotedHexStrLexer,
          SingleQuotedRegularStrLexer,
        }},
        yul::{handlers, Lit, syntactic},
        types::{LitHexStr, LitRegularStr},
        error::yul as error,
      };

      #[cfg(feature = "evm")]
      use crate::yul::EvmBuiltinFunction;

      type StringError = crate::error::StringError<$char>;
      type HexStringError = crate::error::HexStringError<$char>;
      type Error = error::Error<$char, RecursionLimitExceeded>;
      type Errors = error::Errors<$char, RecursionLimitExceeded>;
      type UnderlyingErrorContainer = <Errors as Wrapper>::Underlying;

      #[allow(warnings)]
      impl<'b $(: $lt)?, $($lt: 'b)?> logosky::Token<'b> for syntactic::Token<$slice> {
        type Kind = syntactic::TokenKind;
        type Char = $char;
        type Logos = Token $(<$lt>)?;

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }
      }

      #[doc(hidden)]
      #[derive(Logos, Clone, Debug)]
      #[logos(
        crate = logosky::logos,
        source = $source,
        extras = RecursionLimiter,
        error(Errors, |l| Errors::from(handlers::$handlers::default_error(l)))
      )]
      #[logos(skip r"[ \t\r\n\u{000C}]+|//[^\r\n]*|/\*([^*]|\*+[^*/])*\*+/")]
      #[logos(subpattern escape_sequence = r#"\\['"\\nrt\n\r]|\\u[0-9a-fA-F]{4}|\\x[0-9a-fA-F]{2}"#)]
      #[logos(subpattern double_quoted_printable = "[\u{0020}-\u{0021}\u{0023}-\u{005B}\u{005D}-\u{007E}]")]
      #[logos(subpattern single_quoted_printable = "[\u{0020}-\u{0026}\u{0028}-\u{005B}\u{005D}-\u{007E}]")]
      #[logos(subpattern double_quoted_char = "(?&double_quoted_printable)|(?&escape_sequence)")]
      #[logos(subpattern single_quoted_char = "(?&single_quoted_printable)|(?&escape_sequence)")]
      #[logos(subpattern double_quoted_chars = r#"(?&double_quoted_char)+"#)]
      #[logos(subpattern single_quoted_chars = r#"(?&single_quoted_char)+"#)]
      #[logos(subpattern hex_digit = "[0-9A-Fa-f]")]
      #[logos(subpattern hex_digit_pair = "(?&hex_digit){2}")]
      #[logos(subpattern hex_string_content = "(?&hex_digit_pair)(?:_(?&hex_digit_pair))*")]
      #[logos(subpattern digit = "[0-9]")]
      #[logos(subpattern decimal = "0|[1-9][0-9]*")]
      #[logos(subpattern hexadecimal = "0x(?&hex_digit)+")]
      pub enum Token $(<$lt>)? {
        #[token(":=")]
        ColonAssign,
        #[token("->")]
        ThinArrow,
        #[token("{", |lexer| lexer.increase_and_check().map_err(|e| Errors::from(Error::State(e))))]
        LBrace,
        #[token("}", |lexer| lexer.decrease())]
        RBrace,
        #[token("(", |lexer| lexer.increase_and_check().map_err(|e| Errors::from(Error::State(e))))]
        LParen,
        #[token(")", |lexer| lexer.decrease())]
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
        Identifier($slice),

        #[token("true", |lexer| Lit::lit_true(lexer.slice()))]
        #[token("false", |lexer| Lit::lit_false(lexer.slice()))]
        #[regex("(?&decimal)", handlers::$handlers::handle_decimal_suffix)]
        #[regex("[1-9][0-9_]+", handlers::$handlers::handle_malformed_decimal_suffix)]
        #[regex("0(?&digit)+", handlers::$handlers::handle_leading_zero_and_suffix)]
        #[regex("(?&hexadecimal)", handlers::$handlers::handle_hexadecimal_suffix, priority = 7)]
        #[regex("0x[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[regex("0X[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[regex("0[xX]{2,}[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[token("0x", handlers::$handlers::handle_hexadecimal_prefix_with_invalid_following)]

        // Double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)\"", |lexer| Lit::lit_double_quoted_hex_string(lexer.slice()))]
        // Error handling branches for double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)", |lexer| unclosed_double_quoted_hex_string_error(lexer.span().into()))]
        #[token("hex\"", |lexer| {
          <LitHexStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedHexStrLexer::<logosky::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
        })]

        // Single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)'", |lexer| Lit::lit_single_quoted_hex_string(lexer.slice()))]
        // Error handling branches for single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)", |lexer| unclosed_single_quoted_hex_string_error(lexer.span().into()))]
        #[token("hex'", |lexer| {
          <LitHexStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedHexStrLexer::<logosky::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
        })]

        // Double quoted non-empty string literal lexing
        #[regex(r#""(?&double_quoted_chars)""#, |lexer| Lit::lit_double_quoted_regular_string(lexer.slice()))]
        // Error handling branches for double quoted non-empty string literal lexing
        #[token(r#""""#, |lexer| empty_double_quoted_string_error(lexer.span().into()))]
        #[regex(r#""(?&double_quoted_chars)"#, |lexer| unclosed_double_quoted_regular_string_error(lexer.span().into()))]
        #[token("\"", |lexer| {
          <LitRegularStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedRegularStrLexer::<logosky::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
        })]
        // Single quoted non-empty string literal lexing
        #[regex(r"'(?&single_quoted_chars)'", |lexer| Lit::lit_single_quoted_regular_string(lexer.slice()))]
        // Error handling branches for single quoted non-empty string literal lexing
        #[token("''", |lexer| empty_single_quoted_string_error(lexer.span().into()))]
        #[regex(r"'(?&single_quoted_chars)", |lexer| unclosed_single_quoted_regular_string_error(lexer.span().into()))]
        #[token("\'", |lexer| {
          <LitRegularStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedRegularStrLexer::<logosky::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
        })]
        Lit(Lit<$slice>),

        #[cfg(feature = "evm")]
        #[token("stop", |_| EvmBuiltinFunction::STOP)]
        #[token("add", |_| EvmBuiltinFunction::ADD)]
        #[token("sub", |_| EvmBuiltinFunction::SUB)]
        #[token("mul", |_| EvmBuiltinFunction::MUL)]
        #[token("div", |_| EvmBuiltinFunction::DIV)]
        #[token("sdiv", |_| EvmBuiltinFunction::SDIV)]
        #[token("mod", |_| EvmBuiltinFunction::MOD)]
        #[token("smod", |_| EvmBuiltinFunction::SMOD)]
        #[token("exp", |_| EvmBuiltinFunction::EXP)]
        #[token("not", |_| EvmBuiltinFunction::NOT)]
        #[token("lt", |_| EvmBuiltinFunction::LT)]
        #[token("gt", |_| EvmBuiltinFunction::GT)]
        #[token("slt", |_| EvmBuiltinFunction::SLT)]
        #[token("sgt", |_| EvmBuiltinFunction::SGT)]
        #[token("eq", |_| EvmBuiltinFunction::EQ)]
        #[token("iszero", |_| EvmBuiltinFunction::ISZERO)]
        #[token("and", |_| EvmBuiltinFunction::AND)]
        #[token("or", |_| EvmBuiltinFunction::OR)]
        #[token("xor", |_| EvmBuiltinFunction::XOR)]
        #[token("byte", |_| EvmBuiltinFunction::BYTE)]
        #[token("shl", |_| EvmBuiltinFunction::SHL)]
        #[token("shr", |_| EvmBuiltinFunction::SHR)]
        #[token("sar", |_| EvmBuiltinFunction::SAR)]
        #[token("clz", |_| EvmBuiltinFunction::CLZ)]
        #[token("addmod", |_| EvmBuiltinFunction::ADDMOD)]
        #[token("mulmod", |_| EvmBuiltinFunction::MULMOD)]
        #[token("signextend", |_| EvmBuiltinFunction::SIGNEXTEND)]
        #[token("keccak256", |_| EvmBuiltinFunction::KECCAK256)]
        #[token("pop", |_| EvmBuiltinFunction::POP)]
        #[token("mload", |_| EvmBuiltinFunction::MLOAD)]
        #[token("mstore", |_| EvmBuiltinFunction::MSTORE)]
        #[token("mstore8", |_| EvmBuiltinFunction::MSTORE8)]
        #[token("sload", |_| EvmBuiltinFunction::SLOAD)]
        #[token("sstore", |_| EvmBuiltinFunction::SSTORE)]
        #[token("tload", |_| EvmBuiltinFunction::TLOAD)]
        #[token("tstore", |_| EvmBuiltinFunction::TSTORE)]
        #[token("msize", |_| EvmBuiltinFunction::MSIZE)]
        #[token("gas", |_| EvmBuiltinFunction::GAS)]
        #[token("address", |_| EvmBuiltinFunction::ADDRESS)]
        #[token("balance", |_| EvmBuiltinFunction::BALANCE)]
        #[token("selfbalance", |_| EvmBuiltinFunction::SELFBALANCE)]
        #[token("caller", |_| EvmBuiltinFunction::CALLER)]
        #[token("callvalue", |_| EvmBuiltinFunction::CALLVALUE)]
        #[token("calldataload", |_| EvmBuiltinFunction::CALLDATALOAD)]
        #[token("calldatasize", |_| EvmBuiltinFunction::CALLDATASIZE)]
        #[token("calldatacopy", |_| EvmBuiltinFunction::CALLDATACOPY)]
        #[token("extcodesize", |_| EvmBuiltinFunction::EXTCODESIZE)]
        #[token("extcodecopy", |_| EvmBuiltinFunction::EXTCODECOPY)]
        #[token("returndatasize", |_| EvmBuiltinFunction::RETURNDATASIZE)]
        #[token("returndatacopy", |_| EvmBuiltinFunction::RETURNDATACOPY)]
        #[token("mcopy", |_| EvmBuiltinFunction::MCOPY)]
        #[token("extcodehash", |_| EvmBuiltinFunction::EXTCODEHASH)]
        #[token("create", |_| EvmBuiltinFunction::CREATE)]
        #[token("create2", |_| EvmBuiltinFunction::CREATE2)]
        #[token("call", |_| EvmBuiltinFunction::CALL)]
        #[token("callcode", |_| EvmBuiltinFunction::CALLCODE)]
        #[token("delegatecall", |_| EvmBuiltinFunction::DELEGATECALL)]
        #[token("staticcall", |_| EvmBuiltinFunction::STATICCALL)]
        #[token("return", |_| EvmBuiltinFunction::RETURN)]
        #[token("revert", |_| EvmBuiltinFunction::REVERT)]
        #[token("selfdestruct", |_| EvmBuiltinFunction::SELFDESTRUCT)]
        #[token("invalid", |_| EvmBuiltinFunction::INVALID)]
        #[token("log0", |_| EvmBuiltinFunction::LOG0)]
        #[token("log1", |_| EvmBuiltinFunction::LOG1)]
        #[token("log2", |_| EvmBuiltinFunction::LOG2)]
        #[token("log3", |_| EvmBuiltinFunction::LOG3)]
        #[token("log4", |_| EvmBuiltinFunction::LOG4)]
        #[token("chainid", |_| EvmBuiltinFunction::CHAINID)]
        #[token("origin", |_| EvmBuiltinFunction::ORIGIN)]
        #[token("gasprice", |_| EvmBuiltinFunction::GASPRICE)]
        #[token("blockhash", |_| EvmBuiltinFunction::BLOCKHASH)]
        #[token("blobhash", |_| EvmBuiltinFunction::BLOBHASH)]
        #[token("coinbase", |_| EvmBuiltinFunction::COINBASE)]
        #[token("timestamp", |_| EvmBuiltinFunction::TIMESTAMP)]
        #[token("number", |_| EvmBuiltinFunction::NUMBER)]
        #[token("difficulty", |_| EvmBuiltinFunction::DIFFICULTY)]
        #[token("prevrandao", |_| EvmBuiltinFunction::PREVRANDAO)]
        #[token("gaslimit", |_| EvmBuiltinFunction::GASLIMIT)]
        #[token("basefee", |_| EvmBuiltinFunction::BASEFEE)]
        #[token("blobbasefee", |_| EvmBuiltinFunction::BLOBBASEFEE)]
        EvmBuiltin(EvmBuiltinFunction),
      }

      impl$(<$lt>)? From<Token $(<$lt>)?> for syntactic::Token<$slice> {
        #[cfg_attr(not(tarpaulin), inline(always))]
        fn from(value: Token $(<$lt>)?) -> Self {
          match value {
            Token::ColonAssign => Self::ColonAssign,
            Token::ThinArrow => Self::ThinArrow,
            Token::LBrace => Self::LBrace,
            Token::RBrace => Self::RBrace,
            Token::LParen => Self::LParen,
            Token::RParen => Self::RParen,
            Token::Dot => Self::Dot,
            Token::Comma => Self::Comma,
            Token::Leave => Self::Leave,
            Token::Continue => Self::Continue,
            Token::Break => Self::Break,
            Token::Switch => Self::Switch,
            Token::Case => Self::Case,
            Token::Default => Self::Default,
            Token::Function => Self::Function,
            Token::Let => Self::Let,
            Token::If => Self::If,
            Token::For => Self::For,
            Token::Identifier(slice) => Self::Identifier(slice),
            Token::Lit(lit) => Self::Lit(lit),
            #[cfg(feature = "evm")]
            Token::EvmBuiltin(func) => Self::EvmBuiltin(func),
          }
        }
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn empty_single_quoted_string_error<S>(span: Span) -> Result<Lit<S>, Errors> {
        Err(Error::empty_single_quote(span).into())
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn empty_double_quoted_string_error<S>(span: Span) -> Result<Lit<S>, Errors> {
        Err(Error::empty_double_quote(span).into())
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

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn malformed_hex_literal_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut logosky::logos::Lexer<'b, Token $(<$lt>)? >) -> Result<Lit<$slice>, Errors> {
        Err(Error::from(crate::error::yul::HexadecimalError::malformed(lexer.span().into())).into())
      }
    }
  }
}

pub(super) use token;
