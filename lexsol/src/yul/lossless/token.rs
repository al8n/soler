macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty $(,)?)) => {
    #[allow(single_use_lifetimes)]
    mod $mod {
      use tokit::{
        lexer::Lexable, logos::{Lexer, Logos},
        utils::tracker::{LimitExceeded, Limiter, Tracker},
        error::ErrorContainer,
      };

      use crate::{
        utils::{Wrapper, sealed::{
          DoubleQuotedHexStrLexer, DoubleQuotedRegularStrLexer, SingleQuotedHexStrLexer,
          SingleQuotedRegularStrLexer,
        }},
        yul::{handlers, Lit, lossless},
        types::{LitHexStr, LitRegularStr},
        error::yul as error,
      };

      #[cfg(feature = "evm")]
      use crate::yul::EvmBuiltinFunction;

      type StringError = crate::error::StringError<$char>;
      type HexStringError = crate::error::HexStringError<$char>;
      type Error = error::Error<$char, LimitExceeded>;
      type Errors = error::Errors<$char, LimitExceeded>;
      type UnderlyingErrorContainer = <Errors as Wrapper>::Underlying;

      #[allow(warnings)]
      impl<'b $(: $lt)?, $($lt: 'b)?> tokit::Token<'b> for lossless::Token<$slice> {
        type Kind = lossless::TokenKind;
        type Error = Errors;

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn is_trivia(&self) -> bool {
          match self {
            lossless::Token::Space
            | lossless::Token::Tab
            | lossless::Token::NewLine
            | lossless::Token::CarriageReturn
            | lossless::Token::CarriageReturnNewLine
            | lossless::Token::FormFeed
            | lossless::Token::LineComment(_)
            | lossless::Token::MultiLineComment(_) => true,
            _ => false,
          }
        }
      }

      #[allow(warnings)]
      impl<'b $(: $lt)?, $($lt: 'b)?> crate::TokenBridge<'b> for lossless::Token<$slice> {
        type Logos = Token;

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn kind(value: &Self::Logos) -> lossless::TokenKind {
          match value {
            Token::Space => lossless::TokenKind::Space,
            Token::Tab => lossless::TokenKind::Tab,
            Token::NewLine => lossless::TokenKind::NewLine,
            Token::CarriageReturn => lossless::TokenKind::CarriageReturn,
            Token::CarriageReturnNewLine => lossless::TokenKind::CarriageReturnNewLine,
            Token::FormFeed => lossless::TokenKind::FormFeed,
            Token::ColonAssign => lossless::TokenKind::ColonAssign,
            Token::ThinArrow => lossless::TokenKind::ThinArrow,
            Token::LBrace => lossless::TokenKind::LBrace,
            Token::RBrace => lossless::TokenKind::RBrace,
            Token::LParen => lossless::TokenKind::LParen,
            Token::RParen => lossless::TokenKind::RParen,
            Token::Dot => lossless::TokenKind::Dot,
            Token::Comma => lossless::TokenKind::Comma,
            Token::Leave => lossless::TokenKind::Leave,
            Token::Continue => lossless::TokenKind::Continue,
            Token::Break => lossless::TokenKind::Break,
            Token::Switch => lossless::TokenKind::Switch,
            Token::Case => lossless::TokenKind::Case,
            Token::Default => lossless::TokenKind::Default,
            Token::Function => lossless::TokenKind::Function,
            Token::Let => lossless::TokenKind::Let,
            Token::If => lossless::TokenKind::If,
            Token::For => lossless::TokenKind::For,
            Token::LineComment => lossless::TokenKind::LineComment,
            Token::MultiLineComment => lossless::TokenKind::MultiLineComment,
            Token::Identifier => lossless::TokenKind::Identifier,
            Token::Lit(lit) => lossless::TokenKind::Lit(*lit),
            #[cfg(feature = "evm")]
            Token::EvmBuiltin(func) => lossless::TokenKind::EvmBuiltin(*func),
          }
        }
      }

      #[doc(hidden)]
      #[derive(Logos, Clone, Debug)]
      #[logos(
        crate = tokit::logos,
        source = $source,
        extras = Limiter,
        error(Errors, |l| {
          let mut errs = Errors::from(handlers::$handlers::default_error(l));
          match l.increase_token_and_check() {
            Ok(_) => errs,
            Err(e) => {
              errs.push(Error::State(e));
              errs
            },
          }
        })
      )]
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
      pub enum Token {
        #[token(" ", |l| l.increase_token())]
        Space,
        #[token("\t", |l| l.increase_token())]
        Tab,
        #[token("\n", |l| l.increase_token())]
        NewLine,
        #[token("\r", |l| l.increase_token())]
        CarriageReturn,
        #[token("\r\n", |l| l.increase_token())]
        CarriageReturnNewLine,
        #[token("\u{000C}", |l| l.increase_token())]
        FormFeed,

        #[token(":=", |l| l.increase_token())]
        ColonAssign,
        #[token("->", |l| l.increase_token())]
        ThinArrow,
        #[token("{", |lexer| lexer.increase_both())]
        LBrace,
        #[token("}", |lexer| lexer.increase_token_and_decrease_recursion())]
        RBrace,
        #[token("(", |lexer| lexer.increase_both())]
        LParen,
        #[token(")", |lexer| lexer.increase_token_and_decrease_recursion())]
        RParen,
        #[token(".", |l| l.increase_token())]
        Dot,
        #[token(",", |l| l.increase_token())]
        Comma,

        #[token("leave", |l| l.increase_token())]
        Leave,
        #[token("continue", |l| l.increase_token())]
        Continue,
        #[token("break", |l| l.increase_token())]
        Break,
        #[token("switch", |l| l.increase_token())]
        Switch,
        #[token("case", |l| l.increase_token())]
        Case,
        #[token("default", |l| l.increase_token())]
        Default,
        #[token("function", |l| l.increase_token())]
        Function,
        #[token("let", |l| l.increase_token())]
        Let,
        #[token("if", |l| l.increase_token())]
        If,
        #[token("for", |l| l.increase_token())]
        For,

        #[regex(r"//[^\r\n]*", |lexer| increase_token_then_with(lexer, |_| ()))]
        LineComment,

        #[regex(r"/\*([^*]|\*+[^*/])*\*+/", |lexer| increase_token_then_with(lexer, |_| ()))]
        MultiLineComment,

        #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*", |lexer| increase_token_then_with(lexer, |_| ()))]
        Identifier,

        #[token("true", |lexer| increase_token_then_with(lexer, |_| Lit::lit_true(())))]
        #[token("false", |lexer| increase_token_then_with(lexer, |_| Lit::lit_false(())))]
        #[regex("(?&decimal)", |lexer| {
          match handlers::$handlers::handle_decimal_suffix(lexer) {
            Ok(lit) => {
              lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))?;
              Ok(lit)
            },
            Err(e) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(Errors::from(e)),
                Err(state_err) => {
                  let mut errs = Errors::from(e);
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            },
          }
        })]
        #[regex("[1-9][0-9_]+", |lexer| {
          match handlers::$handlers::handle_malformed_decimal_suffix(lexer) {
            Ok(lit) => {
              lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))?;
              Ok(lit)
            },
            Err(e) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(Errors::from(e)),
                Err(state_err) => {
                  let mut errs = Errors::from(e);
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            },
          }
        })]
        #[regex("0(?&digit)+", |lexer| {
          match handlers::$handlers::handle_leading_zero_and_suffix(lexer) {
            Ok(_) => {
              unreachable!("regex guarantees no valid literal can be formed with leading zeros")
            },
            Err(e) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(Errors::from(e)),
                Err(state_err) => {
                  let mut errs = Errors::from(e);
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            },
          }
        })]

        #[regex("(?&hexadecimal)", |lexer| {
          match handlers::$handlers::handle_hexadecimal_suffix(lexer) {
            Ok(lit) => {
              lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))?;
              Ok(lit)
            },
            Err(e) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(Errors::from(e)),
                Err(state_err) => {
                  let mut errs = Errors::from(e);
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            },
          }
        }, priority = 7)]
        #[token("0x", |lexer| {
          match handlers::$handlers::handle_hexadecimal_prefix_with_invalid_following(lexer) {
            Ok(_) => {
              unreachable!("regex guarantees no valid literal can be formed with incomplete hexadecimal literal")
            },
            Err(e) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(Errors::from(e)),
                Err(state_err) => {
                  let mut errs = Errors::from(e);
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            },
          }
        })]
        #[regex("0x[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[regex("0X[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[regex("0[xX]{2,}[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]

        // Double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)\"", |lexer| {
          increase_token_then_with(lexer, |_| Lit::lit_double_quoted_hex_string(()))
        })]
        // Error handling branches for double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)", unclosed_double_quoted_hex_string_error)]
        #[token("hex\"", |lexer| {
          match <LitHexStr as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedHexStrLexer::<tokit::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
          {
            Ok(lit) => {
              lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))?;
              Ok(lit)
            },
            Err(mut errs) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(errs),
                Err(state_err) => {
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            },
          }
        })]

        // Single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)'", |lexer| {
          increase_token_then_with(lexer, |_| Lit::lit_single_quoted_hex_string(()))
        })]
        // Error handling branches for single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)", unclosed_single_quoted_hex_string_error)]
        #[token("hex'", |lexer| {
          match <LitHexStr as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedHexStrLexer::<tokit::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
          {
            Ok(lit) => {
              lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))?;
              Ok(lit)
            },
            Err(mut errs) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(errs),
                Err(state_err) => {
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            },
          }
        })]

        // Double quoted non-empty string literal lexing
        #[regex(r#""(?&double_quoted_chars)""#, |lexer| {
          increase_token_then_with(lexer, |_| Lit::lit_double_quoted_regular_string(()))
        })]
        // Error handling branches for double quoted non-empty string literal lexing
        #[token(r#""""#, empty_double_quoted_string_error)]
        #[regex(r#""(?&double_quoted_chars)"#, unclosed_double_quoted_regular_string_error)]
        #[token("\"", |lexer| {
          match <LitRegularStr as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedRegularStrLexer::<tokit::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
          {
            Ok(lit) => {
              lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))?;
              Ok(lit)
            },
            Err(mut errs) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(errs),
                Err(state_err) => {
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            }
          }
        })]
        // Single quoted non-empty string literal lexing
        #[regex(r"'(?&single_quoted_chars)'", |lexer| {
          increase_token_then_with(lexer, |_| Lit::lit_single_quoted_regular_string(()))
        })]
        // Error handling branches for single quoted non-empty string literal lexing
        #[token("''", empty_single_quoted_string_error)]
        #[regex(r"'(?&single_quoted_chars)", unclosed_single_quoted_regular_string_error)]
        #[token("\'", |lexer| {
          match <LitRegularStr as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedRegularStrLexer::<tokit::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
          {
            Ok(lit) => {
              lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))?;
              Ok(lit)
            },
            Err(mut errs) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(errs),
                Err(state_err) => {
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            }
          }
        })]
        Lit(Lit),

        #[cfg(feature = "evm")]
        #[token("stop", |lexer| increase_token_then(lexer, EvmBuiltinFunction::STOP))]
        #[token("add", |lexer| increase_token_then(lexer, EvmBuiltinFunction::ADD))]
        #[token("sub", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SUB))]
        #[token("mul", |lexer| increase_token_then(lexer, EvmBuiltinFunction::MUL))]
        #[token("div", |lexer| increase_token_then(lexer, EvmBuiltinFunction::DIV))]
        #[token("sdiv", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SDIV))]
        #[token("mod", |lexer| increase_token_then(lexer, EvmBuiltinFunction::MOD))]
        #[token("smod", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SMOD))]
        #[token("exp", |lexer| increase_token_then(lexer, EvmBuiltinFunction::EXP))]
        #[token("not", |lexer| increase_token_then(lexer, EvmBuiltinFunction::NOT))]
        #[token("lt", |lexer| increase_token_then(lexer, EvmBuiltinFunction::LT))]
        #[token("gt", |lexer| increase_token_then(lexer, EvmBuiltinFunction::GT))]
        #[token("slt", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SLT))]
        #[token("sgt", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SGT))]
        #[token("eq", |lexer| increase_token_then(lexer, EvmBuiltinFunction::EQ))]
        #[token("iszero", |lexer| increase_token_then(lexer, EvmBuiltinFunction::ISZERO))]
        #[token("and", |lexer| increase_token_then(lexer, EvmBuiltinFunction::AND))]
        #[token("or", |lexer| increase_token_then(lexer, EvmBuiltinFunction::OR))]
        #[token("xor", |lexer| increase_token_then(lexer, EvmBuiltinFunction::XOR))]
        #[token("byte", |lexer| increase_token_then(lexer, EvmBuiltinFunction::BYTE))]
        #[token("shl", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SHL))]
        #[token("shr", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SHR))]
        #[token("sar", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SAR))]
        #[token("clz", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CLZ))]
        #[token("addmod", |lexer| increase_token_then(lexer, EvmBuiltinFunction::ADDMOD))]
        #[token("mulmod", |lexer| increase_token_then(lexer, EvmBuiltinFunction::MULMOD))]
        #[token("signextend", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SIGNEXTEND))]
        #[token("keccak256", |lexer| increase_token_then(lexer, EvmBuiltinFunction::KECCAK256))]
        #[token("pop", |lexer| increase_token_then(lexer, EvmBuiltinFunction::POP))]
        #[token("mload", |lexer| increase_token_then(lexer, EvmBuiltinFunction::MLOAD))]
        #[token("mstore", |lexer| increase_token_then(lexer, EvmBuiltinFunction::MSTORE))]
        #[token("mstore8", |lexer| increase_token_then(lexer, EvmBuiltinFunction::MSTORE8))]
        #[token("sload", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SLOAD))]
        #[token("sstore", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SSTORE))]
        #[token("tload", |lexer| increase_token_then(lexer, EvmBuiltinFunction::TLOAD))]
        #[token("tstore", |lexer| increase_token_then(lexer, EvmBuiltinFunction::TSTORE))]
        #[token("msize", |lexer| increase_token_then(lexer, EvmBuiltinFunction::MSIZE))]
        #[token("gas", |lexer| increase_token_then(lexer, EvmBuiltinFunction::GAS))]
        #[token("address", |lexer| increase_token_then(lexer, EvmBuiltinFunction::ADDRESS))]
        #[token("balance", |lexer| increase_token_then(lexer, EvmBuiltinFunction::BALANCE))]
        #[token("selfbalance", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SELFBALANCE))]
        #[token("caller", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CALLER))]
        #[token("callvalue", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CALLVALUE))]
        #[token("calldataload", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CALLDATALOAD))]
        #[token("calldatasize", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CALLDATASIZE))]
        #[token("calldatacopy", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CALLDATACOPY))]
        #[token("extcodesize", |lexer| increase_token_then(lexer, EvmBuiltinFunction::EXTCODESIZE))]
        #[token("extcodecopy", |lexer| increase_token_then(lexer, EvmBuiltinFunction::EXTCODECOPY))]
        #[token("returndatasize", |lexer| increase_token_then(lexer, EvmBuiltinFunction::RETURNDATASIZE))]
        #[token("returndatacopy", |lexer| increase_token_then(lexer, EvmBuiltinFunction::RETURNDATACOPY))]
        #[token("mcopy", |lexer| increase_token_then(lexer, EvmBuiltinFunction::MCOPY))]
        #[token("extcodehash", |lexer| increase_token_then(lexer, EvmBuiltinFunction::EXTCODEHASH))]
        #[token("create", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CREATE))]
        #[token("create2", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CREATE2))]
        #[token("call", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CALL))]
        #[token("callcode", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CALLCODE))]
        #[token("delegatecall", |lexer| increase_token_then(lexer, EvmBuiltinFunction::DELEGATECALL))]
        #[token("staticcall", |lexer| increase_token_then(lexer, EvmBuiltinFunction::STATICCALL))]
        #[token("return", |lexer| increase_token_then(lexer, EvmBuiltinFunction::RETURN))]
        #[token("revert", |lexer| increase_token_then(lexer, EvmBuiltinFunction::REVERT))]
        #[token("selfdestruct", |lexer| increase_token_then(lexer, EvmBuiltinFunction::SELFDESTRUCT))]
        #[token("invalid", |lexer| increase_token_then(lexer, EvmBuiltinFunction::INVALID))]
        #[token("log0", |lexer| increase_token_then(lexer, EvmBuiltinFunction::LOG0))]
        #[token("log1", |lexer| increase_token_then(lexer, EvmBuiltinFunction::LOG1))]
        #[token("log2", |lexer| increase_token_then(lexer, EvmBuiltinFunction::LOG2))]
        #[token("log3", |lexer| increase_token_then(lexer, EvmBuiltinFunction::LOG3))]
        #[token("log4", |lexer| increase_token_then(lexer, EvmBuiltinFunction::LOG4))]
        #[token("chainid", |lexer| increase_token_then(lexer, EvmBuiltinFunction::CHAINID))]
        #[token("origin", |lexer| increase_token_then(lexer, EvmBuiltinFunction::ORIGIN))]
        #[token("gasprice", |lexer| increase_token_then(lexer, EvmBuiltinFunction::GASPRICE))]
        #[token("blockhash", |lexer| increase_token_then(lexer, EvmBuiltinFunction::BLOCKHASH))]
        #[token("blobhash", |lexer| increase_token_then(lexer, EvmBuiltinFunction::BLOBHASH))]
        #[token("coinbase", |lexer| increase_token_then(lexer, EvmBuiltinFunction::COINBASE))]
        #[token("timestamp", |lexer| increase_token_then(lexer, EvmBuiltinFunction::TIMESTAMP))]
        #[token("number", |lexer| increase_token_then(lexer, EvmBuiltinFunction::NUMBER))]
        #[token("difficulty", |lexer| increase_token_then(lexer, EvmBuiltinFunction::DIFFICULTY))]
        #[token("prevrandao", |lexer| increase_token_then(lexer, EvmBuiltinFunction::PREVRANDAO))]
        #[token("gaslimit", |lexer| increase_token_then(lexer, EvmBuiltinFunction::GASLIMIT))]
        #[token("basefee", |lexer| increase_token_then(lexer, EvmBuiltinFunction::BASEFEE))]
        #[token("blobbasefee", |lexer| increase_token_then(lexer, EvmBuiltinFunction::BLOBBASEFEE))]
        EvmBuiltin(EvmBuiltinFunction),
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn empty_single_quoted_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token>) -> Result<Lit, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          Error::empty_single_quote(l.span().into())
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn empty_double_quoted_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token>) -> Result<Lit, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          Error::empty_double_quote(l.span().into())
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_double_quoted_regular_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token>) -> Result<Lit, Errors>{
        Err(increase_token_on_err(lexer, |l| {
          crate::error::StringError::unclosed_double_quote(l.span().into()).into()
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_single_quoted_regular_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token>) -> Result<Lit, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          crate::error::StringError::unclosed_single_quote(l.span().into()).into()
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_double_quoted_hex_string_error <'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token>) -> Result<Lit, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          crate::error::HexStringError::unclosed_double_quote(l.span().into()).into()
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_single_quoted_hex_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token>) -> Result<Lit, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          crate::error::HexStringError::unclosed_single_quote(l.span().into()).into()
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn malformed_hex_literal_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token>) -> Result<Lit, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          crate::error::yul::HexadecimalError::malformed(l.span().into()).into()
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_on_err<'b $(: $lt)?, $($lt: 'b,)?>(
        lexer: &mut Lexer<'b, Token>,
        f: impl FnOnce(&mut Lexer<'b, Token>) -> Error,
      ) -> Errors {
        lexer.increase_token();
        f(lexer).into()
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_then<'b $(: $lt)?, $($lt: 'b,)? O>(
        lexer: &mut Lexer<'b, Token>,
        output: O,
      ) -> O {
        lexer.increase_token();
        output
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_then_with<'b $(: $lt)?, $($lt: 'b,)? O>(
        lexer: &mut Lexer<'b, Token>,
        output: impl FnOnce(&mut Lexer<'b, Token>) -> O,
      ) -> O {
        lexer.increase_token();
        output(lexer)
      }
    }
  }
}

pub(super) use token;
