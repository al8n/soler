macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty $(,)?)) => {
    #[allow(single_use_lifetimes)]
    mod $mod {
      use logosky::{
        Lexable, Logos, logos::Lexer,
        utils::tracker::{LimitExceeded, Limiter, Tracker},
      };

      use crate::{
        utils::{Wrapper, sealed::{
          DoubleQuotedHexStrLexer, DoubleQuotedRegularStrLexer, SingleQuotedHexStrLexer,
          SingleQuotedRegularStrLexer,
        }},
        yul::{handlers, Lit, LitHexStr, LitRegularStr, lossless},
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
      impl<'b $(: $lt)?, $($lt: 'b)?> logosky::Token<'b> for lossless::Token<$slice> {
        type Kind = lossless::TokenKind;
        type Char = $char;
        type Logos = Token $(<$lt>)?;

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }
      }

      #[doc(hidden)]
      #[derive(Logos, Clone)]
      #[logos(
        crate = logosky::logos,
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
      pub enum Token $(<$lt>)? {
        #[token(" ", increase_token_and_check_on_token)]
        Space,
        #[token("\t", increase_token_and_check_on_token)]
        Tab,
        #[token("\n", increase_token_and_check_on_token)]
        NewLine,
        #[token("\r", increase_token_and_check_on_token)]
        CarriageReturn,
        #[token("\r\n", increase_token_and_check_on_token)]
        CarriageReturnNewLine,
        #[token("\u{000C}", increase_token_and_check_on_token)]
        FormFeed,

        #[token(":=", increase_token_and_check_on_token)]
        Assign,
        #[token("->", increase_token_and_check_on_token)]
        ThinArrow,
        #[token("{", |lexer| lexer.increase_both_and_check().map_err(|e| Errors::from(Error::State(e))))]
        LBrace,
        #[token("}", |lexer| lexer.decrease_recursion())]
        RBrace,
        #[token("(", |lexer| lexer.increase_both_and_check().map_err(|e| Errors::from(Error::State(e))))]
        LParen,
        #[token(")", |lexer| lexer.decrease_recursion())]
        RParen,
        #[token(".", increase_token_and_check_on_token)]
        Dot,
        #[token(",", increase_token_and_check_on_token)]
        Comma,

        #[token("leave", increase_token_and_check_on_token)]
        Leave,
        #[token("continue", increase_token_and_check_on_token)]
        Continue,
        #[token("break", increase_token_and_check_on_token)]
        Break,
        #[token("switch", increase_token_and_check_on_token)]
        Switch,
        #[token("case", increase_token_and_check_on_token)]
        Case,
        #[token("default", increase_token_and_check_on_token)]
        Default,
        #[token("function", increase_token_and_check_on_token)]
        Function,
        #[token("let", increase_token_and_check_on_token)]
        Let,
        #[token("if", increase_token_and_check_on_token)]
        If,
        #[token("for", increase_token_and_check_on_token)]
        For,

        #[regex(r"//[^\r\n]*", |lexer| increase_token_and_check_on_token_with(lexer, |lexer| lexer.slice()))]
        LineComment($slice),

        #[regex(r"/\*([^*]|\*+[^*/])*\*+/", |lexer| increase_token_and_check_on_token_with(lexer, |lexer| lexer.slice()))]
        MultiLineComment($slice),

        #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*", |lexer| increase_token_and_check_on_token_with(lexer, |lexer| lexer.slice()))]
        Identifier($slice),

        #[token("true", |lexer| increase_token_and_check_on_token_with(lexer, |lexer| Lit::lit_true(lexer.slice())))]
        #[token("false", |lexer| increase_token_and_check_on_token_with(lexer, |lexer| Lit::lit_false(lexer.slice())))]
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
        })]

        // Double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)\"", |lexer| {
          increase_token_and_check_on_token_with(lexer, |l| Lit::lit_double_quoted_hex_string(l.slice()))
        })]
        // Error handling branches for double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)", unclosed_double_quoted_hex_string_error)]
        #[token("hex\"", |lexer| {
          match <LitHexStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedHexStrLexer::<logosky::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer))
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
          increase_token_and_check_on_token_with(lexer, |l| Lit::lit_single_quoted_hex_string(l.slice()))
        })]
        // Error handling branches for single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)", unclosed_single_quoted_hex_string_error)]
        #[token("hex'", |lexer| {
          match <LitHexStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedHexStrLexer::<logosky::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer))
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
          increase_token_and_check_on_token_with(lexer, |l| Lit::lit_double_quoted_regular_string(l.slice()))
        })]
        // Error handling branches for double quoted non-empty string literal lexing 
        #[token(r#""""#, empty_double_quoted_string_error)]
        #[regex(r#""(?&double_quoted_chars)"#, unclosed_double_quoted_regular_string_error)]
        #[token("\"", |lexer| {
          match <LitRegularStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedRegularStrLexer::<logosky::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer))
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
          increase_token_and_check_on_token_with(lexer, |l| Lit::lit_single_quoted_regular_string(l.slice()))
        })]
        // Error handling branches for single quoted non-empty string literal lexing
        #[token("''", empty_single_quoted_string_error)]
        #[regex(r"'(?&single_quoted_chars)", unclosed_single_quoted_regular_string_error)]
        #[token("\'", |lexer| {
          match <LitRegularStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedRegularStrLexer::<logosky::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer))
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
        Lit(Lit<$slice>),

        #[cfg(feature = "evm")]
        #[token("stop", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Stop))]
        #[token("add", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Add))]
        #[token("sub", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Sub))]
        #[token("mul", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Mul))]
        #[token("div", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Div))]
        #[token("sdiv", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Sdiv))]
        #[token("mod", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Mod))]
        #[token("smod", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Smod))]
        #[token("exp", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Exp))]
        #[token("not", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Not))]
        #[token("lt", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Lt))]
        #[token("gt", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Gt))]
        #[token("slt", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Slt))]
        #[token("sgt", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Sgt))]
        #[token("eq", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Eq))]
        #[token("iszero", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Iszero))]
        #[token("and", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::And))]
        #[token("or", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Or))]
        #[token("xor", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Xor))]
        #[token("byte", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Byte))]
        #[token("shl", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Shl))]
        #[token("shr", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Shr))]
        #[token("sar", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Sar))]
        #[token("clz", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Clz))]
        #[token("addmod", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Addmod))]
        #[token("mulmod", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Mulmod))]
        #[token("signextend", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Signextend))]
        #[token("keccak256", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Keccak256))]
        #[token("pop", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Pop))]
        #[token("mload", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Mload))]
        #[token("mstore", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Mstore))]
        #[token("mstore8", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Mstore8))]
        #[token("sload", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Sload))]
        #[token("sstore", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Sstore))]
        #[token("tload", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Tload))]
        #[token("tstore", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Tstore))]
        #[token("msize", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Msize))]
        #[token("gas", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Gas))]
        #[token("address", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Address))]
        #[token("balance", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Balance))]
        #[token("selfbalance", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Selfbalance))]
        #[token("caller", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Caller))]
        #[token("callvalue", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Callvalue))]
        #[token("calldataload", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Calldataload))]
        #[token("calldatasize", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Calldatasize))]
        #[token("calldatacopy", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Calldatacopy))]
        #[token("extcodesize", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Extcodesize))]
        #[token("extcodecopy", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Extcodecopy))]
        #[token("returndatasize", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Returndatasize))]
        #[token("returndatacopy", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Returndatacopy))]
        #[token("mcopy", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Mcopy))]
        #[token("extcodehash", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Extcodehash))]
        #[token("create", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Create))]
        #[token("create2", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Create2))]
        #[token("call", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Call))]
        #[token("callcode", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Callcode))]
        #[token("delegatecall", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Delegatecall))]
        #[token("staticcall", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Staticcall))]
        #[token("return", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Return))]
        #[token("revert", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Revert))]
        #[token("selfdestruct", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Selfdestruct))]
        #[token("invalid", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Invalid))]
        #[token("log0", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Log0))]
        #[token("log1", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Log1))]
        #[token("log2", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Log2))]
        #[token("log3", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Log3))]
        #[token("log4", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Log4))]
        #[token("chainid", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Chainid))]
        #[token("origin", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Origin))]
        #[token("gasprice", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Gasprice))]
        #[token("blockhash", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Blockhash))]
        #[token("blobhash", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Blobhash))]
        #[token("coinbase", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Coinbase))]
        #[token("timestamp", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Timestamp))]
        #[token("number", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Number))]
        #[token("difficulty", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Difficulty))]
        #[token("prevrandao", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Prevrandao))]
        #[token("gaslimit", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Gaslimit))]
        #[token("basefee", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Basefee))]
        #[token("blobbasefee", |lexer| increase_token_and_check_on_token_with_output(lexer, EvmBuiltinFunction::Blobbasefee))]
        EvmBuiltin(EvmBuiltinFunction),
      }

      impl$(<$lt>)? From<Token $(<$lt>)?> for lossless::Token<$slice> {
        #[cfg_attr(not(tarpaulin), inline(always))]
        fn from(value: Token $(<$lt>)?) -> Self {
          match value {
            Token::Space => Self::Space,
            Token::Tab => Self::Tab,
            Token::NewLine => Self::NewLine,
            Token::CarriageReturn => Self::CarriageReturn,
            Token::CarriageReturnNewLine => Self::CarriageReturnNewLine,
            Token::FormFeed => Self::FormFeed,
            Token::Assign => Self::Assign,
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
            Token::LineComment(slice) => Self::LineComment(slice),
            Token::MultiLineComment(slice) => Self::MultiLineComment(slice),
            Token::Identifier(slice) => Self::Identifier(slice),
            Token::Lit(lit) => Self::Lit(lit),
            #[cfg(feature = "evm")]
            Token::EvmBuiltin(func) => Self::EvmBuiltin(func),
          }
        }
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn empty_single_quoted_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token $(<$lt>)? >) -> Result<Lit<$slice>, Errors> {
        increase_token_and_check_on_error_token(lexer, |l| {
          Error::empty_single_quote(l.span().into())
        })
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn empty_double_quoted_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token $(<$lt>)? >) -> Result<Lit<$slice>, Errors> {
        increase_token_and_check_on_error_token(lexer, |l| {
          Error::empty_double_quote(l.span().into())
        })
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_double_quoted_regular_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token $(<$lt>)? >) -> Result<Lit<$slice>, Errors>{
        increase_token_and_check_on_error_token(lexer, |l| {
          crate::error::StringError::unclosed_double_quote(l.span().into()).into()
        })
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_single_quoted_regular_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token $(<$lt>)? >) -> Result<Lit<$slice>, Errors> {
        increase_token_and_check_on_error_token(lexer, |l| {
          crate::error::StringError::unclosed_single_quote(l.span().into()).into()
        })
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_double_quoted_hex_string_error <'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token $(<$lt>)? >) -> Result<Lit<$slice>, Errors> {
        increase_token_and_check_on_error_token(lexer, |l| {
          crate::error::HexStringError::unclosed_double_quote(l.span().into()).into()
        })
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_single_quoted_hex_string_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut Lexer<'b, Token $(<$lt>)? >) -> Result<Lit<$slice>, Errors> {
        increase_token_and_check_on_error_token(lexer, |l| {
          crate::error::HexStringError::unclosed_single_quote(l.span().into()).into()
        })
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_and_check_on_error_token<'b $(: $lt)?, $($lt: 'b,)? O>(
        lexer: &mut Lexer<'b, Token $(<$lt>)? >,
        f: impl FnOnce(&mut Lexer<'b, Token $(<$lt>)? >) -> Error,
      ) -> Result<O, Errors> {
        match lexer.increase_token_and_check() {
          Ok(_) => Err(f(lexer).into()),
          Err(e) => {
            let mut errs = Errors::with_capacity(2);
            errs.push(Error::State(e));
            errs.push(f(lexer));
            Err(errs)
          }
        }
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_and_check_on_token<'b $(: $lt)?, $($lt: 'b,)?>(
        lexer: &mut Lexer<'b, Token $(<$lt>)? >,
      ) -> Result<(), Errors> {
        lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_and_check_on_token_with_output<'b $(: $lt)?, $($lt: 'b,)? O>(
        lexer: &mut Lexer<'b, Token $(<$lt>)? >,
        output: O,
      ) -> Result<O, Errors> {
        match lexer.increase_token_and_check() {
          Ok(_) => Ok(output),
          Err(e) => Err(Errors::from(Error::State(e))),
        }
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_and_check_on_token_with<'b $(: $lt)?, $($lt: 'b,)? O>(
        lexer: &mut Lexer<'b, Token $(<$lt>)? >,
        output: impl FnOnce(&mut Lexer<'b, Token $(<$lt>)? >) -> O,
      ) -> Result<O, Errors> {
        match lexer.increase_token_and_check() {
          Ok(_) => Ok(output(lexer)),
          Err(e) => Err(Errors::from(Error::State(e))),
        }
      }
    }
  }
}

pub(super) use token;
