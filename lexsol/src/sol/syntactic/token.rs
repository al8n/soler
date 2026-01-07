macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($($utf8:literal,)?$slice: ty, $char: ty, $handlers:ident $(,)?)) => {
    #[allow(single_use_lifetimes)]
    mod $mod {
      use tokit::{
        lexer::Lexable,
        logos::Logos,
        SimpleSpan,
        state::recursion_tracker::{RecursionLimitExceeded, RecursionLimiter, RecursionTracker},
      };

      use crate::{
        error::sol as error,
        sol::{Denomination, FixedBytes, Int, Lit, LitUnicodeStr, Uint, handlers, syntactic},
        types::{LitHexStr, LitRegularStr},
        utils::{
          Wrapper,
          sealed::{
            DoubleQuotedHexStrLexer, DoubleQuotedRegularStrLexer, DoubleQuotedUnicodeStrLexer,
            SingleQuotedHexStrLexer, SingleQuotedRegularStrLexer, SingleQuotedUnicodeStrLexer,
          },
        },
      };

      type UnicodeStringError = error::UnicodeStringError<$char>;
      type StringError = crate::error::StringError<$char>;
      type HexStringError = crate::error::HexStringError<$char>;
      type Error = error::Error<syntactic::SyntaxKind, $char, RecursionLimitExceeded>;
      type Errors = error::Errors<syntactic::SyntaxKind, $char, RecursionLimitExceeded>;
      type UnderlyingErrorContainer = <Errors as Wrapper>::Underlying;

      #[allow(warnings)]
      impl<'b $(: $lt)?, $($lt: 'b)?> tokit::Token<'b> for syntactic::Token<$slice> {
        type Kind = syntactic::TokenKind;
        type Error = Errors;

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn kind(&self) -> Self::Kind {
          self.kind()
        }

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn is_trivia(&self) -> bool {
          false
        }
      }

      #[allow(warnings)]
      impl<'b $(: $lt)?, $($lt: 'b)?> crate::TokenBridge<'b> for syntactic::Token<$slice> {
        type Logos = Token;

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn kind(value: &Self::Logos) -> syntactic::TokenKind {
          match value {
            Token::Abstract => syntactic::TokenKind::Abstract,
            Token::Address => syntactic::TokenKind::Address,
            Token::Anonymous => syntactic::TokenKind::Anonymous,
            Token::As => syntactic::TokenKind::As,
            Token::Assembly => syntactic::TokenKind::Assembly,
            Token::Bool => syntactic::TokenKind::Bool,
            Token::Break => syntactic::TokenKind::Break,
            Token::Bytes => syntactic::TokenKind::Bytes,
            Token::Calldata => syntactic::TokenKind::Calldata,
            Token::Catch => syntactic::TokenKind::Catch,
            Token::Constant => syntactic::TokenKind::Constant,
            Token::Constructor => syntactic::TokenKind::Constructor,
            Token::Continue => syntactic::TokenKind::Continue,
            Token::Contract => syntactic::TokenKind::Contract,
            Token::Delete => syntactic::TokenKind::Delete,
            Token::Do => syntactic::TokenKind::Do,
            Token::Else => syntactic::TokenKind::Else,
            Token::Emit => syntactic::TokenKind::Emit,
            Token::Enum => syntactic::TokenKind::Enum,
            Token::Event => syntactic::TokenKind::Event,
            Token::External => syntactic::TokenKind::External,
            Token::Fallback => syntactic::TokenKind::Fallback,
            Token::For => syntactic::TokenKind::For,
            Token::Function => syntactic::TokenKind::Function,
            Token::If => syntactic::TokenKind::If,
            Token::Immutable => syntactic::TokenKind::Immutable,
            Token::Import => syntactic::TokenKind::Import,
            Token::Indexed => syntactic::TokenKind::Indexed,
            Token::Interface => syntactic::TokenKind::Interface,
            Token::Internal => syntactic::TokenKind::Internal,
            Token::Is => syntactic::TokenKind::Is,
            Token::Library => syntactic::TokenKind::Library,
            Token::Mapping => syntactic::TokenKind::Mapping,
            Token::Memory => syntactic::TokenKind::Memory,
            Token::Modifier => syntactic::TokenKind::Modifier,
            Token::New => syntactic::TokenKind::New,
            Token::Override => syntactic::TokenKind::Override,
            Token::Payable => syntactic::TokenKind::Payable,
            Token::Private => syntactic::TokenKind::Private,
            Token::Public => syntactic::TokenKind::Public,
            Token::Pure => syntactic::TokenKind::Pure,
            Token::Pragma => syntactic::TokenKind::Pragma,
            Token::Receive => syntactic::TokenKind::Receive,
            Token::Return => syntactic::TokenKind::Return,
            Token::Returns => syntactic::TokenKind::Returns,
            Token::Storage => syntactic::TokenKind::Storage,
            Token::String => syntactic::TokenKind::String,
            Token::Struct => syntactic::TokenKind::Struct,
            Token::Try => syntactic::TokenKind::Try,
            Token::Type => syntactic::TokenKind::Type,
            Token::Unchecked => syntactic::TokenKind::Unchecked,
            Token::Using => syntactic::TokenKind::Using,
            Token::View => syntactic::TokenKind::View,
            Token::Virtual => syntactic::TokenKind::Virtual,
            Token::While => syntactic::TokenKind::While,
            Token::LParen => syntactic::TokenKind::LParen,
            Token::RParen => syntactic::TokenKind::RParen,
            Token::LBracket => syntactic::TokenKind::LBracket,
            Token::RBracket => syntactic::TokenKind::RBracket,
            Token::LBrace => syntactic::TokenKind::LBrace,
            Token::RBrace => syntactic::TokenKind::RBrace,
            Token::Colon => syntactic::TokenKind::Colon,
            Token::Semicolon => syntactic::TokenKind::Semicolon,
            Token::Dot => syntactic::TokenKind::Dot,
            Token::Question => syntactic::TokenKind::Question,
            Token::FatArrow => syntactic::TokenKind::FatArrow,
            Token::ThinArrow => syntactic::TokenKind::ThinArrow,
            Token::Assign => syntactic::TokenKind::Assign,

            Token::BitOrAssign => syntactic::TokenKind::BitOrAssign,
            Token::BitAndAssign => syntactic::TokenKind::BitAndAssign,
            Token::BitXorAssign => syntactic::TokenKind::BitXorAssign,
            Token::ShlAssign => syntactic::TokenKind::ShlAssign,
            Token::SarAssign => syntactic::TokenKind::SarAssign,
            Token::ShrAssign => syntactic::TokenKind::ShrAssign,
            Token::AddAssign => syntactic::TokenKind::AddAssign,
            Token::SubAssign => syntactic::TokenKind::SubAssign,
            Token::MulAssign => syntactic::TokenKind::MulAssign,
            Token::DivAssign => syntactic::TokenKind::DivAssign,
            Token::ModAssign => syntactic::TokenKind::ModAssign,
            Token::Comma => syntactic::TokenKind::Comma,

            Token::Or => syntactic::TokenKind::Or,
            Token::And => syntactic::TokenKind::And,
            Token::BitOr => syntactic::TokenKind::BitOr,
            Token::BitAnd => syntactic::TokenKind::BitAnd,
            Token::BitXor => syntactic::TokenKind::BitXor,
            Token::Shl => syntactic::TokenKind::Shl,
            Token::Sar => syntactic::TokenKind::Sar,
            Token::Shr => syntactic::TokenKind::Shr,
            Token::Add => syntactic::TokenKind::Add,
            Token::Sub => syntactic::TokenKind::Sub,
            Token::Mul => syntactic::TokenKind::Mul,
            Token::Div => syntactic::TokenKind::Div,
            Token::Mod => syntactic::TokenKind::Mod,
            Token::Exp => syntactic::TokenKind::Exp,

            Token::Eq => syntactic::TokenKind::Eq,
            Token::Ne => syntactic::TokenKind::Ne,
            Token::Lt => syntactic::TokenKind::Lt,
            Token::Le => syntactic::TokenKind::Le,
            Token::Gt => syntactic::TokenKind::Gt,
            Token::Ge => syntactic::TokenKind::Ge,

            Token::Not => syntactic::TokenKind::Not,
            Token::BitNot => syntactic::TokenKind::BitNot,
            Token::Inc => syntactic::TokenKind::Inc,
            Token::Dec => syntactic::TokenKind::Dec,

            Token::FixedBytes(val) => syntactic::TokenKind::FixedBytes(*val),
            Token::Denomination(val) => syntactic::TokenKind::Denomination(*val),
            Token::Int(val) => syntactic::TokenKind::Int(*val),
            Token::Uint(val) => syntactic::TokenKind::Uint(*val),
            Token::Fixed => syntactic::TokenKind::Fixed,
            Token::UFixed => syntactic::TokenKind::UFixed,
            Token::Lit(val) => syntactic::TokenKind::Lit(*val),
            Token::Identifier => syntactic::TokenKind::Identifier,
          }
        }
      }

      /// Token
      #[derive(Logos, Clone, Debug, PartialEq, Eq, Hash)]
      #[logos(
        crate = tokit::logos,
        $(utf8 = $utf8,)?
        extras = RecursionLimiter,
        error(Errors, |l| Errors::from(handlers::$handlers::default_error(l)))
      )]
      #[logos(skip r"[ \t\r\n\u{000C}]+|//[^\r\n]*|/\*([^*]|\*+[^*/])*\*+/")]
      #[logos(subpattern double_quoted_printable = "[\u{0020}-\u{0021}\u{0023}-\u{005B}\u{005D}-\u{007E}]")]
      #[logos(subpattern single_quoted_printable = "[\u{0020}-\u{0026}\u{0028}-\u{005B}\u{005D}-\u{007E}]")]
      #[logos(subpattern double_quoted_unicode = r#"[^"\r\n\\]"#)]
      #[logos(subpattern single_quoted_unicode = r"[^'\r\n\\]")]
      #[logos(subpattern escape_sequence = r#"\\['"\\nrt\n\r]|\\u[0-9a-fA-F]{4}|\\x[0-9a-fA-F]{2}"#)]
      #[logos(subpattern double_quoted_char = "(?&double_quoted_printable)|(?&escape_sequence)")]
      #[logos(subpattern single_quoted_char = "(?&single_quoted_printable)|(?&escape_sequence)")]
      #[logos(subpattern unicode_double_quoted_char = "((?&double_quoted_unicode)|(?&escape_sequence))")]
      #[logos(subpattern unicode_single_quoted_char = "((?&single_quoted_unicode)|(?&escape_sequence))")]
      #[logos(subpattern double_quoted_chars = r#"(?&double_quoted_char)+"#)]
      #[logos(subpattern single_quoted_chars = r#"(?&single_quoted_char)+"#)]
      #[logos(subpattern unicode_double_quoted_chars = "(?&unicode_double_quoted_char)*")]
      #[logos(subpattern unicode_single_quoted_chars = "(?&unicode_single_quoted_char)*")]
      #[logos(subpattern hex_string_digit = "[0-9A-Fa-f]")]
      #[logos(subpattern hex_string_digit_pair = "(?&hex_string_digit){2}")]
      #[logos(subpattern hex_string_content = "(?&hex_string_digit_pair)(?:_(?&hex_string_digit_pair))*")]
      #[logos(subpattern hex_digit = r"[0-9A-Fa-f]")]
      #[logos(subpattern hex_digits = r"(?&hex_digit)(?:_?(?&hex_digit))*")]
      // digits with underscores between them
      #[logos(subpattern dec_digit = r"[0-9]")]
      #[logos(subpattern dec_int   = r"(?&dec_digit)(?:_?(?&dec_digit))*")]
      // exponent: e or E, optional +/-, then digits-with-underscores
      #[logos(subpattern dec_exp   = r"[eE][+-]?(?&dec_int)")]
      pub enum Token {
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
        #[token("pragma")]
        Pragma,
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

        #[token("(", |lexer| lexer.increase())]
        LParen,
        #[token(")", |lexer| lexer.decrease())]
        RParen,
        #[token("[", |lexer| lexer.increase())]
        LBracket,
        #[token("]", |lexer| lexer.decrease())]
        RBracket,
        #[token("{", |lexer| lexer.increase())]
        LBrace,
        #[token("}", |lexer| lexer.decrease())]
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
        Fixed,

        #[token("ufixed")]
        #[regex("ufixed[1-9][0-9]*x[1-9][0-9]*")]
        UFixed,

        // ==================================== Boolean literals ====================================
        #[token("true", |_| Lit::lit_true(()))]
        #[token("false", |_| Lit::lit_false(()))]
        // ==================================== Empty quoted string literals ====================================
        #[token("\"\"", |_| Lit::lit_empty_double_quoted_string(()))]
        #[token("''", |_| Lit::lit_empty_single_quoted_string(()))]
        // ==================================== Regular string literals ====================================
        // Double quoted non-empty string literal lexing
        #[regex(r#""(?&double_quoted_chars)""#, |_| Lit::lit_double_quoted_regular_string(()))]
        // Error handling branches for double quoted non-empty string literal lexing
        #[regex(r#""(?&double_quoted_chars)"#, |lexer| unclosed_double_quoted_regular_string_error(lexer.span().into()))]
        #[token("\"", |lexer| {
          <LitRegularStr as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedRegularStrLexer::<tokit::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
        })]
        // Single quoted non-empty string literal lexing
        #[regex(r"'(?&single_quoted_chars)'", |_| Lit::lit_single_quoted_regular_string(()))]
        // Error handling branches for single quoted non-empty string literal lexing
        #[regex(r"'(?&single_quoted_chars)", |lexer| unclosed_single_quoted_regular_string_error(lexer.span().into()))]
        #[token("\'", |lexer| {
          <LitRegularStr as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedRegularStrLexer::<tokit::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
        })]
        // ==================================== Hex string literals ====================================
        // Double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)\"", |_| Lit::lit_double_quoted_hex_string(()))]
        // Error handling branches for double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)", |lexer| unclosed_double_quoted_hex_string_error(lexer.span().into()))]
        #[token("hex\"", |lexer| {
          <LitHexStr as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedHexStrLexer::<tokit::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
        })]
        // Single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)'", |_| Lit::lit_single_quoted_hex_string(()))]
        // Error handling branches for single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)", |lexer| unclosed_single_quoted_hex_string_error(lexer.span().into()))]
        #[token("hex'", |lexer| {
          <LitHexStr as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedHexStrLexer::<tokit::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer))
            .map(Into::into)
            .map_err(Errors::from_underlying)
        })]
        // ==================================== Unicode string literals ====================================
        // Double quoted unicode string literal lexing
        #[regex(r#"unicode"(?&unicode_double_quoted_chars)""#, |_| Lit::lit_double_quoted_unicode_string(()))]
        // Error handling branches for double quoted unicode string literal lexing
        #[token("unicode\"", |lexer| {
            <LitUnicodeStr as Lexable<_, UnderlyingErrorContainer>>::lex(DoubleQuotedUnicodeStrLexer::<tokit::logos::Lexer<'_, _>, $char, UnicodeStringError, Error>::from_mut(lexer))
              .map(Into::into)
              .map_err(Errors::from_underlying)
        })]
        // Single quoted unicode string literal lexing
        #[regex("unicode'(?&unicode_single_quoted_chars)'", |_| Lit::lit_single_quoted_unicode_string(()))]
        // Error handling branches for single quoted unicode string literal lexing
        #[token("unicode\'", |lexer| {
            <LitUnicodeStr as Lexable<_, UnderlyingErrorContainer>>::lex(SingleQuotedUnicodeStrLexer::<tokit::logos::Lexer<'_, _>, $char, UnicodeStringError, Error>::from_mut(lexer))
              .map(Into::into)
              .map_err(Errors::from_underlying)
        })]
        // ==================================== Number literals ====================================
        #[regex("0x(?&hex_digits)", handlers::$handlers::handle_hexadecimal_suffix, priority = 7)]
        #[regex("0x[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[regex("0X[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[regex("0[xX]{2,}[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[token("0x", handlers::$handlers::handle_hexadecimal_prefix_with_invalid_following)]

        #[regex(r"(?&dec_int)(?:\.(?&dec_int))?(?&dec_exp)?", handlers::$handlers::handle_decimal_suffix)]
        #[regex(r"0(?&dec_int)(?:\.(?&dec_int))?(?&dec_exp)?", handlers::$handlers::handle_leading_zero_and_suffix)]
        #[regex("[1-9][0-9_]+", handlers::$handlers::handle_malformed_decimal_suffix)]
        Lit(Lit),

        #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*")]
        Identifier,
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_double_quoted_regular_string_error(span: SimpleSpan) -> Result<Lit, Errors> {
        Err(Errors::from(Error::String(
          crate::error::StringError::unclosed_double_quote(span),
        )))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_single_quoted_regular_string_error(span: SimpleSpan) -> Result<Lit, Errors> {
        Err(Errors::from(Error::String(
          crate::error::StringError::unclosed_single_quote(span),
        )))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_double_quoted_hex_string_error(span: SimpleSpan) -> Result<Lit, Errors> {
        Err(Errors::from(Error::HexString(
          crate::error::HexStringError::unclosed_double_quote(span),
        )))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_single_quoted_hex_string_error(span: SimpleSpan) -> Result<Lit, Errors> {
        Err(Errors::from(Error::HexString(
          crate::error::HexStringError::unclosed_single_quote(span),
        )))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn malformed_hex_literal_error<'b $(: $lt)?, $($lt: 'b)?> (lexer: &mut tokit::logos::Lexer<'b,  Token>) -> Result<Lit, Errors> {
        Err(Error::from(crate::error::sol::HexadecimalError::malformed(lexer.span().into())).into())
      }
    }
  }
}

pub(super) use token;
