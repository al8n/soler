macro_rules! token {
  ($mod:ident $(<$lt:lifetime>)?($slice: ty, $char: ty, $handlers:ident, $source:ty $(,)?)) => {
    #[allow(single_use_lifetimes)]
    mod $mod {
      use tokit::{
        lexer::Lexable,
        logos::{Logos, Lexer},
        utils::tracker::{LimitExceeded, Limiter, Tracker},
        error::ErrorContainer,
      };

      use crate::{
        error::sol as error,
        sol::{Denomination, FixedBytes, Int, Lit, LitUnicodeStr, Uint, handlers, lossless},
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

      /// Token
      #[derive(Logos, Clone, Debug, PartialEq, Eq, Hash)]
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
      pub enum Token $(<$lt>)? {
        #[token(" ", |lexer| lexer.increase_token())]
        Space,
        #[token("\t", |lexer| lexer.increase_token())]
        Tab,
        #[token("\n", |lexer| lexer.increase_token())]
        NewLine,
        #[token("\r", |lexer| lexer.increase_token())]
        CarriageReturn,
        #[token("\r\n", |lexer| lexer.increase_token())]
        CarriageReturnNewLine,
        #[token("\u{000C}", |lexer| lexer.increase_token())]
        FormFeed,

        #[token("abstract", |lexer| lexer.increase_token())]
        Abstract,
        #[token("address", |lexer| lexer.increase_token())]
        Address,
        #[token("anonymous", |lexer| lexer.increase_token())]
        Anonymous,
        #[token("as", |lexer| lexer.increase_token())]
        As,
        #[token("assembly", |lexer| lexer.increase_token())]
        Assembly,
        #[token("bool", |lexer| lexer.increase_token())]
        Bool,
        #[token("break", |lexer| lexer.increase_token())]
        Break,
        #[token("bytes", |lexer| lexer.increase_token())]
        Bytes,
        #[token("calldata", |lexer| lexer.increase_token())]
        Calldata,
        #[token("catch", |lexer| lexer.increase_token())]
        Catch,
        #[token("constant", |lexer| lexer.increase_token())]
        Constant,
        #[token("constructor", |lexer| lexer.increase_token())]
        Constructor,
        #[token("continue", |lexer| lexer.increase_token())]
        Continue,
        #[token("contract", |lexer| lexer.increase_token())]
        Contract,
        #[token("delete", |lexer| lexer.increase_token())]
        Delete,
        #[token("do", |lexer| lexer.increase_token())]
        Do,
        #[token("else", |lexer| lexer.increase_token())]
        Else,
        #[token("emit", |lexer| lexer.increase_token())]
        Emit,
        #[token("enum", |lexer| lexer.increase_token())]
        Enum,
        #[token("event", |lexer| lexer.increase_token())]
        Event,
        #[token("external", |lexer| lexer.increase_token())]
        External,
        #[token("fallback", |lexer| lexer.increase_token())]
        Fallback,
        #[token("for", |lexer| lexer.increase_token())]
        For,
        #[token("function", |lexer| lexer.increase_token())]
        Function,
        #[token("if", |lexer| lexer.increase_token())]
        If,
        #[token("immutable", |lexer| lexer.increase_token())]
        Immutable,
        #[token("import", |lexer| lexer.increase_token())]
        Import,
        #[token("indexed", |lexer| lexer.increase_token())]
        Indexed,
        #[token("interface", |lexer| lexer.increase_token())]
        Interface,
        #[token("internal", |lexer| lexer.increase_token())]
        Internal,
        #[token("is", |lexer| lexer.increase_token())]
        Is,
        #[token("library", |lexer| lexer.increase_token())]
        Library,
        #[token("mapping", |lexer| lexer.increase_token())]
        Mapping,
        #[token("memory", |lexer| lexer.increase_token())]
        Memory,
        #[token("modifier", |lexer| lexer.increase_token())]
        Modifier,
        #[token("new", |lexer| lexer.increase_token())]
        New,
        #[token("override", |lexer| lexer.increase_token())]
        Override,
        #[token("payable", |lexer| lexer.increase_token())]
        Payable,
        #[token("private", |lexer| lexer.increase_token())]
        Private,
        #[token("public", |lexer| lexer.increase_token())]
        Public,
        #[token("pure", |lexer| lexer.increase_token())]
        Pure,
        #[token("pragma", |lexer| lexer.increase_token())]
        Pragma,
        #[token("receive", |lexer| lexer.increase_token())]
        Receive,
        #[token("return", |lexer| lexer.increase_token())]
        Return,
        #[token("returns", |lexer| lexer.increase_token())]
        Returns,
        #[token("storage", |lexer| lexer.increase_token())]
        Storage,
        #[token("string", |lexer| lexer.increase_token())]
        String,
        #[token("struct", |lexer| lexer.increase_token())]
        Struct,
        #[token("try", |lexer| lexer.increase_token())]
        Try,
        #[token("type", |lexer| lexer.increase_token())]
        Type,
        #[token("unchecked", |lexer| lexer.increase_token())]
        Unchecked,
        #[token("using", |lexer| lexer.increase_token())]
        Using,
        #[token("view", |lexer| lexer.increase_token())]
        View,
        #[token("virtual", |lexer| lexer.increase_token())]
        Virtual,
        #[token("while", |lexer| lexer.increase_token())]
        While,

        #[token("(", |lexer| lexer.increase_both())]
        LParen,
        #[token(")", |lexer| lexer.increase_token_and_decrease_recursion())]
        RParen,
        #[token("[", |lexer| lexer.increase_both())]
        LBracket,
        #[token("]", |lexer| lexer.increase_token_and_decrease_recursion())]
        RBracket,
        #[token("{", |lexer| lexer.increase_both())]
        LBrace,
        #[token("}", |lexer| lexer.increase_token_and_decrease_recursion())]
        RBrace,
        #[token(":", |lexer| lexer.increase_token())]
        Colon,
        #[token(";", |lexer| lexer.increase_token())]
        Semicolon,
        #[token(".", |lexer| lexer.increase_token())]
        Dot,
        #[token("?", |lexer| lexer.increase_token())]
        Question,
        #[token("=>", |lexer| lexer.increase_token())]
        FatArrow,
        #[token("->", |lexer| lexer.increase_token())]
        ThinArrow,
        #[token("=", |lexer| lexer.increase_token())]
        Assign,
        #[token("|=", |lexer| lexer.increase_token())]
        BitOrAssign,
        #[token("&=", |lexer| lexer.increase_token())]
        BitAndAssign,
        #[token("^=", |lexer| lexer.increase_token())]
        BitXorAssign,
        #[token("<<=", |lexer| lexer.increase_token())]
        ShlAssign,
        #[token(">>=", |lexer| lexer.increase_token())]
        SarAssign,
        #[token(">>>=", |lexer| lexer.increase_token())]
        ShrAssign,
        #[token("+=", |lexer| lexer.increase_token())]
        AddAssign,
        #[token("-=", |lexer| lexer.increase_token())]
        SubAssign,
        #[token("*=", |lexer| lexer.increase_token())]
        MulAssign,
        #[token("/=", |lexer| lexer.increase_token())]
        DivAssign,
        #[token("%=", |lexer| lexer.increase_token())]
        ModAssign,
        #[token(",", |lexer| lexer.increase_token())]
        Comma,
        #[token("||", |lexer| lexer.increase_token())]
        Or,
        #[token("&&", |lexer| lexer.increase_token())]
        And,
        #[token("|", |lexer| lexer.increase_token())]
        BitOr,
        #[token("&", |lexer| lexer.increase_token())]
        BitAnd,
        #[token("^", |lexer| lexer.increase_token())]
        BitXor,
        #[token("<<", |lexer| lexer.increase_token())]
        Shl,
        #[token(">>", |lexer| lexer.increase_token())]
        Sar,
        #[token(">>>", |lexer| lexer.increase_token())]
        Shr,
        #[token("+", |lexer| lexer.increase_token())]
        Add,
        #[token("-", |lexer| lexer.increase_token())]
        Sub,
        #[token("*", |lexer| lexer.increase_token())]
        Mul,
        #[token("/", |lexer| lexer.increase_token())]
        Div,
        #[token("%", |lexer| lexer.increase_token())]
        Mod,
        #[token("**", |lexer| lexer.increase_token())]
        Exp,
        #[token("==", |lexer| lexer.increase_token())]
        Eq,
        #[token("!=", |lexer| lexer.increase_token())]
        Ne,
        #[token("<", |lexer| lexer.increase_token())]
        Lt,
        #[token("<=", |lexer| lexer.increase_token())]
        Le,
        #[token(">", |lexer| lexer.increase_token())]
        Gt,
        #[token(">=", |lexer| lexer.increase_token())]
        Ge,
        #[token("!", |lexer| lexer.increase_token())]
        Not,
        #[token("~", |lexer| lexer.increase_token())]
        BitNot,
        #[token("++", |lexer| lexer.increase_token())]
        Inc,
        #[token("--", |lexer| lexer.increase_token())]
        Dec,

        #[token("bytes1", |lexer| increase_token_then(lexer, FixedBytes::BYTES1))]
        #[token("bytes2", |lexer| increase_token_then(lexer, FixedBytes::BYTES2))]
        #[token("bytes3", |lexer| increase_token_then(lexer, FixedBytes::BYTES3))]
        #[token("bytes4", |lexer| increase_token_then(lexer, FixedBytes::BYTES4))]
        #[token("bytes5", |lexer| increase_token_then(lexer, FixedBytes::BYTES5))]
        #[token("bytes6", |lexer| increase_token_then(lexer, FixedBytes::BYTES6))]
        #[token("bytes7", |lexer| increase_token_then(lexer, FixedBytes::BYTES7))]
        #[token("bytes8", |lexer| increase_token_then(lexer, FixedBytes::BYTES8))]
        #[token("bytes9", |lexer| increase_token_then(lexer, FixedBytes::BYTES9))]
        #[token("bytes10", |lexer| increase_token_then(lexer, FixedBytes::BYTES10))]
        #[token("bytes11", |lexer| increase_token_then(lexer, FixedBytes::BYTES11))]
        #[token("bytes12", |lexer| increase_token_then(lexer, FixedBytes::BYTES12))]
        #[token("bytes13", |lexer| increase_token_then(lexer, FixedBytes::BYTES13))]
        #[token("bytes14", |lexer| increase_token_then(lexer, FixedBytes::BYTES14))]
        #[token("bytes15", |lexer| increase_token_then(lexer, FixedBytes::BYTES15))]
        #[token("bytes16", |lexer| increase_token_then(lexer, FixedBytes::BYTES16))]
        #[token("bytes17", |lexer| increase_token_then(lexer, FixedBytes::BYTES17))]
        #[token("bytes18", |lexer| increase_token_then(lexer, FixedBytes::BYTES18))]
        #[token("bytes19", |lexer| increase_token_then(lexer, FixedBytes::BYTES19))]
        #[token("bytes20", |lexer| increase_token_then(lexer, FixedBytes::BYTES20))]
        #[token("bytes21", |lexer| increase_token_then(lexer, FixedBytes::BYTES21))]
        #[token("bytes22", |lexer| increase_token_then(lexer, FixedBytes::BYTES22))]
        #[token("bytes23", |lexer| increase_token_then(lexer, FixedBytes::BYTES23))]
        #[token("bytes24", |lexer| increase_token_then(lexer, FixedBytes::BYTES24))]
        #[token("bytes25", |lexer| increase_token_then(lexer, FixedBytes::BYTES25))]
        #[token("bytes26", |lexer| increase_token_then(lexer, FixedBytes::BYTES26))]
        #[token("bytes27", |lexer| increase_token_then(lexer, FixedBytes::BYTES27))]
        #[token("bytes28", |lexer| increase_token_then(lexer, FixedBytes::BYTES28))]
        #[token("bytes29", |lexer| increase_token_then(lexer, FixedBytes::BYTES29))]
        #[token("bytes30", |lexer| increase_token_then(lexer, FixedBytes::BYTES30))]
        #[token("bytes31", |lexer| increase_token_then(lexer, FixedBytes::BYTES31))]
        #[token("bytes32", |lexer| increase_token_then(lexer, FixedBytes::BYTES32))]
        FixedBytes(FixedBytes),

        #[token("wei", |lexer| increase_token_then(lexer, Denomination::Wei))]
        #[token("gwei", |lexer| increase_token_then(lexer, Denomination::Gwei))]
        #[token("ether", |lexer| increase_token_then(lexer, Denomination::Ether))]
        #[token("seconds", |lexer| increase_token_then(lexer, Denomination::Seconds))]
        #[token("minutes", |lexer| increase_token_then(lexer, Denomination::Minutes))]
        #[token("hours", |lexer| increase_token_then(lexer, Denomination::Hours))]
        #[token("days", |lexer| increase_token_then(lexer, Denomination::Days))]
        #[token("weeks", |lexer| increase_token_then(lexer, Denomination::Weeks))]
        #[token("years", |lexer| increase_token_then(lexer, Denomination::Years))]
        Denomination(Denomination),

        #[token("int8", |lexer| increase_token_then(lexer, Int::I8))]
        #[token("int16", |lexer| increase_token_then(lexer, Int::I16))]
        #[token("int24", |lexer| increase_token_then(lexer, Int::I24))]
        #[token("int32", |lexer| increase_token_then(lexer, Int::I32))]
        #[token("int40", |lexer| increase_token_then(lexer, Int::I40))]
        #[token("int48", |lexer| increase_token_then(lexer, Int::I48))]
        #[token("int56", |lexer| increase_token_then(lexer, Int::I56))]
        #[token("int64", |lexer| increase_token_then(lexer, Int::I64))]
        #[token("int72", |lexer| increase_token_then(lexer, Int::I72))]
        #[token("int80", |lexer| increase_token_then(lexer, Int::I80))]
        #[token("int88", |lexer| increase_token_then(lexer, Int::I88))]
        #[token("int96", |lexer| increase_token_then(lexer, Int::I96))]
        #[token("int104", |lexer| increase_token_then(lexer, Int::I104))]
        #[token("int112", |lexer| increase_token_then(lexer, Int::I112))]
        #[token("int120", |lexer| increase_token_then(lexer, Int::I120))]
        #[token("int128", |lexer| increase_token_then(lexer, Int::I128))]
        #[token("int136", |lexer| increase_token_then(lexer, Int::I136))]
        #[token("int144", |lexer| increase_token_then(lexer, Int::I144))]
        #[token("int152", |lexer| increase_token_then(lexer, Int::I152))]
        #[token("int160", |lexer| increase_token_then(lexer, Int::I160))]
        #[token("int168", |lexer| increase_token_then(lexer, Int::I168))]
        #[token("int176", |lexer| increase_token_then(lexer, Int::I176))]
        #[token("int184", |lexer| increase_token_then(lexer, Int::I184))]
        #[token("int192", |lexer| increase_token_then(lexer, Int::I192))]
        #[token("int200", |lexer| increase_token_then(lexer, Int::I200))]
        #[token("int208", |lexer| increase_token_then(lexer, Int::I208))]
        #[token("int216", |lexer| increase_token_then(lexer, Int::I216))]
        #[token("int224", |lexer| increase_token_then(lexer, Int::I224))]
        #[token("int232", |lexer| increase_token_then(lexer, Int::I232))]
        #[token("int240", |lexer| increase_token_then(lexer, Int::I240))]
        #[token("int248", |lexer| increase_token_then(lexer, Int::I248))]
        #[token("int256", |lexer| increase_token_then(lexer, Int::I256))]
        #[token("int", |lexer| increase_token_then(lexer, Int::I256))]
        Int(Int),

        #[token("uint8", |lexer| increase_token_then(lexer, Uint::U8))]
        #[token("uint16", |lexer| increase_token_then(lexer, Uint::U16))]
        #[token("uint24", |lexer| increase_token_then(lexer, Uint::U24))]
        #[token("uint32", |lexer| increase_token_then(lexer, Uint::U32))]
        #[token("uint40", |lexer| increase_token_then(lexer, Uint::U40))]
        #[token("uint48", |lexer| increase_token_then(lexer, Uint::U48))]
        #[token("uint56", |lexer| increase_token_then(lexer, Uint::U56))]
        #[token("uint64", |lexer| increase_token_then(lexer, Uint::U64))]
        #[token("uint72", |lexer| increase_token_then(lexer, Uint::U72))]
        #[token("uint80", |lexer| increase_token_then(lexer, Uint::U80))]
        #[token("uint88", |lexer| increase_token_then(lexer, Uint::U88))]
        #[token("uint96", |lexer| increase_token_then(lexer, Uint::U96))]
        #[token("uint104", |lexer| increase_token_then(lexer, Uint::U104))]
        #[token("uint112", |lexer| increase_token_then(lexer, Uint::U112))]
        #[token("uint120", |lexer| increase_token_then(lexer, Uint::U120))]
        #[token("uint128", |lexer| increase_token_then(lexer, Uint::U128))]
        #[token("uint136", |lexer| increase_token_then(lexer, Uint::U136))]
        #[token("uint144", |lexer| increase_token_then(lexer, Uint::U144))]
        #[token("uint152", |lexer| increase_token_then(lexer, Uint::U152))]
        #[token("uint160", |lexer| increase_token_then(lexer, Uint::U160))]
        #[token("uint168", |lexer| increase_token_then(lexer, Uint::U168))]
        #[token("uint176", |lexer| increase_token_then(lexer, Uint::U176))]
        #[token("uint184", |lexer| increase_token_then(lexer, Uint::U184))]
        #[token("uint192", |lexer| increase_token_then(lexer, Uint::U192))]
        #[token("uint200", |lexer| increase_token_then(lexer, Uint::U200))]
        #[token("uint208", |lexer| increase_token_then(lexer, Uint::U208))]
        #[token("uint216", |lexer| increase_token_then(lexer, Uint::U216))]
        #[token("uint224", |lexer| increase_token_then(lexer, Uint::U224))]
        #[token("uint232", |lexer| increase_token_then(lexer, Uint::U232))]
        #[token("uint240", |lexer| increase_token_then(lexer, Uint::U240))]
        #[token("uint248", |lexer| increase_token_then(lexer, Uint::U248))]
        #[token("uint256", |lexer| increase_token_then(lexer, Uint::U256))]
        #[token("uint", |lexer| increase_token_then(lexer, Uint::U256))]
        Uint(Uint),

        #[token("fixed", |lexer| increase_token_then_with(lexer, |lexer| lexer.slice()))]
        #[regex("fixed[1-9][0-9]*x[1-9][0-9]*", |lexer| increase_token_then_with(lexer, |lexer| lexer.slice()))]
        Fixed($slice),

        #[token("ufixed", |lexer| increase_token_then_with(lexer, |lexer| lexer.slice()))]
        #[regex("ufixed[1-9][0-9]*x[1-9][0-9]*", |lexer| increase_token_then_with(lexer, |lexer| lexer.slice()))]
        UFixed($slice),

        #[regex(r"//[^\r\n]*", |lexer| increase_token_then_with(lexer, |lexer| lexer.slice()))]
        LineComment($slice),

        #[regex(r"/\*([^*]|\*+[^*/])*\*+/", |lexer| increase_token_then_with(lexer, |lexer| lexer.slice()))]
        MultiLineComment($slice),

        // ==================================== Boolean literals ====================================
        #[token("true", |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_true(lexer.slice())))]
        #[token("false", |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_false(lexer.slice())))]
        // ==================================== Empty quoted string literals ====================================
        #[token("\"\"", |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_empty_double_quoted_string(lexer.slice())))]
        #[token("''", |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_empty_single_quoted_string(lexer.slice())))]
        // ==================================== Regular string literals ====================================
        // Double quoted non-empty string literal lexing
        #[regex(r#""(?&double_quoted_chars)""#, |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_double_quoted_regular_string(lexer.slice())))]
        // Error handling branches for double quoted non-empty string literal lexing
        #[regex(r#""(?&double_quoted_chars)"#, unclosed_double_quoted_regular_string_error)]
        #[token("\"", |lexer| {
          match <LitRegularStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(
            DoubleQuotedRegularStrLexer::<tokit::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer),
          )
          .map_err(Errors::from_underlying) {
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
        #[regex(r"'(?&single_quoted_chars)'", |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_single_quoted_regular_string(lexer.slice())))]
        // Error handling branches for single quoted non-empty string literal lexing
        #[regex(r"'(?&single_quoted_chars)", unclosed_single_quoted_regular_string_error)]
        #[token("\'", |lexer| {
          match <LitRegularStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(
            SingleQuotedRegularStrLexer::<tokit::logos::Lexer<'_, _>, $char, StringError, Error>::from_mut(lexer),
          )
          .map_err(Errors::from_underlying) {
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
        // ==================================== Hex string literals ====================================
        // Double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)\"", |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_double_quoted_hex_string(lexer.slice())))]
        // Error handling branches for double quoted hex string literal lexing
        #[regex("hex\"(?&hex_string_content)", unclosed_double_quoted_hex_string_error)]
        #[token("hex\"", |lexer| {
          match <LitHexStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(
            DoubleQuotedHexStrLexer::<tokit::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer),
          )
          .map_err(Errors::from_underlying) {
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
        // Single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)'", |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_single_quoted_hex_string(lexer.slice())))]
        // Error handling branches for single quoted hex string literal lexing
        #[regex("hex'(?&hex_string_content)", unclosed_single_quoted_hex_string_error)]
        #[token("hex'", |lexer| {
          match <LitHexStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(
            SingleQuotedHexStrLexer::<tokit::logos::Lexer<'_, _>, $char, HexStringError, Error>::from_mut(lexer),
          )
          .map_err(Errors::from_underlying) {
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
        // ==================================== Unicode string literals ====================================
        // Double quoted unicode string literal lexing
        #[regex(r#"unicode"(?&unicode_double_quoted_chars)""#, |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_double_quoted_unicode_string(lexer.slice())))]
        // Error handling branches for double quoted unicode string literal lexing
        #[token("unicode\"", |lexer| {
            match <LitUnicodeStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(
              DoubleQuotedUnicodeStrLexer::<tokit::logos::Lexer<'_, _>, $char, UnicodeStringError, Error>::from_mut(lexer),
            )
            .map_err(Errors::from_underlying) {
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
        // Single quoted unicode string literal lexing
        #[regex("unicode'(?&unicode_single_quoted_chars)'", |lexer| increase_token_then_with(lexer, |lexer| Lit::lit_single_quoted_unicode_string(lexer.slice())))]
        // Error handling branches for single quoted unicode string literal lexing
        #[token("unicode\'", |lexer| {
            match <LitUnicodeStr<_> as Lexable<_, UnderlyingErrorContainer>>::lex(
              SingleQuotedUnicodeStrLexer::<tokit::logos::Lexer<'_, _>, $char, UnicodeStringError, Error>::from_mut(lexer),
            )
            .map_err(Errors::from_underlying) {
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
        // ==================================== Number literals ====================================
        #[regex("0x(?&hex_digits)", |lexer| {
          match handlers::$handlers::handle_hexadecimal_suffix(lexer) {
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
        }, priority = 7)]
        #[regex("0x[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[regex("0X[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[regex("0[xX]{2,}[0-9a-fA-F_]+[g-zG-Z$]?[0-9a-zA-Z_$]*", malformed_hex_literal_error)]
        #[token("0x", |lexer| {
          match handlers::$handlers::handle_hexadecimal_prefix_with_invalid_following(lexer) {
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

        #[regex(r"(?&dec_int)(?:\.(?&dec_int))?(?&dec_exp)?", |lexer| {
          match handlers::$handlers::handle_decimal_suffix(lexer) {
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
        #[regex(r"0(?&dec_int)(?:\.(?&dec_int))?(?&dec_exp)?", |lexer| {
          match handlers::$handlers::handle_leading_zero_and_suffix(lexer) {
            Ok(lit) => {
              lexer.increase_token_and_check().map_err(|e| Errors::from(Error::State(e)))?;
              Ok(lit)
            },
            Err(e) => {
              match lexer.increase_token_and_check() {
                Ok(_) => Err(e),
                Err(state_err) => {
                  let mut errs = e;
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
                Ok(_) => Err(e),
                Err(state_err) => {
                  let mut errs = e;
                  errs.push(Error::State(state_err));
                  Err(errs)
                }
              }
            },
          }
        })]
        Lit(Lit<$slice>),

        #[regex("[a-zA-Z$_][a-zA-Z0-9$_]*", |lexer| increase_token_then_with(lexer, |lexer| lexer.slice()))]
        Identifier($slice),
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
            Token::Abstract => Self::Abstract,
            Token::Address => Self::Address,
            Token::Anonymous => Self::Anonymous,
            Token::As => Self::As,
            Token::Assembly => Self::Assembly,
            Token::Bool => Self::Bool,
            Token::Break => Self::Break,
            Token::Bytes => Self::Bytes,
            Token::Calldata => Self::Calldata,
            Token::Catch => Self::Catch,
            Token::Constant => Self::Constant,
            Token::Constructor => Self::Constructor,
            Token::Continue => Self::Continue,
            Token::Contract => Self::Contract,
            Token::Delete => Self::Delete,
            Token::Do => Self::Do,
            Token::Else => Self::Else,
            Token::Emit => Self::Emit,
            Token::Enum => Self::Enum,
            Token::Event => Self::Event,
            Token::External => Self::External,
            Token::Fallback => Self::Fallback,
            Token::For => Self::For,
            Token::Function => Self::Function,
            Token::If => Self::If,
            Token::Immutable => Self::Immutable,
            Token::Import => Self::Import,
            Token::Indexed => Self::Indexed,
            Token::Interface => Self::Interface,
            Token::Internal => Self::Internal,
            Token::Is => Self::Is,
            Token::Library => Self::Library,
            Token::Mapping => Self::Mapping,
            Token::Memory => Self::Memory,
            Token::Modifier => Self::Modifier,
            Token::New => Self::New,
            Token::Override => Self::Override,
            Token::Payable => Self::Payable,
            Token::Private => Self::Private,
            Token::Public => Self::Public,
            Token::Pure => Self::Pure,
            Token::Pragma => Self::Pragma,
            Token::Receive => Self::Receive,
            Token::Return => Self::Return,
            Token::Returns => Self::Returns,
            Token::Storage => Self::Storage,
            Token::String => Self::String,
            Token::Struct => Self::Struct,
            Token::Try => Self::Try,
            Token::Type => Self::Type,
            Token::Unchecked => Self::Unchecked,
            Token::Using => Self::Using,
            Token::View => Self::View,
            Token::Virtual => Self::Virtual,
            Token::While => Self::While,

            Token::LParen => Self::LParen,
            Token::RParen => Self::RParen,
            Token::LBracket => Self::LBracket,
            Token::RBracket => Self::RBracket,
            Token::LBrace => Self::LBrace,
            Token::RBrace => Self::RBrace,
            Token::Colon => Self::Colon,
            Token::Semicolon => Self::Semicolon,
            Token::Dot => Self::Dot,
            Token::Question => Self::Question,
            Token::FatArrow => Self::FatArrow,
            Token::ThinArrow => Self::ThinArrow,
            Token::Assign => Self::Assign,

            Token::BitOrAssign => Self::BitOrAssign,
            Token::BitAndAssign => Self::BitAndAssign,
            Token::BitXorAssign => Self::BitXorAssign,
            Token::ShlAssign => Self::ShlAssign,
            Token::SarAssign => Self::SarAssign,
            Token::ShrAssign => Self::ShrAssign,
            Token::AddAssign => Self::AddAssign,
            Token::SubAssign => Self::SubAssign,
            Token::MulAssign => Self::MulAssign,
            Token::DivAssign => Self::DivAssign,
            Token::ModAssign => Self::ModAssign,

            Token::Comma => Self::Comma,

            Token::Or => Self::Or,
            Token::And => Self::And,
            Token::BitOr => Self::BitOr,
            Token::BitAnd => Self::BitAnd,
            Token::BitXor => Self::BitXor,
            Token::Shl => Self::Shl,
            Token::Sar => Self::Sar,
            Token::Shr => Self::Shr,

            Token::Add => Self::Add,
            Token::Sub => Self::Sub,
            Token::Mul => Self::Mul,
            Token::Div => Self::Div,
            Token::Mod => Self::Mod,
            Token::Exp => Self::Exp,

            Token::Eq => Self::Eq,
            Token::Ne => Self::Ne,
            Token::Lt => Self::Lt,
            Token::Le => Self::Le,
            Token::Gt => Self::Gt,
            Token::Ge => Self::Ge,

            Token::Not => Self::Not,
            Token::BitNot => Self::BitNot,
            Token::Inc => Self::Inc,
            Token::Dec => Self::Dec,

            Token::FixedBytes(val) => Self::FixedBytes(val),
            Token::Denomination(val) => Self::Denomination(val),
            Token::Int(val) => Self::Int(val),
            Token::Uint(val) => Self::Uint(val),
            Token::Fixed(val) => Self::Fixed(val),
            Token::UFixed(val) => Self::UFixed(val),
            Token::LineComment(val) => Self::LineComment(val),
            Token::MultiLineComment(val) => Self::MultiLineComment(val),
            Token::Lit(val) => Self::Lit(val),
            Token::Identifier(val) => Self::Identifier(val),
          }
        }
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_double_quoted_regular_string_error<'b $(: $lt)?, $($lt: 'b)?>(
        lexer: &mut Lexer<'b, Token $(<$lt>)?>,
      ) -> Result<Lit<$slice>, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          Error::String(crate::error::StringError::unclosed_double_quote(l.span().into()))
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_single_quoted_regular_string_error<'b $(: $lt)?, $($lt: 'b)?>(
        lexer: &mut Lexer<'b, Token $(<$lt>)?>,
      ) -> Result<Lit<$slice>, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          Error::String(crate::error::StringError::unclosed_single_quote(l.span().into()))
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_double_quoted_hex_string_error<'b $(: $lt)?, $($lt: 'b)?>(
        lexer: &mut Lexer<'b, Token $(<$lt>)?>,
      ) -> Result<Lit<$slice>, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          Error::HexString(crate::error::HexStringError::unclosed_double_quote(l.span().into()))
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn unclosed_single_quoted_hex_string_error<'b $(: $lt)?, $($lt: 'b)?>(
        lexer: &mut Lexer<'b, Token $(<$lt>)?>,
      ) -> Result<Lit<$slice>, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          Error::HexString(crate::error::HexStringError::unclosed_single_quote(l.span().into()))
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn malformed_hex_literal_error<'b $(: $lt)?, $($lt: 'b)?>(
        lexer: &mut Lexer<'b, Token $(<$lt>)?>,
      ) -> Result<Lit<$slice>, Errors> {
        Err(increase_token_on_err(lexer, |l| {
          Error::from(crate::error::sol::HexadecimalError::malformed(l.span().into()))
        }))
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_on_err<'b $(: $lt)?, $($lt: 'b,)?>(
        lexer: &mut Lexer<'b, Token $(<$lt>)?>,
        f: impl FnOnce(&mut Lexer<'b, Token $(<$lt>)?>) -> Error,
      ) -> Errors {
        lexer.increase_token();
        f(lexer).into()
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_then<'b $(: $lt)?, $($lt: 'b,)? O>(
        lexer: &mut Lexer<'b, Token $(<$lt>)?>,
        output: O,
      ) -> O {
        lexer.increase_token();
        output
      }

      #[cfg_attr(not(tarpaulin), inline(always))]
      fn increase_token_then_with<'b $(: $lt)?, $($lt: 'b,)? O>(
        lexer: &mut Lexer<'b, Token $(<$lt>)?>,
        output: impl FnOnce(&mut Lexer<'b, Token $(<$lt>)?>) -> O,
      ) -> O {
        lexer.increase_token();
        output(lexer)
      }
    }
  }
}

pub(super) use token;
