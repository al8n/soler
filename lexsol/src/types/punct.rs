macro_rules! punctuator {
  ($(($name:ident::$trait:ident::$fn:ident, $syntax_tree_display: literal, $punct:literal)),+$(,)?) => {
    paste::paste! {
      $(
        logosky::punctuator! {
          ($name, $syntax_tree_display, $punct)
        }

        paste::paste! {
          pub(crate) trait [<$name TokenKind>] {
            const KIND: Self;
          }

          impl<'a, I, T, Error> logosky::chumsky::Parseable<'a, I, T, Error> for $name
          where
            T: logosky::$trait<'a>,
            T::Kind: [<$name TokenKind>],
            Error: ::core::convert::From<logosky::error::UnexpectedToken<'a, T, T::Kind>> + ::core::convert::From<<T::Logos as logosky::Logos<'a>>::Error>,
          {
            fn parser<E>() -> impl logosky::chumsky::Parser<'a, I, Self, E> + ::core::clone::Clone
            where
              Self: ::core::marker::Sized + 'a,
              I: logosky::chumsky::LogoStream<'a, T, Slice = <<T::Logos as logosky::Logos<'a>>::Source as logosky::Source>::Slice<'a>>,
              T: logosky::Token<'a>,
              Error: 'a,
              E: logosky::chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
            {
              use logosky::chumsky::prelude::*;

              any().try_map(|t: logosky::Lexed<'_, T>, span| {
                match t {
                  logosky::Lexed::Token(t) => {
                    if <T as logosky::$trait>::$fn(&t.data) {
                      ::core::result::Result::Ok(<$name>::new(span))
                    } else {
                      let e = logosky::error::UnexpectedToken::expected_one_with_found(span, t.data, <T::Kind as [<$name TokenKind>]>::KIND);
                      ::core::result::Result::Err(<Error as ::core::convert::From<_>>::from(e))
                    }
                  },
                  logosky::Lexed::Error(e) => {
                    ::core::result::Result::Err(<Error as ::core::convert::From<_>>::from(e))
                  },
                }
              })
            }
          }
        }
      )*
    }
  }
}

punctuator! {
  (ColonAssign::OperatorToken::is_colon_assign, "COLON_ASSIGN", ":="),
  (ThinArrow::OperatorToken::is_arrow_operator, "THIN_ARROW", "->"),
  (LBrace::PunctuatorToken::is_brace_open, "L_BRACE", "{"),
  (RBrace::PunctuatorToken::is_brace_close, "R_BRACE", "}"),
  (LParen::PunctuatorToken::is_paren_open, "L_PAREN", "("),
  (RParen::PunctuatorToken::is_paren_close, "R_PAREN", ")"),
  (Dot::PunctuatorToken::is_dot, "DOT", "."),
  (Comma::PunctuatorToken::is_comma, "COMMA", ","),
}
