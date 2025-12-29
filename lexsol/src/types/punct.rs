macro_rules! punctuator {
  ($(($name:ident::$trait:ident::$fn:ident, $syntax_tree_display: literal, $punct:literal)),+$(,)?) => {
    paste::paste! {
      $(
        tokit::punctuator! {
          ($name, $syntax_tree_display, $punct)
        }

        // paste::paste! {
        //   pub(crate) trait [<$name TokenKind>] {
        //     const KIND: Self;
        //   }

        //   impl<'a, I, T, Error> tokit::chumsky::Parseable<'a, I, T, Error> for $name
        //   where
        //     T: tokit::$trait<'a>,
        //     T::Kind: [<$name TokenKind>],
        //     Error: ::core::convert::From<tokit::error::UnexpectedToken<'a, T, T::Kind>> + ::core::convert::From<<T::Logos as tokit::Logos<'a>>::Error>,
        //   {
        //     fn parser<E>() -> impl tokit::chumsky::Parser<'a, I, Self, E> + ::core::clone::Clone
        //     where
        //       Self: ::core::marker::Sized + 'a,
        //       I: tokit::chumsky::LogoStream<'a, T, Slice = <<T::Logos as tokit::Logos<'a>>::Source as tokit::Source>::Slice<'a>>,
        //       T: tokit::Token<'a>,
        //       Error: 'a,
        //       E: tokit::chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
        //     {
        //       use tokit::chumsky::prelude::*;

        //       any().try_map(|t: tokit::Lexed<'_, T>, span| {
        //         match t {
        //           tokit::Lexed::Token(t) => {
        //             if <T as tokit::$trait>::$fn(&t.data) {
        //               ::core::result::Result::Ok(<$name>::new(span))
        //             } else {
        //               let e = tokit::error::UnexpectedToken::expected_one_with_found(span, t.data, <T::Kind as [<$name TokenKind>]>::KIND);
        //               ::core::result::Result::Err(<Error as ::core::convert::From<_>>::from(e))
        //             }
        //           },
        //           tokit::Lexed::Error(e) => {
        //             ::core::result::Result::Err(<Error as ::core::convert::From<_>>::from(e))
        //           },
        //         }
        //       })
        //     }
        //   }
        // }


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
