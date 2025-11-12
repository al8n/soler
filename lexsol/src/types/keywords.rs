macro_rules! keyword {
  ($(($name:ident, $syntax_tree_display: literal, $keyword:literal)),+$(,)?) => {
    paste::paste! {
      $(
        logosky::keyword! {
          ($name, $syntax_tree_display, $keyword)
        }

        // paste::paste! {
        //   pub(crate) trait [<$name KeywordTokenKind>] {
        //     const KIND: Self;
        //   }

        //   impl<'a, I, T, Error> logosky::chumsky::Parseable<'a, I, T, Error> for $name
        //   where
        //     T: logosky::KeywordToken<'a>,
        //     T::Kind: [<$name KeywordTokenKind>],
        //     Error: ::core::convert::From<logosky::error::UnexpectedToken<'a, T, T::Kind>> + ::core::convert::From<<T::Logos as logosky::Logos<'a>>::Error>,
        //   {
        //     fn parser<E>() -> impl logosky::chumsky::Parser<'a, I, Self, E> + ::core::clone::Clone
        //     where
        //       Self: ::core::marker::Sized + 'a,
        //       I: logosky::chumsky::LogoStream<'a, T, Slice = <<T::Logos as logosky::Logos<'a>>::Source as logosky::Source>::Slice<'a>>,
        //       T: logosky::Token<'a>,
        //       Error: 'a,
        //       E: logosky::chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a,
        //     {
        //       use logosky::chumsky::prelude::*;

        //       any().try_map(|t: logosky::Lexed<'_, T>, span| {
        //         match t {
        //           logosky::Lexed::Token(t) => {
        //             if <T as logosky::KeywordToken<'_>>::matches_keyword(&t.data, $keyword) {
        //               ::core::result::Result::Ok(<$name>::new(span))
        //             } else {
        //               let e = logosky::error::UnexpectedToken::expected_one_with_found(span, t.data, <T::Kind as [<$name KeywordTokenKind>]>::KIND);
        //               ::core::result::Result::Err(<Error as ::core::convert::From<_>>::from(e))
        //             }
        //           },
        //           logosky::Lexed::Error(e) => {
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

keyword! {
  (Default, "default_KW", "default"),
  (Function, "function_KW", "function"),
  (For, "for_KW", "for"),
  (If, "if_KW", "if"),
  (Let, "let_KW", "let"),
  (Leave, "leave_KW", "leave"),
  (Continue, "continue_KW", "continue"),
  (Break, "break_KW", "break"),
  (Switch, "switch_KW", "switch"),
  (Case, "case_KW", "case"),
}
