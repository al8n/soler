use logosky::{
  Lexable, Logos, Source,
  error::UnexpectedLexeme,
  logos::Lexer,
  utils::{Lexeme, PositionedChar, knowledge::LineTerminator},
};

use crate::{
  error::HexStringError,
  types::{LitHexStr, LitStrDelimiterKind},
  utils::sealed::DoubleQuotedHexStrLexer,
};

#[derive(Logos, Copy, Clone)]
#[logos(
  crate = logosky::logos,
  extras = Option<StringToken>,
)]
#[logos(subpattern hex_digit = "[0-9a-fA-F]")]
#[logos(subpattern hex_digit_pair = "(?&hex_digit){2}")]
enum StringToken {
  #[token("\"")]
  Quote,

  #[token("\r\n", priority = 5)]
  CarriageReturnNewLine,

  #[token("\n", priority = 3)]
  NewLine,

  #[token("\r", priority = 3)]
  CarriageReturn,

  #[regex(r#"[^"_0-9a-fA-F]"#)]
  UnsupportedCharacter,
  #[regex(r#"[^"_0-9a-fA-F]{2,}"#)]
  UnsupportedCharacters,

  #[token("_")]
  Underscore,

  #[regex("[_]{2,}")]
  ConsecutiveUnderscores,

  #[regex("(?&hex_digit)")]
  UnpairedHexDigit,

  #[regex("(?&hex_digit_pair)")]
  Continue,
}

impl StringToken {
  #[inline]
  pub(crate) fn lex_hex<'a, S, T, Error, Container>(
    lexer: &mut DoubleQuotedHexStrLexer<Lexer<'a, T>, char, HexStringError, Error>,
  ) -> Result<LitHexStr<S::Slice<'a>>, Container>
  where
    T: Logos<'a, Source = S>,
    S: Source + ?Sized + 'a,
    S::Slice<'a>: AsRef<str>,
    Error: From<HexStringError>,
    Container: Default + crate::utils::Container<Error>,
  {
    let lexer_span = lexer.span();
    let remainder = lexer.remainder();
    let mut string_lexer = StringToken::lexer(remainder.as_ref());

    let mut errs: Container = Container::default();

    while let Some(string_token) = string_lexer.next() {
      match string_token {
        Err(_) => {
          lexer.bump(string_lexer.span().end);
          errs.push(HexStringError::other("unknown double quoted hex string lexing error").into());
          return Err(errs);
        }
        Ok(StringToken::Underscore) => {
          if string_lexer.extras.is_none() {
            errs.push(
              HexStringError::leading_underscore(PositionedChar::with_position(
                '_',
                lexer_span.end,
              ))
              .into(),
            );
          }
        }
        Ok(StringToken::Continue) => {}
        Ok(StringToken::UnpairedHexDigit) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          errs.push(
            HexStringError::unpaired(PositionedChar::with_position(
              string_lexer.slice().chars().next().unwrap(),
              pos,
            ))
            .into(),
          );
        }
        Ok(StringToken::ConsecutiveUnderscores) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          let end = lexer_span.end + span.end;
          errs.push(HexStringError::consecutive_underscores((pos..end).into()).into());
        }
        Ok(StringToken::Quote) => {
          lexer.bump(string_lexer.span().end);
          if !errs.is_empty() {
            return Err(errs);
          }

          let src = lexer.slice();
          return Ok(LitHexStr::single(src));
        }
        Ok(StringToken::NewLine) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            HexStringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Char(PositionedChar::with_position('\n', pos)),
              LineTerminator::NewLine,
            ))
            .into(),
          );
        }
        Ok(StringToken::CarriageReturn) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            HexStringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Char(PositionedChar::with_position('\r', pos)),
              LineTerminator::CarriageReturn,
            ))
            .into(),
          );
        }
        Ok(StringToken::CarriageReturnNewLine) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            HexStringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Range((pos..pos + 2).into()),
              LineTerminator::CarriageReturnNewLine,
            ))
            .into(),
          );
        }
        Ok(StringToken::UnsupportedCharacters) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          let end = lexer_span.end + span.end;
          errs.push(HexStringError::unsupported_characters((pos..end).into()).into());
        }
        Ok(StringToken::UnsupportedCharacter) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          errs.push(
            HexStringError::unsupported_character(PositionedChar::with_position(
              string_lexer.slice().chars().next().unwrap(),
              pos,
            ))
            .into(),
          );
        }
      }

      // cache the last token
      string_lexer.extras = Some(string_token.unwrap());
    }

    lexer.bump(string_lexer.span().end);
    errs.push(HexStringError::unclosed(lexer.span().into(), LitStrDelimiterKind::Double).into());
    Err(errs)
  }
}

impl<'a, S, T, Error, Container>
  Lexable<&mut DoubleQuotedHexStrLexer<Lexer<'a, T>, char, HexStringError, Error>, Container>
  for LitHexStr<S::Slice<'a>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<str>,
  Container: Default + crate::utils::Container<Error>,
  Error: From<HexStringError>,
{
  #[inline]
  fn lex(
    lexer: &mut DoubleQuotedHexStrLexer<Lexer<'a, T>, char, HexStringError, Error>,
  ) -> Result<Self, Container>
  where
    Self: Sized,
  {
    StringToken::lex_hex(lexer)
  }
}
