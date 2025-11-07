use logosky::{
  Lexable, Logos, Source,
  error::UnexpectedLexeme,
  logos::Lexer,
  utils::{Lexeme, PositionedChar, knowledge::LineTerminator},
};

use crate::{
  error::sol::UnicodeStringError, sol::LitUnicodeStr, types::LitStrDelimiterKind,
  utils::sealed::SingleQuotedUnicodeStrLexer,
};

#[derive(Logos, Copy, Clone)]
#[logos(
  crate = logosky::logos,
  extras = Option<StringToken>,
)]
#[logos(subpattern character = r"[^'\r\n\\]")]
enum StringToken {
  #[token("'")]
  Quote,

  #[token("\r\n", priority = 5)]
  CarriageReturnNewLine,

  #[token("\n", priority = 3)]
  NewLine,

  #[token("\r", priority = 3)]
  CarriageReturn,

  #[regex(r#"\\['"\\nrt\n\r]"#)]
  EscapedCharacter,

  #[regex(r"\\u[0-9a-fA-F]{4}")]
  EscapedUnicode,

  #[regex(r"\\x[0-9a-fA-F]{2}")]
  EscapedHex,

  #[regex(r#"\\[^'"\\nrt\n\rxu]"#)]
  UnsupportedEscapeCharacter,

  #[regex(r#"\\x([0-9a-fA-F]{0,1})"#)]
  IncompleteHexEscapeSequence,

  #[regex(r#"\\u([0-9a-fA-F]{0,3})"#)]
  IncompleteUnicodeEscapeSequence,

  #[regex("(?&character)*")]
  Continue,
}

impl StringToken {
  #[inline]
  pub(crate) fn lex_unicode<'a, S, T, Error, Container>(
    lexer: &mut SingleQuotedUnicodeStrLexer<Lexer<'a, T>, char, UnicodeStringError, Error>,
  ) -> Result<LitUnicodeStr<S::Slice<'a>>, Container>
  where
    T: Logos<'a, Source = S>,
    S: Source + ?Sized + 'a,
    S::Slice<'a>: AsRef<str>,
    Error: From<UnicodeStringError>,
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
          errs.push(
            UnicodeStringError::other(
              lexer.span().into(),
              "unknown single quoted unicode string lexing error",
            )
            .into(),
          );
          return Err(errs);
        }
        Ok(StringToken::Quote) => {
          lexer.bump(string_lexer.span().end);
          if !errs.is_empty() {
            return Err(errs);
          }

          let src = lexer.slice();
          return Ok(LitUnicodeStr::single(src));
        }
        Ok(StringToken::NewLine) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            UnicodeStringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Char(PositionedChar::with_position('\n', pos)),
              LineTerminator::NewLine,
            ))
            .into(),
          );
        }
        Ok(StringToken::CarriageReturn) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            UnicodeStringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Char(PositionedChar::with_position('\r', pos)),
              LineTerminator::CarriageReturn,
            ))
            .into(),
          );
        }
        Ok(StringToken::CarriageReturnNewLine) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            UnicodeStringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Range((pos..pos + 2).into()),
              LineTerminator::CarriageReturnNewLine,
            ))
            .into(),
          );
        }
        Ok(StringToken::Continue) => {}
        Ok(StringToken::EscapedUnicode) => {}
        Ok(StringToken::EscapedHex) => {}
        Ok(StringToken::EscapedCharacter) => {}
        Ok(StringToken::UnsupportedEscapeCharacter) => {
          let slash_pos = lexer_span.end + string_lexer.span().start;
          let pos = slash_pos + 1;
          errs.push(
            UnicodeStringError::unsupported_escape_character(
              (slash_pos..pos + 1).into(),
              PositionedChar::with_position(string_lexer.slice().chars().last().unwrap(), pos),
            )
            .into(),
          );
        }
        Ok(StringToken::IncompleteHexEscapeSequence) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          let end = lexer_span.end + span.end;
          errs.push(UnicodeStringError::incomplete_hex_escape_sequence((pos..end).into()).into());
        }
        Ok(StringToken::IncompleteUnicodeEscapeSequence) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          let end = lexer_span.end + span.end;
          errs
            .push(UnicodeStringError::incomplete_unicode_escape_sequence((pos..end).into()).into());
        }
      }
    }

    lexer.bump(string_lexer.span().end);
    errs
      .push(UnicodeStringError::unclosed(lexer.span().into(), LitStrDelimiterKind::Single).into());
    Err(errs)
  }
}

impl<'a, S, T, Error, Container>
  Lexable<
    &mut SingleQuotedUnicodeStrLexer<Lexer<'a, T>, char, UnicodeStringError, Error>,
    Container,
  > for LitUnicodeStr<S::Slice<'a>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<str>,
  Container: Default + crate::utils::Container<Error>,
  Error: From<UnicodeStringError>,
{
  #[inline]
  fn lex(
    lexer: &mut SingleQuotedUnicodeStrLexer<Lexer<'a, T>, char, UnicodeStringError, Error>,
  ) -> Result<Self, Container>
  where
    Self: Sized,
  {
    StringToken::lex_unicode(lexer)
  }
}
