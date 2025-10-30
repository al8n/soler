use logosky::{
  Lexable, Logos, Source,
  logos::Lexer,
  utils::{Lexeme, LineTerminator, PositionedChar, UnexpectedLexeme},
};

use crate::{
  types::{LitRegularStr, LitStrDelimiterKind},
  utils::sealed::DoubleQuotedRegularStrLexer,
};

type StringError = crate::error::StringError<u8>;

#[derive(Logos)]
#[logos(
  crate = logosky::logos,
  source = [u8],
)]
#[logos(subpattern double_quoted_printable = "[\u{0020}-\u{0021}\u{0023}-\u{005B}\u{005D}-\u{007E}]")]
#[logos(subpattern double_quoted_char = "(?&double_quoted_printable)")]
#[logos(subpattern double_quoted_chars = r#"(?&double_quoted_char)+"#)]
enum StringToken {
  #[token("\"")]
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

  #[regex(r#"[^"\\\u{0020}-\u{0021}\u{0023}-\u{005B}\u{005D}-\u{007E}]"#)]
  UnsupportedCharacter,

  #[regex(r#"[^"\\\u{0020}-\u{0021}\u{0023}-\u{005B}\u{005D}-\u{007E}]{2,}"#)]
  UnsupportedCharacters,

  #[regex("(?&double_quoted_chars)")]
  Continue,
}

impl StringToken {
  #[inline]
  pub(crate) fn lex_regular<'a, S, T, Error, Container>(
    lexer: &mut DoubleQuotedRegularStrLexer<Lexer<'a, T>, u8, StringError, Error>,
  ) -> Result<LitRegularStr<S::Slice<'a>>, Container>
  where
    T: Logos<'a, Source = S>,
    S: Source + ?Sized + 'a,
    S::Slice<'a>: AsRef<[u8]>,
    Error: From<StringError>,
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
          errs
            .push(StringError::other("unknown double quoted non-empty string lexing error").into());
          return Err(errs);
        }
        Ok(StringToken::Quote) => {
          lexer.bump(string_lexer.span().end);
          if !errs.is_empty() {
            return Err(errs);
          }

          let src = lexer.slice();
          return Ok(LitRegularStr::double(src));
        }
        Ok(StringToken::NewLine) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            StringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Char(PositionedChar::with_position(b'\n', pos)),
              LineTerminator::NewLine,
            ))
            .into(),
          );
        }
        Ok(StringToken::CarriageReturn) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            StringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Char(PositionedChar::with_position(b'\r', pos)),
              LineTerminator::CarriageReturn,
            ))
            .into(),
          );
        }
        Ok(StringToken::CarriageReturnNewLine) => {
          let pos = lexer_span.end + string_lexer.span().start;
          errs.push(
            StringError::unexpected_line_terminator(UnexpectedLexeme::new(
              Lexeme::Span((pos..pos + 2).into()),
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
          let pos = lexer_span.end + string_lexer.span().start + 1;
          errs.push(
            StringError::unsupported_escape_character(PositionedChar::with_position(
              string_lexer.slice().last().copied().unwrap(),
              pos,
            ))
            .into(),
          );
        }
        Ok(StringToken::IncompleteHexEscapeSequence) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          let end = lexer_span.end + span.end;
          errs.push(StringError::incomplete_hex_escape_sequence((pos..end).into()).into());
        }
        Ok(StringToken::IncompleteUnicodeEscapeSequence) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          let end = lexer_span.end + span.end;
          errs.push(StringError::incomplete_unicode_escape_sequence((pos..end).into()).into());
        }
        Ok(StringToken::UnsupportedCharacters) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          let end = lexer_span.end + span.end;
          errs.push(StringError::unsupported_characters((pos..end).into()).into());
        }
        Ok(StringToken::UnsupportedCharacter) => {
          let span = string_lexer.span();
          let pos = lexer_span.end + span.start;
          errs.push(
            StringError::unsupported_character(PositionedChar::with_position(
              string_lexer.slice().iter().next().copied().unwrap(),
              pos,
            ))
            .into(),
          );
        }
      }
    }

    lexer.bump(string_lexer.span().end);
    errs.push(StringError::unclosed(lexer.span().into(), LitStrDelimiterKind::Double).into());
    Err(errs)
  }
}

impl<'a, S, T, Error, Container>
  Lexable<&mut DoubleQuotedRegularStrLexer<Lexer<'a, T>, u8, StringError, Error>, Container>
  for LitRegularStr<S::Slice<'a>>
where
  T: Logos<'a, Source = S>,
  S: Source + ?Sized + 'a,
  S::Slice<'a>: AsRef<[u8]>,
  Container: Default + crate::utils::Container<Error>,
  Error: From<StringError>,
{
  #[inline]
  fn lex(
    lexer: &mut DoubleQuotedRegularStrLexer<Lexer<'a, T>, u8, StringError, Error>,
  ) -> Result<Self, Container>
  where
    Self: Sized,
  {
    StringToken::lex_regular(lexer)
  }
}
