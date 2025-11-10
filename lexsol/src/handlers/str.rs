use logosky::{
  Logos, Source,
  error::{UnexpectedEot, UnknownLexeme},
  logos::Lexer,
  utils::{Lexeme, PositionedChar, Span},
};

use crate::Lxr;

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_error<'a, S, T, L, E>(lexer: &mut Lexer<'a, T>) -> E
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: From<UnknownLexeme<char, L>> + From<UnexpectedEot>,
  L: Lxr,
{
  let span = lexer.span();

  let slice = lexer.slice();
  let slice_str = slice.as_ref();

  let mut chars = slice_str.char_indices();

  match chars.next() {
    None => E::from(UnexpectedEot::eot(span.into())),
    Some((idx, first)) => {
      match first {
        '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '$' => {}
        _ => {
          return UnknownLexeme::from_char(lexer.span().start + idx, first, L::INIT).into();
        }
      }

      match chars.next() {
        Some((_, _)) => UnknownLexeme::from_range(span, L::INIT).into(),
        None => {
          let start = span.start;
          let mut cur = 0;
          let remaining = lexer.remainder();
          let remaining_str = remaining.as_ref();
          let rem_chars = remaining_str.char_indices();

          for (r_idx, ch) in rem_chars {
            match ch {
              '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '$' => {
                cur = r_idx + 1;
                continue;
              }
              _ => break,
            }
          }

          if cur == 0 {
            return UnknownLexeme::from_char(span.end, first, L::INIT).into();
          }

          lexer.bump(cur);

          UnknownLexeme::from_range(start..span.end + cur, L::INIT).into()
        }
      }
    }
  }
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_number_suffix<'a, S, T, E>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<(), E>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();

  let mut end = 0;
  let mut first = None;
  for (idx, ch) in remainder.as_ref().char_indices() {
    match ch {
      '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '$' => {
        end = idx + 1;
        if idx == 0 {
          first = Some(ch);
        }
        continue;
      }
      _ => break,
    }
  }

  let l = match end {
    0 => return Ok(()),
    1 => {
      let ch = first.unwrap();
      let pc = PositionedChar::with_position(ch, lexer.span().end);
      unexpected_suffix(Lexeme::Char(pc))
    }
    len => {
      let span_start = lexer.span().end;
      let span_end = span_start + len;
      unexpected_suffix(Lexeme::Range(Span::new(span_start, span_end)))
    }
  };

  lexer.bump(end);
  Err(l)
}
