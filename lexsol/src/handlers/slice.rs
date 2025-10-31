use logosky::{
  Logos, Source,
  logos::Lexer,
  utils::{PositionedChar, UnexpectedEot, UnknownLexeme},
};

use crate::Lxr;

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_error<'a, S, T, L, E>(lexer: &mut Lexer<'a, T>) -> E
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
  E: From<UnknownLexeme<u8, L>> + From<UnexpectedEot>,
  L: Lxr,
{
  let span = lexer.span();

  let slice = lexer.slice();
  let slice_ref = slice.as_ref();

  let mut chars = slice_ref.iter().enumerate();

  match chars.next() {
    None => E::from(UnexpectedEot::EOT),
    Some((idx, first)) => {
      match first {
        b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => {}
        _ => {
          return UnknownLexeme::from_char(
            PositionedChar::with_position(*first, lexer.span().start + idx),
            L::INIT,
          )
          .into();
        }
      }

      match chars.next() {
        Some((_, _)) => UnknownLexeme::from_span(span, L::INIT).into(),
        None => {
          let start = span.start;
          let mut cur = 0;
          let remaining = lexer.remainder();
          let remaining_ref = remaining.as_ref();
          let rem_chars = remaining_ref.iter().enumerate();

          for (r_idx, ch) in rem_chars {
            match ch {
              b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => {
                cur = r_idx + 1;
                continue;
              }
              _ => break,
            }
          }

          if cur == 0 {
            return UnknownLexeme::from_char(
              PositionedChar::with_position(*first, span.end),
              L::INIT,
            )
            .into();
          }

          lexer.bump(cur);

          UnknownLexeme::from_span(start..span.end + cur, L::INIT).into()
        }
      }
    }
  }
}
