use logosky::{Logos, Source, logos::Lexer};

use crate::yul::error::Error;

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> Error<char, Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  match lexer.slice().as_ref().chars().next() {
    Some(ch) => Error::unknown_char(ch, lexer.span().start),
    None => Error::unexpected_eoi(lexer.span().end),
  }
}
