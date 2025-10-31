use logosky::{
  Logos, Source,
  logos::Lexer,
  utils::{Lexeme, PositionedChar, Span},
};

use crate::{
  error::sol::{DecimalError, Error, Errors, HexadecimalError},
  sol::Lit,
};

#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn default_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> Error<char, Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  crate::handlers::str::default_error(lexer)
}
