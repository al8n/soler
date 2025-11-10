use logosky::{
  Logos, Source,
  error::ErrorContainer,
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

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
fn handle_suffix<'a, S, T, Extras, E>(
  lexer: &mut Lexer<'a, T>,
  from_slice: impl FnOnce(S::Slice<'a>) -> Lit<S::Slice<'a>>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<Lit<S::Slice<'a>>, Error<char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  Error<char, Extras>: From<E>,
{
  crate::handlers::str::handle_number_suffix::<_, _, E>(lexer, unexpected_suffix)
    .map(|_| from_slice(lexer.slice()))
    .map_err(Into::into)
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_decimal_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span = lexer.span();
  handle_suffix::<_, _, Extras, DecimalError>(lexer, Lit::lit_decimal, |l| {
    DecimalError::unexpected_suffix(span.into(), l)
  })
  .map_err(Into::into)
}

#[inline]
fn leading_zero_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> Error<char, Extras>
where
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  T: Logos<'a, Source = S>,
{
  let slice = lexer.slice();
  let mut zeros = 0;
  let mut chars = slice.as_ref().chars();

  let zero_start_at = match chars.next() {
    Some('0') => {
      zeros += 1;
      lexer.span().start
    }
    Some(_) | None => unreachable!("regex should ensure the first char is '0'"),
  };

  for ch in chars {
    if ch == '0' {
      zeros += 1;
    } else {
      break;
    }
  }

  let l = if zeros == 1 {
    let pc = PositionedChar::with_position('0', zero_start_at);
    Lexeme::Char(pc)
  } else {
    Lexeme::Range(Span::new(zero_start_at, zero_start_at + zeros))
  };

  DecimalError::LeadingZeros(l).into()
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_leading_zero_and_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span = lexer.span();
  let err = leading_zero_error(lexer);
  let mut errs: Errors<char, Extras> = Errors::default();
  errs.push(err);

  match crate::handlers::str::handle_number_suffix::<_, _, DecimalError>(lexer, |l| {
    DecimalError::unexpected_suffix(span.into(), l)
  }) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(e.into());
      Err(errs)
    }
  }
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_malformed_decimal_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span: Span = lexer.span().into();
  let malformed = Error::from(DecimalError::malformed(span));
  match handle_suffix::<_, _, Extras, DecimalError>(lexer, Lit::lit_decimal, |l| {
    DecimalError::unexpected_suffix(span, l)
  }) {
    Ok(_) => Err(malformed.into()),
    Err(e) => Err([e, malformed].into_iter().collect()),
  }
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_hexadecimal_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let span = lexer.span();
  handle_suffix::<_, _, Extras, HexadecimalError>(lexer, Lit::lit_hexadecimal, |l| {
    HexadecimalError::unexpected_suffix(span.into(), l)
  })
  .map_err(Into::into)
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_hexadecimal_prefix_with_invalid_following<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<char, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  if remainder_str.is_empty() {
    let err = HexadecimalError::<char>::incomplete(lexer.span().into());
    return Err(Error::from(err).into());
  }

  let mut end = 0;
  for (idx, ch) in remainder_str.char_indices() {
    match ch {
      '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '$' => {
        end = idx + 1;
        continue;
      }
      _ => break,
    }
  }

  lexer.bump(end);

  Err(Error::from(HexadecimalError::malformed(lexer.span().into())).into())
}
