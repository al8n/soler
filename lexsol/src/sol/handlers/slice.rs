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
pub(crate) fn default_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> Error<u8, Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  crate::handlers::slice::default_error(lexer)
}

#[inline]
fn leading_zero_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> Error<u8, Extras>
where
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
  T: Logos<'a, Source = S>,
{
  let slice = lexer.slice();
  let mut zeros = 0;
  let mut u8s = slice.as_ref().iter().copied();

  let zero_start_at = match u8s.next() {
    Some(0) => {
      zeros += 1;
      lexer.span().start
    }
    Some(_) | None => unreachable!("regex should ensure the first u8 is '0'"),
  };

  for ch in u8s {
    if ch == 0 {
      zeros += 1;
    } else {
      break;
    }
  }

  let l = if zeros == 1 {
    let pc = PositionedChar::with_position(0, zero_start_at);
    Lexeme::Char(pc)
  } else {
    Lexeme::Range(Span::new(zero_start_at, zero_start_at + zeros))
  };

  DecimalError::LeadingZeros(l).into()
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
fn handle_suffix<'a, S, T, Extras, E>(
  lexer: &mut Lexer<'a, T>,
  from_slice: impl FnOnce(S::Slice<'a>) -> Lit<S::Slice<'a>>,
  unexpected_suffix: impl FnOnce(Lexeme<u8>) -> E,
) -> Result<Lit<S::Slice<'a>>, Error<u8, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
  Error<u8, Extras>: From<E>,
{
  crate::handlers::slice::handle_number_suffix::<_, _, E>(lexer, unexpected_suffix)
    .map(|_| from_slice(lexer.slice()))
    .map_err(Into::into)
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_leading_zero_and_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<u8, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let span = lexer.span();
  let err = leading_zero_error(lexer);
  let mut errs: Errors<u8, Extras> = Errors::default();
  errs.push(err);

  match crate::handlers::slice::handle_number_suffix::<_, _, DecimalError<u8>>(lexer, |l| {
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
pub(crate) fn handle_decimal_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<u8, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let span = lexer.span();
  handle_suffix::<_, _, Extras, DecimalError<u8>>(lexer, Lit::lit_decimal, |l| {
    DecimalError::unexpected_suffix(span.into(), l)
  })
  .map_err(Into::into)
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_malformed_decimal_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<u8, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let span: Span = lexer.span().into();
  let malformed = Error::from(DecimalError::malformed(span));
  match handle_suffix::<_, _, Extras, DecimalError<u8>>(lexer, Lit::lit_decimal, |l| {
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
) -> Result<Lit<S::Slice<'a>>, Errors<u8, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let span = lexer.span();
  handle_suffix::<_, _, Extras, HexadecimalError<u8>>(lexer, Lit::lit_hexadecimal, |l| {
    HexadecimalError::unexpected_suffix(span.into(), l)
  })
  .map_err(Into::into)
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
pub(crate) fn handle_hexadecimal_prefix_with_invalid_following<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<Lit<S::Slice<'a>>, Errors<u8, Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<[u8]>,
{
  let remainder = lexer.remainder();
  let remainder_slice = remainder.as_ref();
  if remainder_slice.is_empty() {
    let err = HexadecimalError::<u8>::incomplete(lexer.span().into());
    return Err(Error::from(err).into());
  }

  let mut end = 0;
  for (idx, ch) in remainder_slice.iter().enumerate() {
    match *ch {
      b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => {
        end = idx + 1;
        continue;
      }
      _ => break,
    }
  }

  lexer.bump(end);

  Err(Error::from(HexadecimalError::malformed(lexer.span().into())).into())
}
