use logosky::{Logos, Source, logos::Lexer, utils::{Lexeme, PositionedChar, Span}};

use crate::{error::yul::{DecimalError, Error, Errors, HexadecimalError}, yul::Lit};

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

#[inline]
fn leading_zero_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Error<char, Extras>
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
    Lexeme::Span(Span::new(zero_start_at, zero_start_at + zeros))
  };

  DecimalError::LeadingZeros(l).into()
}

#[allow(clippy::result_large_err)]
#[cfg_attr(not(tarpaulin), inline(always))]
fn handle_suffix_inner<'a, S, T, E>(
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
      unexpected_suffix(Lexeme::Span(Span::new(span_start, span_end)))
    }
  };

  lexer.bump(end);
  Err(l)
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
  let err = leading_zero_error(lexer);
  let mut errs: Errors<char, Extras> = Errors::default();
  errs.push(err);

  match handle_suffix_inner::<_, _, DecimalError>(lexer, DecimalError::UnexpectedSuffix) {
    Ok(_) => Err(errs),
    Err(e) => {
      errs.push(e.into());
      Err(errs)
    }
  }
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
  handle_suffix_inner::<_, _, E>(lexer, unexpected_suffix).map(|_| from_slice(lexer.slice())).map_err(Into::into)
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
  handle_suffix::<_, _, Extras, DecimalError>(lexer, Lit::lit_decimal, DecimalError::UnexpectedSuffix)
    .map_err(Into::into)
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
  handle_suffix::<_, _, Extras, HexadecimalError>(lexer, Lit::lit_hexadecimal, HexadecimalError::UnexpectedSuffix)
    .map_err(Into::into)
}
