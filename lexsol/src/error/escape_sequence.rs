use derive_more::{From, IsVariant, TryUnwrap, Unwrap};
use logosky::{
  error::{FixedUnicodeEscapeError, HexEscapeError},
  utils::{CharLen, EscapedLexeme, Lexeme, human_display::DisplayHuman},
};

/// Escape sequence error
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum EscapeSequenceError<Char = char> {
  /// Hexadecimal escape sequence error.
  Hexadecimal(HexEscapeError<Char>),
  /// Unicode escape sequence error.
  Unicode(FixedUnicodeEscapeError<Char>),
  /// Unsupported escape character.
  ///
  /// Returned when an escape character is not recognized.
  /// For example, `\o` where 'o' is not a valid escape character.
  Unsupported(EscapedLexeme<Char>),
}

impl<Char> EscapeSequenceError<Char> {
  /// Create an unsupported escape sequence error with the given escaped lexeme.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unsupported(escaped_lexeme: EscapedLexeme<Char>) -> Self {
    Self::Unsupported(escaped_lexeme)
  }
}

impl<Char> core::fmt::Display for EscapeSequenceError<Char>
where
  Char: DisplayHuman + CharLen,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Hexadecimal(err) => err.fmt(f),
      Self::Unicode(err) => err.fmt(f),
      Self::Unsupported(escaped_lexeme) => match escaped_lexeme.lexeme_ref() {
        Lexeme::Char(pc) => {
          write!(
            f,
            "unsupported escape character '{}' at {}",
            pc.char_ref().display(),
            pc.position()
          )
        }
        Lexeme::Range(span) => {
          write!(f, "unsupported escape sequence at {}", span,)
        }
      },
    }
  }
}

impl<Char> core::error::Error for EscapeSequenceError<Char> where
  Char: DisplayHuman + core::fmt::Debug + CharLen
{
}
