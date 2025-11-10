use derive_more::{From, IsVariant, TryUnwrap, Unwrap};

use logosky::{
  error::{Unclosed, UnexpectedLexeme},
  utils::{
    Lexeme, Message, PositionedChar, Span, human_display::DisplayHuman, knowledge::LineTerminator,
  },
};

use crate::types::LitStrDelimiterKind;

/// The hex string literal lexing error
#[derive(Debug, PartialEq, Eq, Clone, From, IsVariant, Unwrap, TryUnwrap)]
#[non_exhaustive]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
pub enum HexStringError<Char = char> {
  /// Unclosed string literal.
  Unclosed(Unclosed<LitStrDelimiterKind>),

  /// Unsupported character or characters found in hex string literal.
  #[from(skip)]
  Unsupported(Lexeme<Char>),

  /// Unexpected line terminator found in string literal.
  UnexpectedLineTerminator(UnexpectedLexeme<Char, LineTerminator>),

  /// Hexadecimal string literal with leading underscore or underscores.
  ///
  /// e.g. `hex"_fe"` or `hex'__fe'`, which is invalid.
  #[from(skip)]
  LeadingUnderscore(Lexeme<Char>),

  /// Consecutive underscores are not allowed in hexadecimal string literals.
  ///
  /// e.g. `hex"fe__fe"` or `hex'fe__fe'`, which is invalid.
  #[from(skip)]
  ConsecutiveUnderscores(Span),

  /// Unpaired hex found in hexadecimal string literal.
  ///
  /// e.g. `hex"f"`, `hex"fe_f"`, or `hex'abc'`, which is invalid.
  #[from(skip)]
  Unpaired(PositionedChar<Char>),

  /// Trailing underscore or underscores are not allowed in hexadecimal string literals.
  ///
  /// e.g. `hex"fe_"` or `hex'fe__'`, which is invalid.
  #[from(skip)]
  TrailingUnderscore(Lexeme<Char>),

  /// Other hex string lexing error.
  Other(Message),
}

impl<Char> HexStringError<Char> {
  /// Create a new unclosed hex string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed(span: Span, kind: LitStrDelimiterKind) -> Self {
    Self::Unclosed(Unclosed::new(span, kind))
  }

  /// Create a new unclosed single quoted hex string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_single_quote(span: Span) -> Self {
    Self::unclosed(span, LitStrDelimiterKind::Single)
  }

  /// Create a new unclosed double quoted hex string literal error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unclosed_double_quote(span: Span) -> Self {
    Self::unclosed(span, LitStrDelimiterKind::Double)
  }

  /// Create a new unexpected line terminator error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unexpected_line_terminator(lexeme: UnexpectedLexeme<Char, LineTerminator>) -> Self {
    Self::UnexpectedLineTerminator(lexeme)
  }

  /// Create a unsupported character error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unsupported_character(char: PositionedChar<Char>) -> Self {
    Self::Unsupported(Lexeme::Char(char))
  }

  /// Create a unsupported characters error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unsupported_characters(span: Span) -> Self {
    Self::Unsupported(Lexeme::Range(span))
  }

  /// Create a leading underscores error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn leading_underscores(span: Span) -> Self {
    Self::LeadingUnderscore(Lexeme::Range(span))
  }

  /// Create a leading underscore error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn leading_underscore(char: PositionedChar<Char>) -> Self {
    Self::LeadingUnderscore(Lexeme::Char(char))
  }

  /// Create a consecutive underscores error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn consecutive_underscores(span: Span) -> Self {
    Self::ConsecutiveUnderscores(span)
  }

  /// Create a trailing underscores error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn trailing_underscores(span: Span) -> Self {
    Self::TrailingUnderscore(Lexeme::Range(span))
  }

  /// Create a trailing underscore error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn trailing_underscore(char: PositionedChar<Char>) -> Self {
    Self::TrailingUnderscore(Lexeme::Char(char))
  }

  /// Create an unpaired hex error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn unpaired(ch: PositionedChar<Char>) -> Self {
    Self::Unpaired(ch)
  }

  /// Create other hex string lexing error.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn other(message: impl Into<Message>) -> Self {
    Self::Other(message.into())
  }
}

impl<Char> core::fmt::Display for HexStringError<Char>
where
  Char: DisplayHuman,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Unclosed(err) => match err.delimiter_ref() {
        LitStrDelimiterKind::Single => write!(
          f,
          "unclosed single-quoted hex string literal at {}",
          err.span()
        ),
        LitStrDelimiterKind::Double => write!(
          f,
          "unclosed double-quoted hex string literal at {}",
          err.span()
        ),
      },
      Self::Unsupported(lexeme) => match lexeme {
        Lexeme::Range(span) => write!(
          f,
          "unsupported characters found in hex string literal at {}",
          span
        ),
        Lexeme::Char(ch) => write!(
          f,
          "unsupported character '{}' found in hex string literal at {}",
          ch.char_ref().display(),
          ch.position()
        ),
      },
      Self::UnexpectedLineTerminator(err) => {
        write!(
          f,
          "unexpected line terminator '{}' in hex string literal",
          err.hint(),
        )
      }
      Self::LeadingUnderscore(lexeme) => match lexeme {
        Lexeme::Range(span) => write!(
          f,
          "leading underscores found in hex string literal at {}",
          span
        ),
        Lexeme::Char(ch) => write!(
          f,
          "leading underscore found in hex string literal at {}",
          ch.position()
        ),
      },
      Self::ConsecutiveUnderscores(span) => write!(
        f,
        "consecutive underscores found in hex string literal at {}",
        span
      ),
      Self::Unpaired(ch) => write!(
        f,
        "unpaired hex character '{}' found in hex string literal at {}",
        ch.char_ref().display(),
        ch.position()
      ),
      Self::TrailingUnderscore(lexeme) => match lexeme {
        Lexeme::Range(span) => write!(
          f,
          "trailing underscores found in hex string literal at {}",
          span
        ),
        Lexeme::Char(ch) => write!(
          f,
          "trailing underscore found in hex string literal at {}",
          ch.position()
        ),
      },
      Self::Other(message) => message.fmt(f),
    }
  }
}

impl<Char> core::error::Error for HexStringError<Char> where Char: DisplayHuman + core::fmt::Debug {}
