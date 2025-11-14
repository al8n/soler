use core::marker::PhantomData;

use logosky::{
  IdentifierToken, KeywordToken, Lexed, LogoStream, Logos, PunctuatorToken, Source, Token,
  chumsky::{
    IterParser, Parser,
    container::Container as ChumskyContainer,
    extra::ParserExtra,
    prelude::*,
    token::{
      punct::{brace_close, brace_open},
      recovery::{emit_with, skip_through_closing_delimiter},
    },
  },
  error::{
    UnclosedBrace, UndelimitedBrace, UnexpectedEot, UnexpectedToken, UnopenedBrace,
  },
  syntax::Language,
  utils::{Span, cmp::Equivalent, delimiter::Delimiter},
};

use crate::{SyntaxKind, YUL};

/// A scaffold AST for Yul block statement.
///
/// See [Yul block statement](https://docs.soliditylang.org/en/latest/grammar.html#syntax-rule-SolidityParser.yulBlock)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Block<Statement, Container = Vec<Statement>, Lang = YUL> {
  span: Span,
  statements: Container,
  _m: PhantomData<Lang>,
  _s: PhantomData<Statement>,
}

impl<Statement, Container, Lang> Block<Statement, Container, Lang> {
  /// Create a new block statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new(span: Span, statements: Container) -> Self {
    Self {
      span,
      statements,
      _m: PhantomData,
      _s: PhantomData,
    }
  }

  /// Get the span of the block statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span(&self) -> Span {
    self.span
  }

  /// Returns the mutable span of the block statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn span_mut(&mut self) -> &mut Span {
    &mut self.span
  }

  /// Get the statements of the block statement.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn statements(&self) -> &Container {
    &self.statements
  }

  /// Returns the slice of the block statements.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn statements_slice(&self) -> &[Statement]
  where
    Container: AsRef<[Statement]>,
  {
    self.statements.as_ref()
  }

  /// Returns a parser for the Block with the given statement parser.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn parser<'a, I, T, Error, E>(
    statement_parser: impl Parser<'a, I, Statement, E> + Clone,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: PunctuatorToken<'a>,
    str: Equivalent<T>,
    Lang: Language,
    Lang::SyntaxKind: From<SyntaxKind> + 'a,
    Error:
      From<UnexpectedToken<'a, T, Lang::SyntaxKind>> + From<<T::Logos as Logos<'a>>::Error> + 'a,
    Container: ChumskyContainer<Statement>,
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    statement_parser
      .repeated()
      .collect()
      .delimited_by(
        brace_open(|| SyntaxKind::LBrace.into()),
        brace_close(|| SyntaxKind::RBrace.into()),
      )
      .map_with(|statements, exa| Self::new(exa.span(), statements))
  }

  /// Returns a parser for the Block with the given statement parser with recovery.
  ///
  /// This parser implements comprehensive error recovery for block delimiters while
  /// delegating statement-level recovery to the provided statement parser. It handles
  /// three distinct recovery scenarios based on delimiter presence.
  ///
  /// # Recovery Strategy
  ///
  /// This parser follows a **block-level recovery** philosophy:
  /// - **Block responsibility**: Handle brace presence/absence (structural errors)
  /// - **Statement responsibility**: The `statement_parser` handles statement-level errors
  /// - **Separation of concerns**: Each parser layer handles its own recovery boundaries
  ///
  /// ## Scenario 1: Has Opening Brace `{`
  ///
  /// **Input**: `{ stmt1 stmt2 stmt3` (missing closing brace) or `{ stmt1 stmt2 }`
  ///
  /// **Behavior**:
  /// - Parse statements normally
  /// - Closing brace is **optional** (`.or_not()`)
  /// - If missing: Emit `UnclosedBrace` error with span of entire block
  /// - Return `Block` with all successfully parsed statements
  ///
  /// ## Scenario 2: No Opening Brace, Has Closing Brace `}`
  ///
  /// **Input**: `stmt1 stmt2 }` (missing opening brace)
  ///
  /// **Behavior**:
  /// - Use `skip_through_closing_delimiter` to scan for closing brace
  /// - If found: Rewind and emit `UnopenedBrace` error
  /// - Parse statements from start to closing brace
  /// - Return `Block` with recovered statements
  ///
  /// ## Scenario 3: Neither Opening Nor Closing Brace
  ///
  /// **Input**: `stmt1 stmt2 stmt3` (no delimiters at all)
  ///
  /// **Behavior**:
  /// - Emit `UndelimitedBrace` error with span from start to EOF
  /// - Parse all remaining statements (until EOF or outer delimiter)
  /// - Return `Block` with all parsed statements
  ///
  /// # Error Emission
  ///
  /// - **Brace errors**: Emitted by this parser (UnclosedBrace, UnopenedBrace, UndelimitedBrace)
  /// - **Statement errors**: Emitted by the `statement_parser` (not this parser's concern)
  /// - **Lexer errors**: Automatically propagated through error emitter
  ///
  /// # Examples
  ///
  /// ## Well-Formed Block
  ///
  /// ```text
  /// Input:  { stmt1 stmt2 stmt3 }
  /// Errors: (none)
  /// Result: Block with 3 statements
  /// ```
  ///
  /// ## Missing Closing Brace
  ///
  /// ```text
  /// Input:  { stmt1 stmt2 stmt3
  /// Errors: UnclosedBrace at position of entire block
  /// Result: Block with 3 statements
  /// ```
  ///
  /// ## Missing Opening Brace
  ///
  /// ```text
  /// Input:  stmt1 stmt2 stmt3 }
  /// Errors: UnopenedBrace at position of content + closing brace
  /// Result: Block with 3 statements
  /// ```
  ///
  /// ## Missing Both Braces
  ///
  /// ```text
  /// Input:  stmt1 stmt2 stmt3
  /// Errors: UndelimitedBrace at position of entire content
  /// Result: Block with 3 statements
  /// ```
  ///
  /// ## With Malformed Statements
  ///
  /// ```text
  /// Input:  { good_stmt BAD!@# good_stmt }
  /// Errors: (statement parser emits error for BAD!@#)
  /// Result: Block with good_stmt, error_node, good_stmt
  /// ```
  ///
  /// The key insight is that **delimiter errors are structural** (block-level concern)
  /// while **syntax errors are local** (statement-level concern).
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn recoverable_parser<'a, I, T, Error, E>(
    statement_parser: impl Parser<'a, I, Statement, E> + Clone,
  ) -> impl Parser<'a, I, Self, E> + Clone
  where
    T: PunctuatorToken<'a> + KeywordToken<'a> + IdentifierToken<'a>,
    str: Equivalent<T>,
    Lang: Language,
    Lang::SyntaxKind: From<SyntaxKind> + 'a,
    Error: From<UnexpectedToken<'a, T, Lang::SyntaxKind>>
      + From<UnclosedBrace>
      + From<UndelimitedBrace>
      + From<UnopenedBrace>
      + From<UnexpectedEot>
      + From<<T::Logos as Logos<'a>>::Error>
      + Clone
      + 'a,
    Container: ChumskyContainer<Statement>,
    Self: Sized + 'a,
    I: LogoStream<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: Token<'a>,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    custom(move |inp| {
      let before = inp.cursor();

      // ============================================================
      // STEP 1: Check for opening brace
      // ============================================================
      let tok: Option<Lexed<'_, T>> = inp.peek();
      let open_brace_token = match tok {
        // Empty input: cannot parse block at all
        None => return Err(Error::from(UnexpectedEot::eot(inp.span_since(&before)))),
        Some(Lexed::Token(t)) if t.is_brace_open() => {
          inp.skip(); // Consume the opening brace
          true
        }
        // Other token: might still be valid block (without opening brace)
        _ => false,
      };

      // ============================================================
      // SCENARIO 1: Has opening brace
      // ============================================================
      if open_brace_token {
        // Parse statements until closing brace (or EOF if missing)
        let statements = inp.parse(
          statement_parser.clone().repeated().collect().then_ignore(
            brace_close(|| SyntaxKind::RBrace.into())
              .or_not() // Make closing brace optional for recovery
              .validate(|t, exa, emitter| {
                if t.is_none() {
                  // Closing brace is missing - emit error
                  emitter.emit(Error::from(UnclosedBrace::brace(exa.span())));
                }
                t
              }),
          ),
        )?;
        return Ok(Self::new(inp.span_since(&before), statements));
      }

      // ============================================================
      // STEP 2: No opening brace - scan for closing brace
      // ============================================================
      let checkpoint = inp.save();

      // Scan ahead to see if there's a closing brace anywhere
      let (scaned, rb) = inp.parse(skip_through_closing_delimiter(Delimiter::Brace))?;

      // ============================================================
      // SCENARIO 2: No opening brace, but has closing brace
      // ============================================================
      if rb.is_some() {
        inp.rewind(checkpoint); // Rewind to parse statements properly
        // Emit unopened brace error (use scaned span from start to closing brace)
        let _ = inp.parse(emit_with(move || Error::from(UnopenedBrace::brace(scaned))));

        // Parse statements from start up to the closing brace
        let statements = inp.parse(
          statement_parser
            .clone()
            .repeated()
            .collect()
            .then_ignore(brace_close(|| SyntaxKind::RBrace.into())),
        )?;
        return Ok(Self::new(inp.span_since(&before), statements));
      }

      // ============================================================
      // SCENARIO 3: Neither opening nor closing brace
      // ============================================================
      inp.rewind(checkpoint); // Rewind to parse all statements
      // Emit undelimited brace error (use scaned span from start to EOF)
      let _ = inp.parse(emit_with(move || {
        Error::from(UndelimitedBrace::brace(scaned))
      }));

      // Parse all remaining statements (until EOF or outer delimiter)
      // The statement parser handles its own recovery for malformed statements
      let statements = inp.parse(statement_parser.clone().repeated().collect())?;

      Ok(Self::new(inp.span_since(&before), statements))
    })
  }
}
