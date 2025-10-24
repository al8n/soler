/// String literal types.
pub enum LitStr<S> {
  /// Single-quoted hex string literal
  SingleQuotedHex(S),
  /// Double-quoted hex string literal
  DoubleQuotedHex(S),
  /// Double-quoted string literal
  DoubleQuoted(S),
  /// Single-quoted string literal
  SingleQuoted(S),
  /// Empty double-quoted string literal
  EmptyDoubleQuoted(S),
  /// Empty single-quoted string literal
  EmptySingleQuoted(S),
  /// Double quoted unicode string literal
  DoubleQuotedUnicode(S),
  /// Single quoted unicode string literal
  SingleQuotedUnicode(S),
}
