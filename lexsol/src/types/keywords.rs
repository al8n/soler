macro_rules! keyword {
  ($(($name:ident, $syntax_tree_display: literal, $keyword:literal)),+$(,)?) => {
    paste::paste! {
      $(
        tokit::keyword! {
          ($name, $syntax_tree_display, $keyword)
        }
      )*
    }
  }
}

keyword! {
  (Default, "default_KW", "default"),
  (Function, "function_KW", "function"),
  (For, "for_KW", "for"),
  (If, "if_KW", "if"),
  (Let, "let_KW", "let"),
  (Leave, "leave_KW", "leave"),
  (Continue, "continue_KW", "continue"),
  (Break, "break_KW", "break"),
  (Switch, "switch_KW", "switch"),
  (Case, "case_KW", "case"),
}
