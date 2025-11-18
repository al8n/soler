use core::marker::PhantomData;

use derive_more::Display;
use logosky::{
  syntax::Syntax,
  utils::{
    GenericArrayDeque,
    typenum::{U2, U5},
  },
};

use crate::{SyntaxKind, YUL};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("statement")]
pub struct Statement<Lang = YUL>(PhantomData<Lang>);

impl<Lang> Default for Statement<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("expression")]
pub struct Expression<Lang = YUL>(PhantomData<Lang>);

impl<Lang> Default for Expression<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

/// The component of a variable declaration.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
pub enum VariableDeclarationComponent {
  /// The `let` keyword.
  #[display("let keyword")]
  LetKeyword,
  /// The variable names
  #[display("assignee")]
  Assignee,
  /// The colon assign `:=` token.
  #[display("colon assign")]
  ColonAssign,
  /// The function call of the variable declaration.
  #[display("function call")]
  FunctionCall,
  /// The expression of the variable declaration.
  #[display("expression")]
  Expression,
}

/// A syntax representation of a Yul variable declaration.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("variable declaration")]
pub struct VariableDeclaration<Lang = YUL>(PhantomData<Lang>);

impl<Lang> Default for VariableDeclaration<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl Syntax for VariableDeclaration {
  type Lang = YUL;

  const KIND: SyntaxKind = SyntaxKind::VariableDeclaration;

  type Component = VariableDeclarationComponent;

  type COMPONENTS = U5;

  type REQUIRED = U2;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    const COMPONENTS: &GenericArrayDeque<VariableDeclarationComponent, U5> = &{
      GenericArrayDeque::from_array([
        VariableDeclarationComponent::LetKeyword,
        VariableDeclarationComponent::Assignee,
        VariableDeclarationComponent::ColonAssign,
        VariableDeclarationComponent::FunctionCall,
        VariableDeclarationComponent::Expression,
      ])
    };

    COMPONENTS
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    const REQUIRED: &GenericArrayDeque<VariableDeclarationComponent, U2> = &{
      GenericArrayDeque::from_array([
        VariableDeclarationComponent::LetKeyword,
        VariableDeclarationComponent::Assignee,
      ])
    };

    REQUIRED
  }
}
