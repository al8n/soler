use core::marker::PhantomData;

use derive_more::Display;
use logosky::{
  syntax::Syntax,
  utils::{
    GenericArrayDeque,
    typenum::{U2, U3, U4},
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
pub enum SingleVariableDeclarationComponent {
  /// The `let` keyword.
  #[display("let keyword")]
  LetKeyword,
  /// The name of the variable.
  #[display("name")]
  Name,
  /// The colon assign `:=` token.
  #[display(":=")]
  ColonAssign,
  /// The expression on the right-hand side of the variable declaration.
  #[display("expression")]
  Expression,
}

/// A syntax representation of a Yul single variable declaration.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("single variable declaration")]
pub struct SingleVariableDeclaration<Lang = YUL>(PhantomData<Lang>);

impl<Lang> Default for SingleVariableDeclaration<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl Syntax for SingleVariableDeclaration {
  type Lang = YUL;

  const KIND: SyntaxKind = SyntaxKind::SingleVariableDeclaration;

  type Component = SingleVariableDeclarationComponent;

  type COMPONENTS = U4;

  type REQUIRED = U2;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    const COMPONENTS: &GenericArrayDeque<SingleVariableDeclarationComponent, U4> = &{
      GenericArrayDeque::from_array([
        SingleVariableDeclarationComponent::LetKeyword,
        SingleVariableDeclarationComponent::Name,
        SingleVariableDeclarationComponent::ColonAssign,
        SingleVariableDeclarationComponent::Expression,
      ])
    };

    COMPONENTS
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    const REQUIRED: &GenericArrayDeque<SingleVariableDeclarationComponent, U2> = &{
      GenericArrayDeque::from_array([
        SingleVariableDeclarationComponent::LetKeyword,
        SingleVariableDeclarationComponent::Name,
      ])
    };

    REQUIRED
  }
}

/// The multiple variables declaration component.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
pub enum MultipleVariablesDeclarationComponent {
  /// The `let` keyword.
  #[display("let keyword")]
  LetKeyword,
  /// The names of the variables.
  #[display("names")]
  Names,
  /// The colon assign `:=` token.
  #[display(":=")]
  ColonAssign,
  /// The function call on the right-hand side of the variable declaration.
  #[display("function call")]
  FunctionCall,
}

/// A syntax representation of a Yul multiple variables declaration.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("multiple variables declaration")]
pub struct MultipleVariablesDeclaration<Lang = YUL>(PhantomData<Lang>);

impl<Lang> Default for MultipleVariablesDeclaration<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl Syntax for MultipleVariablesDeclaration {
  type Lang = YUL;

  const KIND: SyntaxKind = SyntaxKind::MultipleVariablesDeclaration;

  type Component = MultipleVariablesDeclarationComponent;

  type COMPONENTS = U4;

  type REQUIRED = U2;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    const COMPONENTS: &GenericArrayDeque<MultipleVariablesDeclarationComponent, U4> = &{
      GenericArrayDeque::from_array([
        MultipleVariablesDeclarationComponent::LetKeyword,
        MultipleVariablesDeclarationComponent::Names,
        MultipleVariablesDeclarationComponent::ColonAssign,
        MultipleVariablesDeclarationComponent::FunctionCall,
      ])
    };

    COMPONENTS
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    const REQUIRED: &GenericArrayDeque<MultipleVariablesDeclarationComponent, U2> = &{
      GenericArrayDeque::from_array([
        MultipleVariablesDeclarationComponent::LetKeyword,
        MultipleVariablesDeclarationComponent::Names,
      ])
    };

    REQUIRED
  }
}

/// The component of a variable declaration.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
pub enum VariableDeclarationComponent {
  /// The `let` keyword.
  #[display("let keyword")]
  LetKeyword,
  /// The left-hand side of a variable declaration.
  #[display("left-hand side")]
  Lhs,
  /// The colon assign `:=` token.
  #[display(":=")]
  ColonAssign,
  /// The right-hand side of a variable declaration.
  #[display("right-hand side")]
  Rhs,
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

  type COMPONENTS = U4;

  type REQUIRED = U2;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    const COMPONENTS: &GenericArrayDeque<VariableDeclarationComponent, U4> = &{
      GenericArrayDeque::from_array([
        VariableDeclarationComponent::LetKeyword,
        VariableDeclarationComponent::Lhs,
        VariableDeclarationComponent::ColonAssign,
        VariableDeclarationComponent::Rhs,
      ])
    };

    COMPONENTS
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    const REQUIRED: &GenericArrayDeque<VariableDeclarationComponent, U2> = &{
      GenericArrayDeque::from_array([
        VariableDeclarationComponent::LetKeyword,
        VariableDeclarationComponent::Lhs,
      ])
    };

    REQUIRED
  }
}

/// The single target assignment component.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
pub enum SingleTargetAssignmentComponent {
  /// The left-hand side of an assignment.
  #[display("name")]
  Name,
  /// The colon assign `:=` token.
  #[display(":=")]
  ColonAssign,
  /// The right-hand side of an assignment.
  #[display("expression")]
  Expression,
}

/// A syntax representation of a Yul single target assignment.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("single target assignment")]
pub struct SingleTargetAssignment<Lang = YUL>(PhantomData<Lang>);

impl<Lang> Default for SingleTargetAssignment<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl Syntax for SingleTargetAssignment {
  type Lang = YUL;

  const KIND: SyntaxKind = SyntaxKind::SingleTargetAssignment;

  type Component = SingleTargetAssignmentComponent;

  type COMPONENTS = U3;

  type REQUIRED = U3;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    Self::required_components()
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    const REQUIRED: &GenericArrayDeque<SingleTargetAssignmentComponent, U3> = &{
      GenericArrayDeque::from_array([
        SingleTargetAssignmentComponent::Name,
        SingleTargetAssignmentComponent::ColonAssign,
        SingleTargetAssignmentComponent::Expression,
      ])
    };

    REQUIRED
  }
}

/// The multiple targets assignment component.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
pub enum MultipleTargetsAssignmentComponent {
  /// The names on the left-hand side of an assignment.
  #[display("names")]
  Names,
  /// The colon assign `:=` token.
  #[display(":=")]
  ColonAssign,
  /// The function call on the right-hand side of an assignment.
  #[display("function call")]
  FunctionCall,
}

/// A syntax representation of a Yul multiple targets assignment.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("multiple targets assignment")]
pub struct MultipleTargetsAssignment<Lang = YUL>(PhantomData<Lang>);

impl<Lang> Default for MultipleTargetsAssignment<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl Syntax for MultipleTargetsAssignment {
  type Lang = YUL;

  const KIND: SyntaxKind = SyntaxKind::MultipleTargetAssignment;

  type Component = MultipleTargetsAssignmentComponent;

  type COMPONENTS = U3;

  type REQUIRED = U3;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    Self::required_components()
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    const REQUIRED: &GenericArrayDeque<MultipleTargetsAssignmentComponent, U3> = &{
      GenericArrayDeque::from_array([
        MultipleTargetsAssignmentComponent::Names,
        MultipleTargetsAssignmentComponent::ColonAssign,
        MultipleTargetsAssignmentComponent::FunctionCall,
      ])
    };

    REQUIRED
  }
}

/// The component of an assignment.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
pub enum AssignmentComponent {
  /// The left-hand side of an assignment.
  #[display("left-hand side")]
  Lhs,
  /// The colon assign `:=` token.
  #[display("colon assign")]
  ColonAssign,
  /// The right-hand side of an assignment.
  #[display("right-hand side")]
  Rhs,
}

/// A syntax representation of a Yul variable declaration.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("assignment")]
pub struct Assignment<Lang = YUL>(PhantomData<Lang>);

impl<Lang> Default for Assignment<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl Syntax for Assignment {
  type Lang = YUL;

  const KIND: SyntaxKind = SyntaxKind::Assignment;

  type Component = AssignmentComponent;

  type COMPONENTS = U3;

  type REQUIRED = U3;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
    Self::required_components()
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
    const REQUIRED: &GenericArrayDeque<AssignmentComponent, U3> = &{
      GenericArrayDeque::from_array([
        AssignmentComponent::Lhs,
        AssignmentComponent::ColonAssign,
        AssignmentComponent::Rhs,
      ])
    };

    REQUIRED
  }
}
