use core::marker::PhantomData;

use derive_more::Display;
use lexsol::yul::{
  self, Yul, lossless::SyntaxKind as LosslessSyntaxKind,
  syntactic::SyntaxKind as SyntacticSyntaxKind,
};
use tokit::{
  syntax::Syntax,
  utils::{
    GenericArrayDeque,
    typenum::{U2, U3, U4},
  },
};

type DefaultLang = Yul<yul::syntactic::SyntaxKind>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("statement")]
pub struct Statement<Lang = DefaultLang>(PhantomData<Lang>);
impl<Lang> Default for Statement<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Display)]
#[display("expression")]
pub struct Expression<Lang = DefaultLang>(PhantomData<Lang>);

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
pub struct SingleVariableDeclaration<Lang = DefaultLang>(PhantomData<Lang>);

impl<Lang> Default for SingleVariableDeclaration<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

macro_rules! impl_syntax {
  (impl $name:ident<$($kind:ty),+$(,)?> {
    type Component = $component:ident;
    type COMPONENTS = $components:ty;
    type REQUIRED = $required:ty;

    fn possible_components() => $possible_components:expr;
    fn required_components() => $required_components:expr;
  }) => {
    $(
      impl Syntax for $name<Yul<$kind>> {
        type Lang = Yul<$kind>;

        const KIND: $kind = <$kind>::$name;

        type Component = $component;

        type COMPONENTS = $components;

        type REQUIRED = $required;

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn possible_components() -> &'static GenericArrayDeque<Self::Component, Self::COMPONENTS> {
          const COMPONENTS: &GenericArrayDeque<$component, $components> = &{
            $possible_components
          };

          COMPONENTS
        }

        #[cfg_attr(not(tarpaulin), inline(always))]
        fn required_components() -> &'static GenericArrayDeque<Self::Component, Self::REQUIRED> {
          const REQUIRED: &GenericArrayDeque<$component, $required> = &{
            $required_components
          };

          REQUIRED
        }
      }
    )*
  };
}

impl_syntax!(
  impl SingleVariableDeclaration<SyntacticSyntaxKind, LosslessSyntaxKind> {
    type Component = SingleVariableDeclarationComponent;
    type COMPONENTS = U4;
    type REQUIRED = U2;

    fn possible_components() => {
      GenericArrayDeque::from_array([
        SingleVariableDeclarationComponent::LetKeyword,
        SingleVariableDeclarationComponent::Name,
        SingleVariableDeclarationComponent::ColonAssign,
        SingleVariableDeclarationComponent::Expression,
      ])
    };
    fn required_components() => {
      GenericArrayDeque::from_array([
        SingleVariableDeclarationComponent::LetKeyword,
        SingleVariableDeclarationComponent::Name,
      ])
    };
  }
);

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
pub struct MultipleVariablesDeclaration<Lang = DefaultLang>(PhantomData<Lang>);

impl<Lang> Default for MultipleVariablesDeclaration<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl_syntax!(
  impl MultipleVariablesDeclaration<SyntacticSyntaxKind, LosslessSyntaxKind> {
    type Component = MultipleVariablesDeclarationComponent;
    type COMPONENTS = U4;
    type REQUIRED = U2;

    fn possible_components() => {
      GenericArrayDeque::from_array([
        MultipleVariablesDeclarationComponent::LetKeyword,
        MultipleVariablesDeclarationComponent::Names,
        MultipleVariablesDeclarationComponent::ColonAssign,
        MultipleVariablesDeclarationComponent::FunctionCall,
      ])
    };
    fn required_components() => {
      GenericArrayDeque::from_array([
        MultipleVariablesDeclarationComponent::LetKeyword,
        MultipleVariablesDeclarationComponent::Names,
      ])
    };
  }
);

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
pub struct VariableDeclaration<Lang = DefaultLang>(PhantomData<Lang>);

impl<Lang> Default for VariableDeclaration<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl_syntax!(
  impl VariableDeclaration<SyntacticSyntaxKind, LosslessSyntaxKind> {
    type Component = VariableDeclarationComponent;
    type COMPONENTS = U4;
    type REQUIRED = U2;

    fn possible_components() => {
      GenericArrayDeque::from_array([
        VariableDeclarationComponent::LetKeyword,
        VariableDeclarationComponent::Lhs,
        VariableDeclarationComponent::ColonAssign,
        VariableDeclarationComponent::Rhs,
      ])
    };
    fn required_components() => {
      GenericArrayDeque::from_array([
        VariableDeclarationComponent::LetKeyword,
        VariableDeclarationComponent::Lhs,
      ])
    };
  }
);

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
pub struct SingleTargetAssignment<Lang = DefaultLang>(PhantomData<Lang>);

impl<Lang> Default for SingleTargetAssignment<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl_syntax!(
  impl SingleTargetAssignment<SyntacticSyntaxKind, LosslessSyntaxKind> {
    type Component = SingleTargetAssignmentComponent;
    type COMPONENTS = U3;
    type REQUIRED = U3;

    fn possible_components() => {
      GenericArrayDeque::from_array([
        SingleTargetAssignmentComponent::Name,
        SingleTargetAssignmentComponent::ColonAssign,
        SingleTargetAssignmentComponent::Expression,
      ])
    };
    fn required_components() => {
      GenericArrayDeque::from_array([
        SingleTargetAssignmentComponent::Name,
        SingleTargetAssignmentComponent::ColonAssign,
        SingleTargetAssignmentComponent::Expression,
      ])
    };
  }
);

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
pub struct MultipleTargetsAssignment<Lang = DefaultLang>(PhantomData<Lang>);

impl<Lang> Default for MultipleTargetsAssignment<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl_syntax!(
  impl MultipleTargetsAssignment<SyntacticSyntaxKind, LosslessSyntaxKind> {
    type Component = MultipleTargetsAssignmentComponent;
    type COMPONENTS = U3;
    type REQUIRED = U3;

    fn possible_components() => {
      GenericArrayDeque::from_array([
        MultipleTargetsAssignmentComponent::Names,
        MultipleTargetsAssignmentComponent::ColonAssign,
        MultipleTargetsAssignmentComponent::FunctionCall,
      ])
    };
    fn required_components() => {
      GenericArrayDeque::from_array([
        MultipleTargetsAssignmentComponent::Names,
        MultipleTargetsAssignmentComponent::ColonAssign,
        MultipleTargetsAssignmentComponent::FunctionCall,
      ])
    };
  }
);

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
pub struct Assignment<Lang = DefaultLang>(PhantomData<Lang>);

impl<Lang> Default for Assignment<Lang> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self(PhantomData)
  }
}

impl_syntax!(
  impl Assignment<SyntacticSyntaxKind, LosslessSyntaxKind> {
    type Component = AssignmentComponent;
    type COMPONENTS = U3;
    type REQUIRED = U3;

    fn possible_components() => {
      GenericArrayDeque::from_array([
        AssignmentComponent::Lhs,
        AssignmentComponent::ColonAssign,
        AssignmentComponent::Rhs,
      ])
    };
    fn required_components() => {
      GenericArrayDeque::from_array([
        AssignmentComponent::Lhs,
        AssignmentComponent::ColonAssign,
        AssignmentComponent::Rhs,
      ])
    };
  }
);
