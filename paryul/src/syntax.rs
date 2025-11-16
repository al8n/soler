use core::marker::PhantomData;

use derive_more::Display;

use crate::YUL;

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
