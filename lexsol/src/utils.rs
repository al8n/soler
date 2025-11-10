use core::mem::MaybeUninit;

/// The default container for no-alloc environments, which can hold at most N elements.
#[derive(Debug)]
pub struct GenericVec<T, const N: usize> {
  values: [MaybeUninit<T>; N],
  len: usize,
}

impl<T, const N: usize> Clone for GenericVec<T, N>
where
  T: Clone,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn clone(&self) -> Self {
    let mut new = Self::new();
    for value in self.iter() {
      // SAFETY: We ensure that the capacity is not exceeded.
      unsafe {
        new.push_unchecked(value.clone());
      }
    }
    new
  }
}

impl<T, const N: usize> FromIterator<T> for GenericVec<T, N> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
    let mut vec = Self::new();
    for item in iter {
      if vec.push(item).is_err() {
        break;
      }
    }
    vec
  }
}

impl<T, const N: usize> AsRef<[T]> for GenericVec<T, N> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_ref(&self) -> &[T] {
    self
  }
}

impl<T, const N: usize> AsMut<[T]> for GenericVec<T, N> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn as_mut(&mut self) -> &mut [T] {
    self
  }
}

impl<T, const N: usize> core::ops::Deref for GenericVec<T, N> {
  type Target = [T];

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn deref(&self) -> &Self::Target {
    self.as_slice()
  }
}

impl<T, const N: usize> core::ops::DerefMut for GenericVec<T, N> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.as_mut_slice()
  }
}

impl<T, const N: usize> Default for GenericVec<T, N> {
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn default() -> Self {
    Self::new()
  }
}

impl<T, const N: usize> GenericVec<T, N> {
  /// Create a new empty `GenericVec`.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn new() -> Self {
    Self {
      values: [const { MaybeUninit::<T>::uninit() }; N],
      len: 0,
    }
  }

  /// Push a value to the end of the `GenericVec`. Returns `Err(value)` if the capacity is exceeded.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn push(&mut self, value: T) -> Result<(), T> {
    if self.len < N {
      self.values[self.len].write(value);
      self.len += 1;
      Ok(())
    } else {
      Err(value)
    }
  }

  /// Push a value to the end of the `GenericVec` without checking capacity.
  ///
  /// # Safety
  /// - The caller must ensure that the capacity is not exceeded.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const unsafe fn push_unchecked(&mut self, value: T) {
    self.values[self.len].write(value);
    self.len += 1;
  }

  /// Returns an iterator over the values.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub fn iter(&self) -> core::slice::Iter<'_, T> {
    self.as_slice().iter()
  }

  /// Returns a slice of the values.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn as_slice(&self) -> &[T] {
    // SAFETY: We ensure that `self.len` is always less than or equal to `N`, and that all
    unsafe { core::slice::from_raw_parts(self.values.as_ptr() as *const T, self.len) }
  }

  /// Returns a mutable slice of the values.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub const fn as_mut_slice(&mut self) -> &mut [T] {
    // SAFETY: We ensure that `self.len` is always less than or equal to `N`, and that all
    unsafe { core::slice::from_raw_parts_mut(self.values.as_mut_ptr() as *mut T, self.len) }
  }
}

pub(super) mod sealed {
  use derive_more::{Deref, DerefMut};

  macro_rules! lit_str_lexer {
    ($name:ident) => {
      #[derive(Deref, DerefMut)]
      #[repr(transparent)]
      pub struct $name<L: ?Sized, Char: ?Sized, StringError: ?Sized, Error: ?Sized> {
        _string_err: core::marker::PhantomData<StringError>,
        _err: core::marker::PhantomData<Error>,
        _char: core::marker::PhantomData<Char>,
        #[deref]
        #[deref_mut]
        lexer: L,
      }

      impl<T, Char, StringError, Error> $name<T, Char, StringError, Error> {
        #[cfg_attr(not(tarpaulin), inline(always))]
        pub const fn from_mut(t: &mut T) -> &mut Self {
          // Safety: This is safe because repr(transparent) over T
          unsafe { &mut *(t as *mut T as *mut Self) }
        }
      }
    };
  }

  lit_str_lexer!(SingleQuotedRegularStrLexer);
  lit_str_lexer!(DoubleQuotedRegularStrLexer);
  lit_str_lexer!(SingleQuotedHexStrLexer);
  lit_str_lexer!(DoubleQuotedHexStrLexer);
  lit_str_lexer!(SingleQuotedUnicodeStrLexer);
  lit_str_lexer!(DoubleQuotedUnicodeStrLexer);
}

/// A collection of error types used in the lexer
pub trait Container<T> {
  /// The iterator type for the container.
  type IntoIter: Iterator<Item = T>;
  /// The iterator type for references to the container.
  type Iter<'a>: Iterator<Item = &'a T>
  where
    Self: 'a,
    T: 'a;

  /// Create a new, empty container.
  fn new() -> Self;

  /// Create a new container with a specified capacity.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn with_capacity(_: usize) -> Self
  where
    Self: Sized,
  {
    Self::new()
  }

  /// Push an error into the collection.
  fn push(&mut self, error: T);

  /// Pop an error from the first of the collection.
  fn pop(&mut self) -> Option<T>;

  /// Returns `true` if the collection is empty.
  #[cfg_attr(not(tarpaulin), inline(always))]
  fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Returns the number of errors in the collection.
  fn len(&self) -> usize;

  /// Returns an iterator over the errors in the collection.
  fn iter(&self) -> Self::Iter<'_>;

  /// Consumes the container and returns an iterator over the errors.
  fn into_iter(self) -> Self::IntoIter;
}

impl<T> Container<T> for Option<T> {
  type IntoIter = core::option::IntoIter<T>;
  type Iter<'a>
    = core::option::Iter<'a, T>
  where
    T: 'a;

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn new() -> Self {
    None
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn push(&mut self, error: T) {
    self.get_or_insert(error);
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn pop(&mut self) -> Option<T> {
    self.take()
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn len(&self) -> usize {
    if self.is_some() { 1 } else { 0 }
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn iter(&self) -> Self::Iter<'_> {
    Self::iter(self)
  }

  #[cfg_attr(not(tarpaulin), inline(always))]
  fn into_iter(self) -> Self::IntoIter {
    <Self as IntoIterator>::into_iter(self)
  }
}

#[cfg(any(feature = "std", feature = "alloc"))]
const _: () = {
  use std::{
    collections::{VecDeque, vec_deque},
    vec::{self, Vec},
  };

  impl<T> Container<T> for Vec<T> {
    type IntoIter = vec::IntoIter<T>;
    type Iter<'a>
      = core::slice::Iter<'a, T>
    where
      T: 'a;

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn new() -> Self {
      Self::new()
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn with_capacity(capacity: usize) -> Self {
      Self::with_capacity(capacity)
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn push(&mut self, error: T) {
      self.push(error);
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn pop(&mut self) -> Option<T> {
      if self.is_empty() {
        None
      } else {
        Some(self.remove(0))
      }
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn len(&self) -> usize {
      self.len()
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn iter(&self) -> Self::Iter<'_> {
      self.as_slice().iter()
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn into_iter(self) -> Self::IntoIter {
      <Self as IntoIterator>::into_iter(self)
    }
  }

  impl<T> Container<T> for VecDeque<T> {
    type IntoIter = vec_deque::IntoIter<T>;
    type Iter<'a>
      = vec_deque::Iter<'a, T>
    where
      T: 'a,
      Self: 'a;

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn new() -> Self {
      Self::new()
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn with_capacity(capacity: usize) -> Self {
      Self::with_capacity(capacity)
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn push(&mut self, error: T) {
      self.push_back(error);
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn pop(&mut self) -> Option<T> {
      self.pop_front()
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn len(&self) -> usize {
      self.len()
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn iter(&self) -> Self::Iter<'_> {
      self.iter()
    }

    #[cfg_attr(not(tarpaulin), inline(always))]
    fn into_iter(self) -> Self::IntoIter {
      <Self as IntoIterator>::into_iter(self)
    }
  }
};

pub(crate) trait Wrapper {
  type Underlying;

  fn from_underlying(underlying: Self::Underlying) -> Self;
}
