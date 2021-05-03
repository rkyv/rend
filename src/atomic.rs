use crate::{Endian, Endianness};
use core::{
    marker::PhantomData,
    sync::atomic::{AtomicI16, AtomicI32, AtomicI64, AtomicU16, AtomicU32, AtomicU64, Ordering},
};

macro_rules! impl_atomic {
    ($ty:ty, $ne:ty) => {
        impl<E: Endianness> Endian<$ty, E> {
            /// Stores a value into the atomic integer if the current value is the same as the
            /// `current` value.
            #[inline]
            pub fn compare_exchange(
                &self,
                current: $ne,
                new: $ne,
                success: Ordering,
                failure: Ordering,
            ) -> Result<$ne, $ne> {
                match self.value.compare_exchange(
                    E::swap_native(current),
                    E::swap_native(new),
                    success,
                    failure,
                ) {
                    Ok(x) => Ok(E::swap_native(x)),
                    Err(x) => Err(E::swap_native(x)),
                }
            }

            /// Adds to the current value, returning the previous value.
            #[inline]
            pub fn fetch_add(&self, val: $ne, order: Ordering) -> $ne {
                self.fetch_update(order, order, |x| Some(x + val)).unwrap()
            }

            /// Bitwise "and" with the current value.
            #[inline]
            pub fn fetch_and(&self, val: $ne, order: Ordering) -> $ne {
                E::swap_native(self.value.fetch_and(E::swap_native(val), order))
            }

            /// Maximum with the current value.
            #[inline]
            pub fn fetch_max(&self, val: $ne, order: Ordering) -> $ne {
                self.fetch_update(order, order, |x| Some(<$ne>::max(x, val)))
                    .unwrap()
            }

            /// Minimum with the current value.
            #[inline]
            pub fn fetch_min(&self, val: $ne, order: Ordering) -> $ne {
                self.fetch_update(order, order, |x| Some(<$ne>::min(x, val)))
                    .unwrap()
            }

            /// Bitwise "nand" with the current value.
            #[inline]
            pub fn fetch_nand(&self, val: $ne, order: Ordering) -> $ne {
                E::swap_native(self.value.fetch_nand(E::swap_native(val), order))
            }

            /// Bitwise "or" with the current value.
            #[inline]
            pub fn fetch_or(&self, val: $ne, order: Ordering) -> $ne {
                E::swap_native(self.value.fetch_or(E::swap_native(val), order))
            }

            /// Subtracts from the current value, returning the previous value.
            #[inline]
            pub fn fetch_sub(&self, val: $ne, order: Ordering) -> $ne {
                self.fetch_update(order, order, |x| Some(x - val)).unwrap()
            }

            /// Fetches the value, and applies a function to it that returns an optional new value.
            /// Returns a `Result` of `Ok(previous_value)` if the function returned `Some(_)`, else
            /// `Err(previous_value)`.
            #[inline]
            pub fn fetch_update<F: FnMut($ne) -> Option<$ne>>(
                &self,
                set_order: Ordering,
                fetch_order: Ordering,
                mut f: F,
            ) -> Result<$ne, $ne> {
                self.value.fetch_update(set_order, fetch_order, |x| {
                    f(E::swap_native(x)).map(|x| E::swap_native(x))
                })
            }

            /// Bitwise "xor" with the current value.
            #[inline]
            pub fn fetch_xor(&self, val: $ne, order: Ordering) -> $ne {
                E::swap_native(self.value.fetch_xor(E::swap_native(val), order))
            }

            /// Consumes the atomic and returns the contained value.
            #[inline]
            pub fn into_inner(self) -> $ne {
                E::swap_native(self.value.into_inner())
            }

            /// Loads a value from the atomic integer.
            #[inline]
            pub fn load(&self, order: Ordering) -> $ne {
                E::swap_native(self.value.load(order))
            }

            /// Creates a new atomic integer.
            #[inline]
            pub fn new(native: $ne) -> Self {
                Self {
                    value: <$ty>::new(E::swap_native(native)),
                    _phantom: PhantomData,
                }
            }

            /// Stores a value into the atomic integer.
            #[inline]
            pub fn store(&self, val: $ne, order: Ordering) {
                self.value.store(E::swap_native(val), order);
            }

            /// Stores a value into the atomic integer, returning the previous value.
            #[inline]
            pub fn swap(&self, val: $ne, order: Ordering) -> $ne {
                E::swap_native(self.value.swap(E::swap_native(val), order))
            }
        }

        impl<E: Endianness> core::fmt::Debug for Endian<$ty, E> {
            #[inline]
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                E::swap_native(self.load(Ordering::Relaxed)).fmt(f)
            }
        }

        impl<E: Endianness> Default for Endian<$ty, E> {
            #[inline]
            fn default() -> Self {
                Self::new(<$ne>::default())
            }
        }

        impl<E: Endianness> From<$ne> for Endian<$ty, E> {
            #[inline]
            fn from(value: $ne) -> Self {
                Self::new(value)
            }
        }

        #[cfg(feature = "std")]
        impl<E: Endianness> ::std::panic::RefUnwindSafe for Endian<$ty, E> {}

        unsafe impl<E: Endianness> Sync for Endian<$ty, E> {}
    };
}

impl_atomic!(AtomicI16, i16);
impl_atomic!(AtomicI32, i32);
#[cfg(has_atomics_64)]
impl_atomic!(AtomicI64, i64);
impl_atomic!(AtomicU16, u16);
impl_atomic!(AtomicU32, u32);
#[cfg(has_atomics_64)]
impl_atomic!(AtomicU64, u64);
