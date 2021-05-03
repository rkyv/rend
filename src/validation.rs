use crate::{Endian, Endianness};
use core::{
    convert::TryFrom,
    num::{
        NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128, NonZeroU16, NonZeroU32, NonZeroU64,
        NonZeroU128
    },
};
use bytecheck::{CharCheckError, CheckBytes, NonZeroCheckError, Unreachable};
#[cfg(has_atomics)]
use core::sync::atomic::{AtomicI16, AtomicI32, AtomicU16, AtomicU32};
#[cfg(has_atomics_64)]
use core::sync::atomic::{AtomicI64, AtomicU64};

macro_rules! impl_primitive {
    ($type:ty) => {
        impl<C: ?Sized, E> CheckBytes<C> for Endian<$type, E> {
            type Error = Unreachable;

            #[inline]
            unsafe fn check_bytes<'a>(value: *const Self, _: &mut C) -> Result<&'a Self, Self::Error> {
                Ok(&*value)
            }
        }
    }
}

impl_primitive!(i16);
impl_primitive!(i32);
impl_primitive!(i64);
impl_primitive!(i128);
impl_primitive!(u16);
impl_primitive!(u32);
impl_primitive!(u64);
impl_primitive!(u128);
impl_primitive!(f32);
impl_primitive!(f64);

impl<C: ?Sized, E: Endianness> CheckBytes<C> for Endian<char, E> {
    type Error = CharCheckError;

    #[inline]
    unsafe fn check_bytes<'a>(value: *const Self, context: &mut C) -> Result<&'a Self, Self::Error> {
        let as_u32 = &*<Endian<u32, E>>::check_bytes(value.cast(), context)?;
        let c = as_u32.to_ne();
        char::try_from(c).map_err(|_| CharCheckError {
            invalid_value: c,
        })?;
        Ok(&*value)
    }
}

macro_rules! impl_nonzero {
    ($nonzero:ident, $underlying:ident) => {
        impl<C: ?Sized, E> CheckBytes<C> for Endian<$nonzero, E> {
            type Error = NonZeroCheckError;

            #[inline]
            unsafe fn check_bytes<'a>(
                value: *const Self,
                context: &mut C,
            ) -> Result<&'a Self, Self::Error> {
                if *$underlying::check_bytes(value.cast(), context)? == 0 {
                    Err(NonZeroCheckError::IsZero)
                } else {
                    Ok(&*value)
                }
            }
        }
    };
}

impl_nonzero!(NonZeroI16, i16);
impl_nonzero!(NonZeroI32, i32);
impl_nonzero!(NonZeroI64, i64);
impl_nonzero!(NonZeroI128, i128);
impl_nonzero!(NonZeroU16, u16);
impl_nonzero!(NonZeroU32, u32);
impl_nonzero!(NonZeroU64, u64);
impl_nonzero!(NonZeroU128, u128);

#[cfg(has_atomics)]
impl_primitive!(AtomicI16);
#[cfg(has_atomics)]
impl_primitive!(AtomicI32);
#[cfg(has_atomics_64)]
impl_primitive!(AtomicI64);
#[cfg(has_atomics)]
impl_primitive!(AtomicU16);
#[cfg(has_atomics)]
impl_primitive!(AtomicU32);
#[cfg(has_atomics_64)]
impl_primitive!(AtomicU64);
