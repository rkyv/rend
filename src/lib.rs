//! # rend
//!
//! rend is a library that provides endian-aware primitives for Rust.
//!
//! It's similar in design to [`simple_endian`](https://crates.io/crates/simple_endian), but has
//! support for more builtin types such as atomics and nonzero integers. It also has support for
//! const functions since it does not rely on traits.
//!
//! rend does not provide endian-aware types for types that are inherently endian-agnostic, such as
//! `bool` and `u8`. It does not provide endian-aware types for types that have an
//! architecture-dependent size, such as `isize` and `usize`. It's also not extensible to custom
//! types.
//!
//! rend is intended to be used to build portable types that can be shared between different
//! architectures, especially with zero-copy deserialization.
//!
//! ## Example:
//!
//! ```
//! use rend::*;
//!
//! let little_int = i32_le::new(0x12345678);
//! // Internal representation is little-endian
//! assert_eq!([0x78, 0x56, 0x34, 0x12], unsafe { ::core::mem::transmute::<_, [u8; 4]>(little_int) });
//!
//! // Can also be made with `.into()`
//! let little_int: i32_le = 0x12345678.into();
//! // Still formats correctly
//! assert_eq!("305419896", format!("{}", little_int));
//! assert_eq!("0x12345678", format!("0x{:x}", little_int));
//!
//! let big_int = i32_be::new(0x12345678);
//! // Internal representation is big-endian
//! assert_eq!([0x12, 0x34, 0x56, 0x78], unsafe { ::core::mem::transmute::<_, [u8; 4]>(big_int) });
//!
//! // Can also be made with `.into()`
//! let big_int: i32_be = 0x12345678.into();
//! // Still formats correctly
//! assert_eq!("305419896", format!("{}", big_int));
//! assert_eq!("0x12345678", format!("0x{:x}", big_int));
//! ```

#[macro_use]
mod impl_struct;
#[cfg(feature = "validation")]
#[macro_use]
mod impl_validation;

use core::{
    hash::{Hash, Hasher},
    num::{NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU128},
};
#[cfg(has_atomics)]
use core::sync::atomic::{AtomicI16, AtomicI32, AtomicU16, AtomicU32, Ordering};
#[cfg(has_atomics_64)]
use core::sync::atomic::{AtomicI64, AtomicU64};

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct LittleEndian<T> {
    value: T,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct BigEndian<T> {
    value: T,
}

pub trait AtomicPrimitive {
    type Primitive;
}

pub type Primitive<T> = <T as AtomicPrimitive>::Primitive;

#[cfg(has_atomics)]
macro_rules! impl_atomic_primitive {
    ($ty:ident, $prim:ident) => {
        impl AtomicPrimitive for $ty {
            type Primitive = $prim;
        }
    }
}

#[cfg(has_atomics)]
impl_atomic_primitive!(AtomicI16, i16);
#[cfg(has_atomics)]
impl_atomic_primitive!(AtomicI32, i32);
#[cfg(has_atomics_64)]
impl_atomic_primitive!(AtomicI64, i64);
#[cfg(has_atomics)]
impl_atomic_primitive!(AtomicU16, u16);
#[cfg(has_atomics)]
impl_atomic_primitive!(AtomicU32, u32);
#[cfg(has_atomics_64)]
impl_atomic_primitive!(AtomicU64, u64);

macro_rules! swap_endian {
    (@LittleEndian $expr:expr) => {
        {
            #[cfg(target_endian = "little")]
            {
                $expr
            }
            #[cfg(target_endian = "big")]
            {
                $expr.swap_bytes()
            }
        }
    };
    (@BigEndian $expr:expr) => {
        {
            #[cfg(target_endian = "little")]
            {
                $expr.swap_bytes()
            }
            #[cfg(target_endian = "big")]
            {
                $expr
            }
        }
    };
}

macro_rules! swap_bytes {
    (@signed_int $endian:ident<$ne:ty> $value:expr) => {
        swap_endian!(@$endian $value)
    };
    (@unsigned_int $endian:ident<$ne:ty> $value:expr) => {
        swap_endian!(@$endian $value)
    };
    (@float $endian:ident<$ne:ty> $value:expr) => {
        <$ne>::from_bits(swap_endian!(@$endian $value.to_bits()))
    };
    (@char $endian:ident<$ne:ty> $value:expr) => {
        unsafe { ::core::char::from_u32_unchecked(swap_endian!(@$endian $value as u32)) }
    };
    (@nonzero $endian:ident<$ne:ty> $value:expr) => {
        unsafe { <$ne>::new_unchecked(swap_endian!(@$endian $value.get())) }
    };
    (@atomic $endian:ident<$ne:ty> $value:expr) => {
        swap_endian!(@$endian $value)
    };
}

macro_rules! impl_endian {
    (@$class:ident $ne:ty, $le:ident, $be:ident) => {
        impl_endian!(@$class LittleEndian<$ne> as $le);
        impl_endian!(@$class BigEndian<$ne> as $be);
    };
    (@$class:ident $endian:ident<$ne:ty> as $alias:ident) => {
        #[allow(non_camel_case_types)]
        pub type $alias = $endian<$ne>;
        impl_struct!(@$class $endian<$ne>);
        #[cfg(feature = "validation")]
        impl_validation!(@$class $endian: $te, $ne);
    };
}

impl_endian!(@signed_int i16, i16_le, i16_be);
impl_endian!(@signed_int i32, i32_le, i32_be);
impl_endian!(@signed_int i64, i64_le, i64_be);
impl_endian!(@signed_int i128, i128_le, i128_be);
impl_endian!(@unsigned_int u16, u16_le, u16_be);
impl_endian!(@unsigned_int u32, u32_le, u32_be);
impl_endian!(@unsigned_int u64, u64_le, u64_be);
impl_endian!(@unsigned_int u128, u128_le, u128_be);

impl_endian!(@float f32, f32_le, f32_be);
impl_endian!(@float f64, f64_le, f64_be);

impl_endian!(@char char, char_le, char_be);

impl_endian!(@nonzero NonZeroI16, NonZeroI16_le, NonZeroI16_be);
impl_endian!(@nonzero NonZeroI32, NonZeroI32_le, NonZeroI32_be);
impl_endian!(@nonzero NonZeroI64, NonZeroI64_le, NonZeroI64_be);
impl_endian!(@nonzero NonZeroI128, NonZeroI128_le, NonZeroI128_be);
impl_endian!(@nonzero NonZeroU16, NonZeroU16_le, NonZeroU16_be);
impl_endian!(@nonzero NonZeroU32, NonZeroU32_le, NonZeroU32_be);
impl_endian!(@nonzero NonZeroU64, NonZeroU64_le, NonZeroU64_be);
impl_endian!(@nonzero NonZeroU128, NonZeroU128_le, NonZeroU128_be);

#[cfg(has_atomics)]
impl_endian!(@atomic AtomicI16, AtomicI16_le, AtomicI16_be);
#[cfg(has_atomics)]
impl_endian!(@atomic AtomicI32, AtomicI32_le, AtomicI32_be);
#[cfg(has_atomics_64)]
impl_endian!(@atomic AtomicI64, AtomicI64_le, AtomicI64_be);
#[cfg(has_atomics)]
impl_endian!(@atomic AtomicU16, AtomicU16_le, AtomicU16_be);
#[cfg(has_atomics)]
impl_endian!(@atomic AtomicU32, AtomicU32_le, AtomicU32_be);
#[cfg(has_atomics_64)]
impl_endian!(@atomic AtomicU64, AtomicU64_le, AtomicU64_be);

#[cfg(test)]
mod tests {
    use crate::*;
    use core::mem;

    #[test]
    fn endian_representation() {
        unsafe {
            // i16
            assert_eq!(
                [0x01, 0x02],
                mem::transmute::<_, [u8; 2]>(i16_be::new(0x0102))
            );
            assert_eq!(
                [0x02, 0x01],
                mem::transmute::<_, [u8; 2]>(i16_le::new(0x0102))
            );

            // i32
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                mem::transmute::<_, [u8; 4]>(i32_be::new(0x01020304))
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                mem::transmute::<_, [u8; 4]>(i32_le::new(0x01020304))
            );

            // i64
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                mem::transmute::<_, [u8; 8]>(i64_be::new(0x0102030405060708))
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                mem::transmute::<_, [u8; 8]>(i64_le::new(0x0102030405060708))
            );

            // i128
            assert_eq!(
                [
                    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
                    0x0e, 0x0f, 0x10
                ],
                mem::transmute::<_, [u8; 16]>(i128_be::new(0x0102030405060708090a0b0c0d0e0f10))
            );
            assert_eq!(
                [
                    0x10, 0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04,
                    0x03, 0x02, 0x01
                ],
                mem::transmute::<_, [u8; 16]>(i128_le::new(0x0102030405060708090a0b0c0d0e0f10))
            );

            // u16
            assert_eq!(
                [0x01, 0x02],
                mem::transmute::<_, [u8; 2]>(u16_be::new(0x0102))
            );
            assert_eq!(
                [0x02, 0x01],
                mem::transmute::<_, [u8; 2]>(u16_le::new(0x0102))
            );

            // u32
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                mem::transmute::<_, [u8; 4]>(u32_be::new(0x01020304))
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                mem::transmute::<_, [u8; 4]>(u32_le::new(0x01020304))
            );

            // u64
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                mem::transmute::<_, [u8; 8]>(u64_be::new(0x0102030405060708))
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                mem::transmute::<_, [u8; 8]>(u64_le::new(0x0102030405060708))
            );

            // u128
            assert_eq!(
                [
                    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
                    0x0e, 0x0f, 0x10
                ],
                mem::transmute::<_, [u8; 16]>(u128_be::new(0x0102030405060708090a0b0c0d0e0f10))
            );
            assert_eq!(
                [
                    0x10, 0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04,
                    0x03, 0x02, 0x01
                ],
                mem::transmute::<_, [u8; 16]>(u128_le::new(0x0102030405060708090a0b0c0d0e0f10))
            );

            // f32
            assert_eq!(
                [0x40, 0x49, 0x0f, 0xd0],
                mem::transmute::<_, [u8; 4]>(f32_be::new(3.141590118408203125f32))
            );
            assert_eq!(
                [0xd0, 0x0f, 0x49, 0x40],
                mem::transmute::<_, [u8; 4]>(f32_le::new(3.141590118408203125f32))
            );

            // f64
            assert_eq!(
                [0x40, 0x09, 0x21, 0xfb, 0x4d, 0x12, 0xd8, 0x4a],
                mem::transmute::<_, [u8; 8]>(f64_be::new(3.1415926000000000684053702571f64))
            );
            assert_eq!(
                [0x4a, 0xd8, 0x12, 0x4d, 0xfb, 0x21, 0x09, 0x40],
                mem::transmute::<_, [u8; 8]>(f64_le::new(3.1415926000000000684053702571f64))
            );

            // char
            assert_eq!(
                [0x00, 0x01, 0xf3, 0x89],
                mem::transmute::<_, [u8; 4]>(char_be::new('🎉'))
            );
            assert_eq!(
                [0x89, 0xf3, 0x01, 0x00],
                mem::transmute::<_, [u8; 4]>(char_le::new('🎉'))
            );

            // AtomicU16
            assert_eq!(
                [0x01, 0x02],
                mem::transmute::<_, [u8; 2]>(AtomicU16_be::new(0x0102))
            );
            assert_eq!(
                [0x02, 0x01],
                mem::transmute::<_, [u8; 2]>(AtomicU16_le::new(0x0102))
            );

            // AtomicU32
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                mem::transmute::<_, [u8; 4]>(AtomicU32_be::new(0x01020304))
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                mem::transmute::<_, [u8; 4]>(AtomicU32_le::new(0x01020304))
            );

            // AtomicU64
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                mem::transmute::<_, [u8; 8]>(AtomicU64_be::new(0x0102030405060708))
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                mem::transmute::<_, [u8; 8]>(AtomicU64_le::new(0x0102030405060708))
            );
        }
    }
}
