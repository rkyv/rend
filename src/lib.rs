//! # rend
//!
//! rend is a library that provides endian-aware primitives for Rust.
//!
//! It's similar in design to [`simple_endian`](https://crates.io/crates/simple_endian), with the
//! exception that it's easily extendable to other endiannesses than the builtin `BigEndian` and
//! `LittleEndian`. It also has support for more builtin types such as atomics and nonzero integers.
//!
//! rend does not provide endian-aware types for types that are inherently endian-agnostic, such as
//! `bool` and `u8`. It also does not provide endian-aware types for types that have an
//! architecture-dependent size, such as `isize` and `usize`.
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

#![cfg_attr(not(feature = "std"), no_std)]

mod alias;
#[cfg(has_atomics)]
mod atomic;
mod endian;
mod impls;
#[cfg(feature = "validation")]
mod validation;

pub use alias::*;
#[cfg(has_atomics)]
pub use atomic::*;
pub use endian::*;

use core::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::{
        NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128, NonZeroU16, NonZeroU32, NonZeroU64,
        NonZeroU128
    },
};

/// A type that can convert between big-endian and little-endian.
pub trait Primitive: Copy {
    /// Swaps from little-endian to big-endian and vice-versa.
    fn swap_endian(self) -> Self;
}

/// A type that can convert between native endianness and a target endianness.
pub trait Endianness {
    /// Swaps from native-endian to target-endian and vice-versa.
    fn swap_native<T: Primitive>(value: T) -> T;
}

/// A type stored with a particular endianness
#[repr(transparent)]
pub struct Endian<T, E> {
    value: T,
    _phantom: PhantomData<E>,
}

impl<T: Primitive, E: Endianness> Endian<T, E> {
    /// Creates a new `Endian` from a native-endian value
    #[inline]
    pub fn new(native: T) -> Self {
        Self {
            value: E::swap_native(native),
            _phantom: PhantomData,
        }
    }

    /// Converts an `Endian` to a native-endian value
    #[inline]
    pub fn to_ne(self) -> T {
        E::swap_native(self.value)
    }

    #[inline]
    fn convert(&mut self) {
        self.value = E::swap_native(self.value);
    }
}

macro_rules! impl_unop {
    ($trait:ident, $fn:ident) => {
        impl<T: Primitive + core::ops::$trait, E: Endianness> core::ops::$trait for Endian<T, E> {
            type Output = <T as core::ops::$trait>::Output;

            #[inline]
            fn $fn(self) -> Self::Output {
                self.to_ne().$fn()
            }
        }
    }
}

macro_rules! impl_from {
    ($($prim:ty),*) => {
        impl<T: Primitive, E: Endianness> From<T> for Endian<T, E> {
            #[inline]
            fn from(value: T) -> Self {
                Self::new(value)
            }
        }
        $(
            impl<E: Endianness> From<Endian<$prim, E>> for $prim {
                #[inline]
                fn from(value: Endian<$prim, E>) -> Self {
                    value.to_ne()
                }
            }
        )*
    }
}

macro_rules! impl_binop {
    (@impl $trait:ident::$fn:ident($self:ident, $other:ident: $in:ty) -> $out:ty { $expr:expr }) => {
        impl<T: Primitive + ::core::ops::$trait<Output = T>, E: Endianness> ::core::ops::$trait<$in> for Endian<T, E> {
            type Output = $out;

            #[inline]
            fn $fn($self, $other: $in) -> Self::Output {
                $expr
            }
        }
    };
    (@prim [$prim:ty] $trait:ident::$fn:ident($self:ident, $other:ident: $in:ty) -> T { $expr:expr }) => {
        impl<E: Endianness> ::core::ops::$trait<$in> for $prim
        where
            $prim: ::core::ops::$trait,
        {
            type Output = $prim;

            #[inline]
            fn $fn($self, $other: $in) -> Self::Output {
                $expr
            }
        }
    };
    (@prims [$($prim:ty),*] $trait:ident::$fn:ident($self:ident, $other:ident: Endian<T, E>) -> T { $expr:expr }) => {
        $(
            impl_binop!(@prim [$prim]
                $trait::$fn($self, $other: Endian<$prim, E>) -> T { $expr }
            );
            impl_binop!(@prim [$prim]
                $trait::$fn($self, $other: &'_ Endian<$prim, E>) -> T { $expr }
            );
        )*
    };
    (@class int $trait:ident::$fn:ident($self:ident, $other:ident: Endian<T, E>) -> T { $expr:expr }) => {
        impl_binop!(@prims [i16, i32, i64, i128, u16, u32, u64, u128]
            $trait::$fn($self, $other: Endian<T, E>) -> T { $expr }
        );
    };
    (@class float $trait:ident::$fn:ident($self:ident, $other:ident: Endian<T, E>) -> T { $expr:expr }) => {
        impl_binop!(@prims [f32, f64]
            $trait::$fn($self, $other: Endian<T, E>) -> T { $expr }
        );
    };
    (@class nonzero $trait:ident::$fn:ident($self:ident, $other:ident: Endian<T, E>) -> T { $expr:expr }) => {
        impl_binop!(@prims [NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU128]
            $trait::$fn($self, $other: Endian<T, E>) -> T { $expr }
        );
    };
    ($trait:ident::$fn:ident) => {
        impl_binop!($trait::$fn [int, float, nonzero]);
    };
    ($trait:ident::$fn:ident [$($class:ident),*]) => {
        impl_binop!(@impl
            $trait::$fn(self, other: Endian<T, E>) -> T {
                self.to_ne().$fn(other.to_ne())
            }
        );
        impl_binop!(@impl
            $trait::$fn(self, other: &'_ Endian<T, E>) -> T {
                self.to_ne().$fn(other.to_ne())
            }
        );
        impl_binop!(@impl
            $trait::$fn(self, other: T) -> T {
                self.to_ne().$fn(other)
            }
        );
        impl_binop!(@impl
            $trait::$fn(self, other: &'_ T) -> T {
                self.to_ne().$fn(*other)
            }
        );
        $(
            impl_binop!(@class $class
                $trait::$fn(self, other: Endian<T, E>) -> T {
                    self.$fn(other.to_ne())
                }
            );
        )*
    };
}

macro_rules! impl_binassign {
    (@impl $trait:ident::$fn:ident($self:ident, $other:ident: $in:ty) { $stmt:stmt }) => {
        impl<T: Primitive + ::core::ops::$trait<T>, E: Endianness> ::core::ops::$trait<$in> for Endian<T, E> {
            #[inline]
            fn $fn(&mut $self, $other: $in) {
                $self.convert();
                $stmt
                $self.convert();
            }
        }
    };
    (@prims [$($prim:ty),*] $trait:ident::$fn:ident($self:ident, $other:ident: Endian<T, E>) { $stmt:stmt }) => {
        $(
            impl<E: Endianness> ::core::ops::$trait<Endian<$prim, E>> for $prim
            where
                $prim: ::core::ops::$trait,
            {
                #[inline]
                fn $fn(&mut $self, $other: Endian<$prim, E>) {
                    $stmt
                }
            }
        )*
    };
    (@class int $trait:ident::$fn:ident($self:ident, $other:ident: Endian<T, E>) { $stmt:stmt }) => {
        impl_binassign!(@prims [i16, i32, i64, i128, u16, u32, u64, u128]
            $trait::$fn($self, $other: Endian<T, E>) { $stmt }
        );
    };
    (@class float $trait:ident::$fn:ident($self:ident, $other:ident: Endian<T, E>) { $stmt:stmt }) => {
        impl_binassign!(@prims [f32, f64]
            $trait::$fn($self, $other: Endian<T, E>) { $stmt }
        );
    };
    (@class nonzero $trait:ident::$fn:ident($self:ident, $other:ident: Endian<T, E>) { $stmt:stmt }) => {
        impl_binassign!(@prims [NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU128]
            $trait::$fn($self, $other: Endian<T, E>) { $stmt }
        );
    };
    ($trait:ident::$fn:ident) => {
        impl_binassign!($trait::$fn [int, float, nonzero]);
    };
    ($trait:ident::$fn:ident [$($class:ident),*]) => {
        impl_binassign!(@impl
            $trait::$fn(self, other: Endian<T, E>) {
                self.value.$fn(other.to_ne())
            }
        );
        impl_binassign!(@impl
            $trait::$fn(self, other: &'_ Endian<T, E>) {
                self.value.$fn(other.to_ne())
            }
        );
        impl_binassign!(@impl
            $trait::$fn(self, other: T) {
                self.value.$fn(other)
            }
        );
        impl_binassign!(@impl
            $trait::$fn(self, other: &'_ T) {
                self.value.$fn(*other)
            }
        );
        $(
            impl_binassign!(@class $class
                $trait::$fn(self, other: Endian<T, E>) {
                    self.$fn(other.to_ne())
                }
            );
        )*
    };
}

macro_rules! impl_fmt {
    ($trait:ident) => {
        impl<T: Primitive + core::fmt::$trait, E: Endianness> core::fmt::$trait for Endian<T, E> {
            #[inline]
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                self.to_ne().fmt(f)
            }
        }
    }
}

impl_binop!(Add::add [int, float]);
impl_binassign!(AddAssign::add_assign [int, float]);
impl_fmt!(Binary);
impl_binop!(BitAnd::bitand [int]);
impl_binassign!(BitAndAssign::bitand_assign [int]);
impl_binop!(BitOr::bitor [int, nonzero]);
impl_binassign!(BitOrAssign::bitor_assign [int, nonzero]);
impl_binop!(BitXor::bitxor [int]);
impl_binassign!(BitXorAssign::bitxor_assign [int]);

impl<T: Clone, E> Clone for Endian<T, E> {
    #[inline]
    fn clone(&self) -> Self {
        Endian {
            value: self.value.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<T: Copy, E> Copy for Endian<T, E> {}

impl_fmt!(Debug);

impl<T: Primitive, E: Endianness> Default for Endian<T, E>
where
    T: Default,
{
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl_fmt!(Display);
impl_binop!(Div::div [int, float]);
impl_binassign!(DivAssign::div_assign [int, float]);

impl<T: Eq, E> Eq for Endian<T, E> {}

impl_from!(i16, i32, i64, i128, u16, u32, u64, u128, f32, f64, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU128);

impl<T: Primitive + Hash, E: Endianness> Hash for Endian<T, E> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_ne().hash(state);
    }
}

impl_fmt!(LowerExp);
impl_fmt!(LowerHex);
impl_binop!(Mul::mul [int, float]);
impl_binassign!(MulAssign::mul_assign [int, float]);
impl_unop!(Neg, neg);
impl_unop!(Not, not);
impl_fmt!(Octal);

impl<T: Primitive + Ord, E: Endianness> Ord for Endian<T, E> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.to_ne().cmp(&other.to_ne())
    }
}

impl<T: PartialEq, E> PartialEq for Endian<T, E> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

impl<T: Primitive + PartialEq, E: Endianness> PartialEq<T> for Endian<T, E> {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.to_ne().eq(other)
    }
}

impl<T: Primitive + PartialOrd, E: Endianness> PartialOrd for Endian<T, E> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.to_ne().partial_cmp(&other.to_ne())
    }
}

impl<T: Primitive + PartialOrd, E: Endianness> PartialOrd<T> for Endian<T, E> {
    #[inline]
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.to_ne().partial_cmp(other)
    }
}

impl<T: Primitive + core::iter::Product, E: Endianness> core::iter::Product for Endian<T, E> {
    #[inline]
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        Self::new(iter.map(|x| x.to_ne()).product())
    }
}

impl_binop!(Rem::rem [int, float]);
impl_binassign!(RemAssign::rem_assign [int, float]);
impl_binop!(Shl::shl [int]);
impl_binassign!(ShlAssign::shl_assign [int]);
impl_binop!(Shr::shr [int]);
impl_binassign!(ShrAssign::shr_assign [int]);
impl_binop!(Sub::sub [int, float]);
impl_binassign!(SubAssign::sub_assign [int, float]);

impl<T: Primitive + core::iter::Sum, E: Endianness> core::iter::Sum for Endian<T, E> {
    #[inline]
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        Self::new(iter.map(|x| x.to_ne()).sum())
    }
}

impl_fmt!(UpperExp);
impl_fmt!(UpperHex);

#[cfg(test)]
mod tests {
    use crate::*;
    use core::mem;

    #[test]
    fn endian_representation() {
        unsafe {
            // i16
            assert_eq!([0x01, 0x02], mem::transmute::<_, [u8; 2]>(i16_be::new(0x0102)));
            assert_eq!([0x02, 0x01], mem::transmute::<_, [u8; 2]>(i16_le::new(0x0102)));

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
            assert_eq!([0x01, 0x02], mem::transmute::<_, [u8; 2]>(u16_be::new(0x0102)));
            assert_eq!([0x02, 0x01], mem::transmute::<_, [u8; 2]>(u16_le::new(0x0102)));

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
            assert_eq!([0x01, 0x02], mem::transmute::<_, [u8; 2]>(AtomicU16_be::new(0x0102)));
            assert_eq!([0x02, 0x01], mem::transmute::<_, [u8; 2]>(AtomicU16_le::new(0x0102)));

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
