//! # rend
//!
//! rend is a library that provides cross-platform primitives for Rust.
//!
//! rend does not provide cross-platform alternatives for types that are
//! inherently cross-platform, such as `bool` and `u8`. It also does not provide
//! cross-platform types for types that have an architecture-dependent size,
//! such as `isize` and `usize`. rend does not support custom types.
//!
//! rend is intended to be used to build portable types that can be shared
//! between different architectures, especially with zero-copy deserialization.
//!
//! ## Features
//!
//! - `validation`: Enables validation support through `bytecheck`
//!
//! ## Example:
//! ```
//! use rend::*;
//!
//! let little_int = i32_le::from_native(0x12345678);
//! // Internal representation is little-endian
//! assert_eq!(
//!     [0x78, 0x56, 0x34, 0x12],
//!     unsafe { ::core::mem::transmute::<_, [u8; 4]>(little_int) }
//! );
//!
//! // Can also be made with `.into()`
//! let little_int: i32_le = 0x12345678.into();
//! // Still formats correctly
//! assert_eq!("305419896", format!("{}", little_int));
//! assert_eq!("0x12345678", format!("0x{:x}", little_int));
//!
//! let big_int = i32_be::from_native(0x12345678);
//! // Internal representation is big-endian
//! assert_eq!(
//!     [0x12, 0x34, 0x56, 0x78],
//!     unsafe { ::core::mem::transmute::<_, [u8; 4]>(big_int) }
//! );
//!
//! // Can also be made with `.into()`
//! let big_int: i32_be = 0x12345678.into();
//! // Still formats correctly
//! assert_eq!("305419896", format!("{}", big_int));
//! assert_eq!("0x12345678", format!("0x{:x}", big_int));
//! ```

#![no_std]
#![deny(
    future_incompatible,
    missing_docs,
    nonstandard_style,
    unsafe_op_in_unsafe_fn,
    unused,
    warnings,
    clippy::all,
    clippy::missing_safety_doc,
    clippy::undocumented_unsafe_blocks,
    rustdoc::broken_intra_doc_links,
    rustdoc::missing_crate_level_docs
)]

#[macro_use]
mod traits;

#[cfg(target_has_atomic = "16")]
use core::sync::atomic::{AtomicI16, AtomicU16};
#[cfg(target_has_atomic = "32")]
use core::sync::atomic::{AtomicI32, AtomicU32};
#[cfg(target_has_atomic = "64")]
use core::sync::atomic::{AtomicI64, AtomicU64};
use core::{
    num::{
        NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroU128,
        NonZeroU16, NonZeroU32, NonZeroU64,
    },
    sync::atomic::Ordering,
};

macro_rules! match_endian {
    (native $native:expr, $little:expr, $big:expr $(,)?) => {
        $native
    };
    (little $native:expr, $little:expr, $big:expr $(,)?) => {
        $little
    };
    (big $native:expr, $little:expr, $big:expr $(,)?) => {
        $big
    };
}

macro_rules! if_native_endian {
    ($endian:ident $true:expr, $false:expr $(,)?) => {
        match_endian!(
            $endian
            $true,
            {
                #[cfg(target_endian = "little")]
                {
                    $true
                }
                #[cfg(target_endian = "big")]
                {
                    $false
                }
            },
            {
                #[cfg(target_endian = "little")]
                {
                    $false
                }
                #[cfg(target_endian = "big")]
                {
                    $true
                }
            },
        )
    }
}

macro_rules! swap_endian {
    ($endian:ident $expr:expr) => {
        if_native_endian!($endian $expr, $expr.swap_bytes())
    }
}

macro_rules! if_explicit_endian {
    ($endian:ident $true:expr, $false:expr $(,)?) => {
        match_endian!($endian $false, $true, $true)
    };
}

macro_rules! endian_name {
    ($endian:ident) => {
        match_endian!($endian "native", "little", "big")
    };
}

macro_rules! opposite_endian_name {
    ($endian:ident) => {
        match_endian!($endian "non-native", "big", "little")
    };
}

// `rustfmt` keeps changing the indentation of the attributes in this macro.
#[rustfmt::skip]
macro_rules! define_newtype {
    ($name:ident: $endian:ident $size_align:literal $prim:ty) => {
        #[allow(non_camel_case_types)]
        #[doc = concat!("A ",
            endian_name!($endian),
            "-endian `",
            stringify!($prim),
            "` with a guaranteed size and alignment of `",
            stringify!($size_align),
            "`.",
        )]
        #[repr(C, align($size_align))]
        pub struct $name($prim);
    };
}

macro_rules! define_integer {
    ($name:ident: $endian:ident $size_align:literal $prim:ty) => {
        define_newtype!($name: $endian $size_align $prim);

        impl $name {
            #[doc = concat!(
                "Returns a `",
                stringify!($name),
                "` containing `value`.",
            )]
            #[inline]
            pub const fn from_native(value: $prim) -> Self {
                Self(swap_endian!($endian value))
            }

            #[doc = concat!(
                "Returns a `",
                stringify!($prim),
                "` with the same value as `self`.",
            )]
            #[inline]
            pub const fn to_native(self) -> $prim {
                swap_endian!($endian self.0)
            }
        }
    };
}

macro_rules! define_signed_integer {
    ($name:ident: $endian:ident $size_align:literal $prim:ty) => {
        define_integer!($name: $endian $size_align $prim);

        // SAFETY: An impl of `CheckBytes` with a `check_bytes` function that is
        // a no-op is sound for signed integers.
        unsafe_impl_check_bytes_noop!(for $name);

        impl_binop!(Add::add for $name: $prim);
        impl_binassign!(AddAssign::add_assign for $name: $prim);
        impl_clone_and_copy!(for $name);
        impl_fmt!(Binary for $name);
        impl_binop!(BitAnd::bitand for $name: $prim);
        impl_binassign!(BitAndAssign::bitand_assign for $name: $prim);
        impl_binop!(BitOr::bitor for $name: $prim);
        impl_binassign!(BitOrAssign::bitor_assign for $name: $prim);
        impl_binop!(BitXor::bitxor for $name: $prim);
        impl_binassign!(BitXorAssign::bitxor_assign for $name: $prim);
        impl_fmt!(Debug for $name);
        impl_default!(for $name: $prim);
        impl_fmt!(Display for $name);
        impl_binop!(Div::div for $name: $prim);
        impl_binassign!(DivAssign::div_assign for $name: $prim);
        impl_from!(for $name: $prim);
        impl_hash!(for $name);
        impl_fmt!(LowerExp for $name);
        impl_fmt!(LowerHex for $name);
        impl_binop!(Mul::mul for $name: $prim);
        impl_binassign!(MulAssign::mul_assign for $name: $prim);
        impl_unop!(Neg::neg for $name: $prim);
        impl_unop!(Not::not for $name: $prim);
        impl_fmt!(Octal for $name);
        impl_ord!(for $name);
        impl_partial_eq_and_eq!(for $name: $prim);
        impl_partial_ord!(for $name: $prim);
        impl_product_and_sum!(for $name);
        impl_binop!(Rem::rem for $name: $prim);
        impl_binassign!(RemAssign::rem_assign for $name: $prim);
        impl_binop!(Shl::shl for $name: $prim);
        impl_binassign!(ShlAssign::shl_assign for $name: $prim);
        impl_binop!(Shr::shr for $name: $prim);
        impl_binassign!(ShrAssign::shr_assign for $name: $prim);
        impl_binop!(Sub::sub for $name: $prim);
        impl_binassign!(SubAssign::sub_assign for $name: $prim);
        impl_fmt!(UpperExp for $name);
        impl_fmt!(UpperHex for $name);
    };
}

macro_rules! define_signed_integers {
    ($(
        $ne:ident $le:ident $be:ident: $size_align:literal $prim:ty
    ),* $(,)?) => {
        $(
            define_signed_integer!($ne: native $size_align $prim);
            define_signed_integer!($le: little $size_align $prim);
            define_signed_integer!($be: big $size_align $prim);
        )*
    };
}

define_signed_integers! {
    i16_ne i16_le i16_be: 2 i16,
    i32_ne i32_le i32_be: 4 i32,
    i64_ne i64_le i64_be: 8 i64,
    i128_ne i128_le i128_be: 16 i128,
}

macro_rules! define_unsigned_integer {
    ($name:ident: $endian:ident $size_align:literal $prim:ty) => {
        define_integer!($name: $endian $size_align $prim);

        // SAFETY: An impl of `CheckBytes` with a `check_bytes` function that is
        // a no-op is sound for unsigned integers.
        unsafe_impl_check_bytes_noop!(for $name);

        impl_binop!(Add::add for $name: $prim);
        impl_binassign!(AddAssign::add_assign for $name: $prim);
        impl_clone_and_copy!(for $name);
        impl_fmt!(Binary for $name);
        impl_binop!(BitAnd::bitand for $name: $prim);
        impl_binassign!(BitAndAssign::bitand_assign for $name: $prim);
        impl_binop!(BitOr::bitor for $name: $prim);
        impl_binassign!(BitOrAssign::bitor_assign for $name: $prim);
        impl_binop!(BitXor::bitxor for $name: $prim);
        impl_binassign!(BitXorAssign::bitxor_assign for $name: $prim);
        impl_fmt!(Debug for $name);
        impl_default!(for $name: $prim);
        impl_fmt!(Display for $name);
        impl_binop!(Div::div for $name: $prim);
        impl_binassign!(DivAssign::div_assign for $name: $prim);
        impl_from!(for $name: $prim);
        impl_hash!(for $name);
        impl_fmt!(LowerExp for $name);
        impl_fmt!(LowerHex for $name);
        impl_binop!(Mul::mul for $name: $prim);
        impl_binassign!(MulAssign::mul_assign for $name: $prim);
        impl_unop!(Not::not for $name: $prim);
        impl_fmt!(Octal for $name);
        impl_ord!(for $name);
        impl_partial_eq_and_eq!(for $name: $prim);
        impl_partial_ord!(for $name: $prim);
        impl_product_and_sum!(for $name);
        impl_binop!(Rem::rem for $name: $prim);
        impl_binassign!(RemAssign::rem_assign for $name: $prim);
        impl_binop!(Shl::shl for $name: $prim);
        impl_binassign!(ShlAssign::shl_assign for $name: $prim);
        impl_binop!(Shr::shr for $name: $prim);
        impl_binassign!(ShrAssign::shr_assign for $name: $prim);
        impl_binop!(Sub::sub for $name: $prim);
        impl_binassign!(SubAssign::sub_assign for $name: $prim);
        impl_fmt!(UpperExp for $name);
        impl_fmt!(UpperHex for $name);
    };
}

macro_rules! define_unsigned_integers {
    ($(
        $ne:ident $le:ident $be:ident: $size_align:literal $prim:ty
    ),* $(,)?) => {
        $(
            define_unsigned_integer!($ne: native $size_align $prim);
            define_unsigned_integer!($le: little $size_align $prim);
            define_unsigned_integer!($be: big $size_align $prim);
        )*
    };
}

define_unsigned_integers! {
    u16_ne u16_le u16_be: 2 u16,
    u32_ne u32_le u32_be: 4 u32,
    u64_ne u64_le u64_be: 8 u64,
    u128_ne u128_le u128_be: 16 u128,
}

macro_rules! define_float {
    ($name:ident: $endian:ident $size_align:literal $prim:ty, $prim_int:ty) => {
        define_newtype!($name: $endian $size_align $prim);

        impl $name {
            #[doc = concat!(
                "Returns a `",
                stringify!($name),
                "` containing `value`.",
            )]
            #[inline]
            pub const fn from_native(value: $prim) -> Self {
                use core::mem::transmute;

                // `transmute` is used here because `from_bits` and `to_bits`
                // are not stably const as of 1.72.0.

                // SAFETY: `$prim` and `$prim_int` have the same size and all
                // bit patterns are valid for both.
                let value = unsafe { transmute::<$prim, $prim_int>(value) };
                let value = swap_endian!($endian value);
                // SAFETY: `$prim` and `$prim_int` have the same size and all
                // bit patterns are valid for both.
                let value = unsafe { transmute::<$prim_int, $prim>(value) };
                Self(value)
            }

            #[doc = concat!(
                "Returns a `",
                stringify!($prim),
                "` with the same value as `self`.",
            )]
            #[inline]
            pub const fn to_native(self) -> $prim {
                use core::mem::transmute;

                // `transmute` is used here because `from_bits` and `to_bits`
                // are not stably const as of 1.72.0.

                // SAFETY: `$prim` and `$prim_int` have the same size and all
                // bit patterns are valid for both.
                let value = unsafe { transmute::<$prim, $prim_int>(self.0) };
                let value = swap_endian!($endian value);
                // SAFETY: `$prim` and `$prim_int` have the same size and all
                // bit patterns are valid for both.
                unsafe { transmute::<$prim_int, $prim>(value) }
            }
        }

        // SAFETY: An impl of `CheckBytes` with a `check_bytes` function that is
        // a no-op is sound for floats.
        unsafe_impl_check_bytes_noop!(for $name);

        impl_binop!(Add::add for $name: $prim);
        impl_binassign!(AddAssign::add_assign for $name: $prim);
        impl_clone_and_copy!(for $name);
        impl_fmt!(Debug for $name);
        impl_default!(for $name: $prim);
        impl_fmt!(Display for $name);
        impl_binop!(Div::div for $name: $prim);
        impl_binassign!(DivAssign::div_assign for $name: $prim);
        impl_from!(for $name: $prim);
        impl_fmt!(LowerExp for $name);
        impl_binop!(Mul::mul for $name: $prim);
        impl_binassign!(MulAssign::mul_assign for $name: $prim);
        impl_unop!(Neg::neg for $name: $prim);
        impl_partial_eq_and_eq!(for $name: $prim);
        impl_partial_ord!(for $name: $prim);
        impl_product_and_sum!(for $name);
        impl_binop!(Rem::rem for $name: $prim);
        impl_binassign!(RemAssign::rem_assign for $name: $prim);
        impl_binop!(Sub::sub for $name: $prim);
        impl_binassign!(SubAssign::sub_assign for $name: $prim);
        impl_fmt!(UpperExp for $name);
    };
}

macro_rules! define_floats {
    ($(
        $ne:ident $le:ident $be:ident:
        $size_align:literal $prim:ty as $prim_int:ty
    ),* $(,)?) => {
        $(
            define_float!($ne: native $size_align $prim, $prim_int);
            define_float!($le: little $size_align $prim, $prim_int);
            define_float!($be: big $size_align $prim, $prim_int);
        )*
    };
}

define_floats! {
    f32_ne f32_le f32_be: 4 f32 as u32,
    f64_ne f64_le f64_be: 8 f64 as u64,
}

macro_rules! define_char {
    ($name:ident: $endian:ident) => {
        define_newtype!($name: $endian 4 u32);

        impl $name {
            #[doc = concat!(
                "Returns a `",
                stringify!($name),
                "` containing `value`.",
            )]
            #[inline]
            pub const fn from_native(value: char) -> Self {
                Self(swap_endian!($endian value as u32))
            }

            #[doc = concat!(
                "Returns a `",
                stringify!($prim),
                "` with the same value as `self`.",
            )]
            #[inline]
            pub const fn to_native(self) -> char {
                use core::mem::transmute;

                // `transmute` is used here because `from_u32_unchecked` is not
                // stably const as of 1.72.0.

                // SAFETY: `u32` and `char` have the same size and it is an
                // invariant of this type that it contains a valid `char` when
                // swapped to native endianness.
                unsafe { transmute::<u32, char>(swap_endian!($endian self.0)) }
            }
        }

        impl_clone_and_copy!(for $name);
        impl_fmt!(Debug for $name);
        impl_default!(for $name: char);
        impl_fmt!(Display for $name);
        impl_from!(for $name: char);
        impl_hash!(for $name);
        impl_ord!(for $name);
        impl_partial_eq_and_eq!(for $name: char);
        impl_partial_ord!(for $name: char);

        #[cfg(feature = "validation")]
        impl<C: ?Sized> bytecheck::CheckBytes<C> for $name {
            type Error = bytecheck::CharCheckError;

            #[inline]
            unsafe fn check_bytes<'a>(
                value: *const Self,
                context: &mut C,
            ) -> Result<&'a Self, Self::Error> {
                // SAFETY: `value` points to a `Self`, which has the same size
                // as a `u32` and is at least as aligned as one.
                let u = unsafe { *u32::check_bytes(value.cast(), context)? };
                let c = swap_endian!($endian u);
                let _ = char::from_u32(c)
                    .ok_or_else(|| bytecheck::CharCheckError {
                        invalid_value: c,
                    })?;
                // SAFETY: We have verified that `value` contains a valid `char`
                // when swapped to native endianness, and so is a valid `Self`.
                Ok(unsafe { &*value })
            }
        }
    };
}

define_char!(char_ne: native);
define_char!(char_le: little);
define_char!(char_be: big);

macro_rules! define_nonzero {
    (
        $name:ident:
        $endian:ident $size_align:literal $prim:ty as $prim_int:ty
    ) => {
        define_newtype!($name: $endian $size_align $prim);

        impl $name {
            /// Creates a non-zero if the given value is not zero.
            #[inline]
            pub const fn new(value: $prim_int) -> Option<Self> {
                if value != 0 {
                    // SAFETY: `value` is not zero.
                    Some(unsafe { Self::new_unchecked(value) })
                } else {
                    None
                }
            }

            /// Creates a non-zero without checking whether it is non-zero. This
            /// results in undefined behavior if the value is zero.
            ///
            /// # Safety
            ///
            /// The value must not be zero.
            #[inline]
            pub const unsafe fn new_unchecked(value: $prim_int) -> Self {
                // SAFETY: The caller has guaranteed that `value` is not zero.
                unsafe {
                    Self(<$prim>::new_unchecked(swap_endian!($endian value)))
                }
            }

            /// Returns the value as a primitive type.
            #[inline]
            pub const fn get(self) -> $prim_int {
                swap_endian!($endian self.0.get())
            }

            #[doc = concat!(
                "Returns a `",
                stringify!($name),
                "` containing `value`.",
            )]
            #[inline]
            pub const fn from_native(value: $prim) -> Self {
                // SAFETY: `value` is a non-zero integer and so `value.get()`
                // cannot return zero.
                unsafe { Self::new_unchecked(value.get()) }
            }

            #[doc = concat!(
                "Returns a `",
                stringify!($prim),
                "` with the same value as `self`.",
            )]
            #[inline]
            pub const fn to_native(self) -> $prim {
                // SAFETY: `self` is a non-zero integer and so `self.get()`
                // cannot return zero.
                unsafe { <$prim>::new_unchecked(self.get()) }
            }
        }

        impl_clone_and_copy!(for $name);
        impl_fmt!(Binary for $name);
        impl_binop_nonzero!(BitOr::bitor for $name: $prim);
        impl_binassign_nonzero!(BitOrAssign::bitor_assign for $name: $prim);
        impl_fmt!(Debug for $name);
        impl_fmt!(Display for $name);
        impl_from!(for $name: $prim);
        impl_hash!(for $name);
        impl_fmt!(LowerHex for $name);
        impl_fmt!(Octal for $name);
        impl_ord!(for $name);
        impl_partial_eq_and_eq!(for $name: $prim);
        impl_partial_ord!(for $name: $prim);
        impl_fmt!(UpperHex for $name);

        #[cfg(feature = "validation")]
        impl<C: ?Sized> bytecheck::CheckBytes<C> for $name {
            type Error = bytecheck::NonZeroCheckError;

            #[inline]
            unsafe fn check_bytes<'a>(
                value: *const Self,
                context: &mut C,
            ) -> Result<&'a Self, Self::Error> {
                // SAFETY: `value` points to a `Self`, which has the same size
                // as a `$prim_int` and is at least as aligned as one.
                let x = unsafe {
                    *<$prim_int>::check_bytes(value.cast(), context)?
                };

                // Note: the bit pattern for 0 is always the same regardless
                // of endianness.
                if x != 0 {
                    // SAFETY: We have verified that `*value` is not zero.
                    Ok(unsafe { &*value })
                } else {
                    Err(bytecheck::NonZeroCheckError::IsZero)
                }
            }
        }
    };
}

macro_rules! define_nonzeros {
    ($(
        $ne:ident $le:ident $be:ident:
        $size_align:literal $prim:ty as $prim_int:ty
    ),* $(,)?) => {
        $(
            define_nonzero!($ne: native $size_align $prim as $prim_int);
            define_nonzero!($le: little $size_align $prim as $prim_int);
            define_nonzero!($be: big $size_align $prim as $prim_int);
        )*
    }
}

define_nonzeros! {
    NonZeroI16_ne NonZeroI16_le NonZeroI16_be: 2 NonZeroI16 as i16,
    NonZeroI32_ne NonZeroI32_le NonZeroI32_be: 4 NonZeroI32 as i32,
    NonZeroI64_ne NonZeroI64_le NonZeroI64_be: 8 NonZeroI64 as i64,
    NonZeroI128_ne NonZeroI128_le NonZeroI128_be: 16 NonZeroI128 as i128,
    NonZeroU16_ne NonZeroU16_le NonZeroU16_be: 2 NonZeroU16 as u16,
    NonZeroU32_ne NonZeroU32_le NonZeroU32_be: 4 NonZeroU32 as u32,
    NonZeroU64_ne NonZeroU64_le NonZeroU64_be: 8 NonZeroU64 as u64,
    NonZeroU128_ne NonZeroU128_le NonZeroU128_be: 16 NonZeroU128 as u128,
}

#[allow(dead_code)]
const fn fetch_ordering(order: Ordering) -> Ordering {
    match order {
        Ordering::Relaxed => Ordering::Relaxed,
        Ordering::Release => Ordering::Relaxed,
        Ordering::Acquire => Ordering::Acquire,
        Ordering::AcqRel => Ordering::Acquire,
        Ordering::SeqCst => Ordering::SeqCst,
        order => order,
    }
}

macro_rules! define_atomic {
    (
        $name:ident:
        $endian:ident $size_align:literal $prim:ty as $prim_int:ty
    ) => {
        define_newtype!($name: $endian $size_align $prim);

        impl $name {
            #[doc = concat!(
                "Returns a `",
                stringify!($name),
                "` containing `value`.",
            )]
            #[inline]
            pub const fn new(value: $prim_int) -> Self {
                Self(<$prim>::new(swap_endian!($endian value)))
            }
        }

        // SAFETY: An impl of `CheckBytes` with a `check_bytes` function that is
        // a no-op is sound for atomic integers.
        unsafe_impl_check_bytes_noop!(for $name);

        impl $name {
            /// Stores a value into the atomic integer if the current value is
            /// the same as the `current` value.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::compare_exchange`] for more information.",
            )]
            #[inline]
            pub fn compare_exchange(
                &self,
                current: $prim_int,
                new: $prim_int,
                success: Ordering,
                failure: Ordering,
            ) -> Result<$prim_int, $prim_int> {
                match self.0.compare_exchange(
                    swap_endian!($endian current),
                    swap_endian!($endian new),
                    success,
                    failure,
                ) {
                    Ok(x) => Ok(swap_endian!($endian x)),
                    Err(x) => Err(swap_endian!($endian x)),
                }
            }

            /// Stores a value into the atomic integer if the current value is
            /// the same as the `current` value.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::compare_exchange_weak`] for more information.",
            )]
            #[inline]
            pub fn compare_exchange_weak(
                &self,
                current: $prim_int,
                new: $prim_int,
                success: Ordering,
                failure: Ordering,
            ) -> Result<$prim_int, $prim_int> {
                match self.0.compare_exchange_weak(
                    swap_endian!($endian current),
                    swap_endian!($endian new),
                    success,
                    failure,
                ) {
                    Ok(x) => Ok(swap_endian!($endian x)),
                    Err(x) => Ok(swap_endian!($endian x)),
                }
            }

            /// Adds to the current value, returning the previous value.
            ///
            #[doc = if_explicit_endian!(
                $endian
                concat!(
                    "Because addition is not an endian-agnostic operation, ",
                    "`fetch_add` is implemented in terms of [`",
                    stringify!($prim),
                    "::compare_exchange_weak`] on ",
                    opposite_endian_name!($endian),
                    "-endian targets. This may result in worse performance on ",
                    "those targets.",
                ),
                "",
            )]
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_add`] for more information.",
            )]
            #[inline]
            pub fn fetch_add(
                &self,
                val: $prim_int,
                order: Ordering,
            ) -> $prim_int {
                if_native_endian!(
                    $endian
                    self.0.fetch_add(val, order),
                    self.fetch_update_fast(
                        order,
                        fetch_ordering(order),
                        |x| x + val,
                    ),
                )
            }

            /// Bitwise "and" with the current value.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_and`] for more information.",
            )]
            #[inline]
            pub fn fetch_and(
                &self,
                val: $prim_int,
                order: Ordering,
            ) -> $prim_int {
                let val = swap_endian!($endian val);
                swap_endian!($endian self.0.fetch_and(val, order))
            }

            /// Maximum with the current value.
            ///
            #[doc = if_explicit_endian!(
                $endian
                concat!(
                    "Because maximum is not an endian-agnostic operation, ",
                    "`fetch_max` is implemented in terms of [`",
                    stringify!($prim),
                    "::compare_exchange_weak`] on ",
                    opposite_endian_name!($endian),
                    "-endian targets. This may result in worse performance on ",
                    "those targets.",
                ),
                "",
            )]
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_max`] for more information.",
            )]
            #[inline]
            pub fn fetch_max(
                &self,
                val: $prim_int,
                order: Ordering,
            ) -> $prim_int {
                if_native_endian!(
                    $endian
                    self.0.fetch_max(val, order),
                    self.fetch_update_fast(
                        order,
                        fetch_ordering(order),
                        |x| <$prim_int>::max(x, val),
                    ),
                )
            }

            /// Minimum with the current value.
            ///
            #[doc = if_explicit_endian!(
                $endian
                concat!(
                    "Because minimum is not an endian-agnostic operation, ",
                    "`fetch_min` is implemented in terms of [`",
                    stringify!($prim),
                    "::compare_exchange_weak`] on ",
                    opposite_endian_name!($endian),
                    "-endian targets. This may result in worse performance on ",
                    "those targets.",
                ),
                "",
            )]
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_min`] for more information.",
            )]
            #[inline]
            pub fn fetch_min(
                &self,
                val: $prim_int,
                order: Ordering,
            ) -> $prim_int {
                if_native_endian!(
                    $endian
                    self.0.fetch_min(val, order),
                    self.fetch_update_fast(
                        order,
                        fetch_ordering(order),
                        |x| <$prim_int>::min(x, val),
                    ),
                )
            }

            /// Bitwise "nand" with the current value.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_nand`] for more information.",
            )]
            #[inline]
            pub fn fetch_nand(
                &self,
                val: $prim_int,
                order: Ordering,
            ) -> $prim_int {
                let val = swap_endian!($endian val);
                swap_endian!($endian self.0.fetch_nand(val, order))
            }

            /// Bitwise "or" with the current value.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_or`] for more information.",
            )]
            #[inline]
            pub fn fetch_or(
                &self,
                val: $prim_int,
                order: Ordering,
            ) -> $prim_int {
                let val = swap_endian!($endian val);
                swap_endian!($endian self.0.fetch_or(val, order))
            }

            /// Subtracts from the current value, returning the previous value.
            ///
            #[doc = if_explicit_endian!(
                $endian
                concat!(
                    "Because subtraction is not an endian-agnostic operation, ",
                    "`fetch_sub` is implemented in terms of [`",
                    stringify!($prim),
                    "::compare_exchange_weak`] on ",
                    opposite_endian_name!($endian),
                    "-endian targets. This may result in worse performance on ",
                    "those targets.",
                ),
                "",
            )]
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_sub`] for more information.",
            )]
            #[inline]
            pub fn fetch_sub(
                &self,
                val: $prim_int,
                order: Ordering,
            ) -> $prim_int {
                if_native_endian!(
                    $endian
                    self.0.fetch_sub(val, order),
                    self.fetch_update_fast(
                        order,
                        fetch_ordering(order),
                        |x| x - val,
                    ),
                )
            }

            #[allow(dead_code)]
            #[inline(always)]
            fn fetch_update_fast<F: Fn($prim_int) -> $prim_int>(
                &self,
                set_order: Ordering,
                fetch_order: Ordering,
                f: F,
            ) -> $prim_int {
                let mut prev = swap_endian!($endian self.0.load(fetch_order));
                loop {
                    let next = swap_endian!($endian f(prev));
                    match self.0.compare_exchange_weak(
                        prev,
                        next,
                        set_order,
                        fetch_order,
                    ) {
                        Ok(x) => break x,
                        Err(next_prev) => {
                            prev = swap_endian!($endian next_prev);
                        }
                    }
                }
            }

            /// Fetches the value, and applies a function to it that returns an
            /// optional new value. Returns a `Result` of `Ok(previous_value)`
            /// if the function returned `Some(_)`, else `Err(previous_value)`.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_update`] for more information.",
            )]
            #[inline]
            pub fn fetch_update<F: FnMut($prim_int) -> Option<$prim_int>>(
                &self,
                set_order: Ordering,
                fetch_order: Ordering,
                mut f: F,
            ) -> Result<$prim_int, $prim_int> {
                self.0.fetch_update(set_order, fetch_order, |x| {
                    f(swap_endian!($endian x)).map(|y| swap_endian!($endian y))
                })
            }

            /// Bitwise "xor" with the current value.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::fetch_xor`] for more information.",
            )]
            #[inline]
            pub fn fetch_xor(
                &self,
                val: $prim_int,
                order: Ordering,
            ) -> $prim_int {
                let val = swap_endian!($endian val);
                swap_endian!($endian self.0.fetch_xor(val, order))
            }

            /// Consumes the atomic and returns the contained value.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::into_inner`] for more information.",
            )]
            #[inline]
            pub fn into_inner(self) -> $prim_int {
                swap_endian!($endian self.0.into_inner())
            }

            /// Loads a value from the atomic integer.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::load`] for more information.",
            )]
            #[inline]
            pub fn load(&self, order: Ordering) -> $prim_int {
                swap_endian!($endian self.0.load(order))
            }

            /// Stores a value into the atomic integer.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::store`] for more information.",
            )]
            #[inline]
            pub fn store(&self, val: $prim_int, order: Ordering) {
                self.0.store(swap_endian!($endian val), order);
            }

            /// Stores a value into the atomic integer, returning the previous
            /// value.
            ///
            #[doc = concat!(
                "See [`",
                stringify!($prim),
                "::swap`] for more information.",
            )]
            #[inline]
            pub fn swap(&self, val: $prim_int, order: Ordering) -> $prim_int {
                let val = swap_endian!($endian val);
                swap_endian!($endian self.0.swap(val, order))
            }
        }

        impl core::fmt::Debug for $name {
            #[inline]
            fn fmt(
                &self,
                f: &mut core::fmt::Formatter<'_>,
            ) -> core::fmt::Result {
                swap_endian!($endian self.load(Ordering::Relaxed)).fmt(f)
            }
        }

        impl Default for $name {
            #[inline]
            fn default() -> Self {
                Self::new(<$prim_int>::default())
            }
        }

        impl From<$prim_int> for $name {
            #[inline]
            fn from(value: $prim_int) -> Self {
                Self::new(value)
            }
        }
    }
}

macro_rules! define_atomics {
    ($(
        $ne:ident $le:ident $be:ident:
        $size_align:literal $prim:ty as $prim_int:ty
    ),* $(,)?) => {
        $(
            define_atomic!($ne: native $size_align $prim as $prim_int);
            define_atomic!($le: little $size_align $prim as $prim_int);
            define_atomic!($be: big $size_align $prim as $prim_int);
        )*
    }
}

#[cfg(target_has_atomic = "16")]
define_atomics! {
    AtomicI16_ne AtomicI16_le AtomicI16_be: 2 AtomicI16 as i16,
    AtomicU16_ne AtomicU16_le AtomicU16_be: 2 AtomicU16 as u16,
}

#[cfg(target_has_atomic = "32")]
define_atomics! {
    AtomicI32_ne AtomicI32_le AtomicI32_be: 4 AtomicI32 as i32,
    AtomicU32_ne AtomicU32_le AtomicU32_be: 4 AtomicU32 as u32,
}

#[cfg(target_has_atomic = "64")]
define_atomics! {
    AtomicI64_ne AtomicI64_le AtomicI64_be: 8 AtomicI64 as i64,
    AtomicU64_ne AtomicU64_le AtomicU64_be: 8 AtomicU64 as u64,
}

#[cfg(test)]
mod tests {
    use crate::*;
    use core::mem::transmute;

    #[test]
    fn primitives() {
        unsafe {
            // i16
            assert_eq!(
                transmute::<_, [u8; 2]>(0x0102i16),
                transmute::<_, [u8; 2]>(i16_ne::from_native(0x0102)),
            );
            assert_eq!(
                [0x02, 0x01],
                transmute::<_, [u8; 2]>(i16_le::from_native(0x0102)),
            );
            assert_eq!(
                [0x01, 0x02],
                transmute::<_, [u8; 2]>(i16_be::from_native(0x0102)),
            );

            // i32
            assert_eq!(
                transmute::<_, [u8; 4]>(0x01020304i32),
                transmute::<_, [u8; 4]>(i32_ne::from_native(0x01020304)),
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 4]>(i32_le::from_native(0x01020304)),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                transmute::<_, [u8; 4]>(i32_be::from_native(0x01020304)),
            );

            // i64
            assert_eq!(
                transmute::<_, [u8; 8]>(0x0102030405060708i64),
                transmute::<_, [u8; 8]>(i64_ne::from_native(
                    0x0102030405060708
                )),
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 8]>(i64_le::from_native(
                    0x0102030405060708
                )),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                transmute::<_, [u8; 8]>(i64_be::from_native(
                    0x0102030405060708
                )),
            );

            // i128
            assert_eq!(
                transmute::<_, [u8; 16]>(
                    0x0102030405060708090a0b0c0d0e0f10i128
                ),
                transmute::<_, [u8; 16]>(i128_ne::from_native(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
            assert_eq!(
                [
                    0x10, 0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07,
                    0x06, 0x05, 0x04, 0x03, 0x02, 0x01
                ],
                transmute::<_, [u8; 16]>(i128_le::from_native(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
            assert_eq!(
                [
                    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a,
                    0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10
                ],
                transmute::<_, [u8; 16]>(i128_be::from_native(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );

            // u16
            assert_eq!(
                transmute::<_, [u8; 2]>(0x0102u16),
                transmute::<_, [u8; 2]>(u16_ne::from_native(0x0102)),
            );
            assert_eq!(
                [0x02, 0x01],
                transmute::<_, [u8; 2]>(u16_le::from_native(0x0102)),
            );
            assert_eq!(
                [0x01, 0x02],
                transmute::<_, [u8; 2]>(u16_be::from_native(0x0102)),
            );

            // u32
            assert_eq!(
                transmute::<_, [u8; 4]>(0x01020304u32),
                transmute::<_, [u8; 4]>(u32_ne::from_native(0x01020304)),
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 4]>(u32_le::from_native(0x01020304)),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                transmute::<_, [u8; 4]>(u32_be::from_native(0x01020304)),
            );

            // u64
            assert_eq!(
                transmute::<_, [u8; 8]>(0x0102030405060708u64),
                transmute::<_, [u8; 8]>(u64_ne::from_native(
                    0x0102030405060708
                )),
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 8]>(u64_le::from_native(
                    0x0102030405060708
                )),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                transmute::<_, [u8; 8]>(u64_be::from_native(
                    0x0102030405060708
                )),
            );

            // u128
            assert_eq!(
                transmute::<_, [u8; 16]>(
                    0x0102030405060708090a0b0c0d0e0f10u128
                ),
                transmute::<_, [u8; 16]>(u128_ne::from_native(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
            assert_eq!(
                [
                    0x10, 0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07,
                    0x06, 0x05, 0x04, 0x03, 0x02, 0x01
                ],
                transmute::<_, [u8; 16]>(u128_le::from_native(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
            assert_eq!(
                [
                    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a,
                    0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10
                ],
                transmute::<_, [u8; 16]>(u128_be::from_native(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );

            // f32
            assert_eq!(
                transmute::<_, [u8; 4]>(core::f32::consts::PI),
                transmute::<_, [u8; 4]>(f32_ne::from_native(
                    core::f32::consts::PI
                )),
            );
            assert_eq!(
                [0xdb, 0x0f, 0x49, 0x40],
                transmute::<_, [u8; 4]>(f32_le::from_native(
                    core::f32::consts::PI
                )),
            );
            assert_eq!(
                [0x40, 0x49, 0x0f, 0xdb],
                transmute::<_, [u8; 4]>(f32_be::from_native(
                    core::f32::consts::PI
                )),
            );

            // f64
            assert_eq!(
                transmute::<_, [u8; 8]>(core::f64::consts::PI),
                transmute::<_, [u8; 8]>(f64_ne::from_native(
                    core::f64::consts::PI
                )),
            );
            assert_eq!(
                [0x18, 0x2d, 0x44, 0x54, 0xfb, 0x21, 0x09, 0x40],
                transmute::<_, [u8; 8]>(f64_le::from_native(
                    core::f64::consts::PI
                )),
            );
            assert_eq!(
                [0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18],
                transmute::<_, [u8; 8]>(f64_be::from_native(
                    core::f64::consts::PI
                )),
            );

            // char
            assert_eq!(
                transmute::<_, [u8; 4]>('🎉'),
                transmute::<_, [u8; 4]>(char_ne::from_native('🎉')),
            );
            assert_eq!(
                [0x89, 0xf3, 0x01, 0x00],
                transmute::<_, [u8; 4]>(char_le::from_native('🎉')),
            );
            assert_eq!(
                [0x00, 0x01, 0xf3, 0x89],
                transmute::<_, [u8; 4]>(char_be::from_native('🎉')),
            );
        }
    }

    #[test]
    fn non_zero() {
        unsafe {
            // NonZeroI16
            assert_eq!(
                transmute::<_, [u8; 2]>(NonZeroI16::new_unchecked(0x0102)),
                transmute::<_, [u8; 2]>(NonZeroI16_ne::new_unchecked(0x0102)),
            );
            assert_eq!(
                [0x02, 0x01],
                transmute::<_, [u8; 2]>(NonZeroI16_le::new_unchecked(0x0102)),
            );
            assert_eq!(
                [0x01, 0x02],
                transmute::<_, [u8; 2]>(NonZeroI16_be::new_unchecked(0x0102)),
            );

            // NonZeroI32
            assert_eq!(
                transmute::<_, [u8; 4]>(NonZeroI32::new_unchecked(0x01020304)),
                transmute::<_, [u8; 4]>(NonZeroI32_ne::new_unchecked(
                    0x01020304
                )),
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 4]>(NonZeroI32_le::new_unchecked(
                    0x01020304
                )),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                transmute::<_, [u8; 4]>(NonZeroI32_be::new_unchecked(
                    0x01020304
                )),
            );

            // NonZeroI64
            assert_eq!(
                transmute::<_, [u8; 8]>(NonZeroI64::new_unchecked(
                    0x0102030405060708
                )),
                transmute::<_, [u8; 8]>(NonZeroI64_ne::new_unchecked(
                    0x0102030405060708
                )),
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 8]>(NonZeroI64_le::new_unchecked(
                    0x0102030405060708
                )),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                transmute::<_, [u8; 8]>(NonZeroI64_be::new_unchecked(
                    0x0102030405060708
                )),
            );

            // NonZeroI128
            assert_eq!(
                transmute::<_, [u8; 16]>(NonZeroI128::new_unchecked(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
                transmute::<_, [u8; 16]>(NonZeroI128_ne::new_unchecked(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
            assert_eq!(
                [
                    0x10, 0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07,
                    0x06, 0x05, 0x04, 0x03, 0x02, 0x01
                ],
                transmute::<_, [u8; 16]>(NonZeroI128_le::new_unchecked(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
            assert_eq!(
                [
                    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a,
                    0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10
                ],
                transmute::<_, [u8; 16]>(NonZeroI128_be::new_unchecked(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );

            // NonZeroU16
            assert_eq!(
                transmute::<_, [u8; 2]>(NonZeroU16::new_unchecked(0x0102)),
                transmute::<_, [u8; 2]>(NonZeroU16_ne::new_unchecked(0x0102)),
            );
            assert_eq!(
                [0x02, 0x01],
                transmute::<_, [u8; 2]>(NonZeroU16_le::new_unchecked(0x0102)),
            );
            assert_eq!(
                [0x01, 0x02],
                transmute::<_, [u8; 2]>(NonZeroU16_be::new_unchecked(0x0102)),
            );

            // NonZeroU32
            assert_eq!(
                transmute::<_, [u8; 4]>(NonZeroU32::new_unchecked(0x01020304)),
                transmute::<_, [u8; 4]>(NonZeroU32_ne::new_unchecked(
                    0x01020304
                )),
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 4]>(NonZeroU32_le::new_unchecked(
                    0x01020304
                )),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                transmute::<_, [u8; 4]>(NonZeroU32_be::new_unchecked(
                    0x01020304
                )),
            );

            // NonZeroU64
            assert_eq!(
                transmute::<_, [u8; 8]>(NonZeroU64::new_unchecked(
                    0x0102030405060708
                )),
                transmute::<_, [u8; 8]>(NonZeroU64_ne::new_unchecked(
                    0x0102030405060708
                )),
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 8]>(NonZeroU64_le::new_unchecked(
                    0x0102030405060708
                )),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                transmute::<_, [u8; 8]>(NonZeroU64_be::new_unchecked(
                    0x0102030405060708
                )),
            );

            // NonZeroU128
            assert_eq!(
                transmute::<_, [u8; 16]>(NonZeroU128::new_unchecked(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
                transmute::<_, [u8; 16]>(NonZeroU128_ne::new_unchecked(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
            assert_eq!(
                [
                    0x10, 0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07,
                    0x06, 0x05, 0x04, 0x03, 0x02, 0x01
                ],
                transmute::<_, [u8; 16]>(NonZeroU128_le::new_unchecked(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
            assert_eq!(
                [
                    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a,
                    0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10
                ],
                transmute::<_, [u8; 16]>(NonZeroU128_be::new_unchecked(
                    0x0102030405060708090a0b0c0d0e0f10
                )),
            );
        }
    }

    #[cfg(target_has_atomic = "16")]
    #[test]
    fn atomics_16() {
        unsafe {
            // AtomicI16
            assert_eq!(
                transmute::<_, [u8; 2]>(AtomicI16::new(0x0102)),
                transmute::<_, [u8; 2]>(AtomicI16_ne::new(0x0102)),
            );
            assert_eq!(
                [0x02, 0x01],
                transmute::<_, [u8; 2]>(AtomicI16_le::new(0x0102)),
            );
            assert_eq!(
                [0x01, 0x02],
                transmute::<_, [u8; 2]>(AtomicI16_be::new(0x0102)),
            );

            // AtomicU16
            assert_eq!(
                transmute::<_, [u8; 2]>(AtomicU16::new(0x0102)),
                transmute::<_, [u8; 2]>(AtomicU16_ne::new(0x0102)),
            );
            assert_eq!(
                [0x02, 0x01],
                transmute::<_, [u8; 2]>(AtomicU16_le::new(0x0102)),
            );
            assert_eq!(
                [0x01, 0x02],
                transmute::<_, [u8; 2]>(AtomicU16_be::new(0x0102)),
            );
        }
    }

    #[cfg(target_has_atomic = "32")]
    #[test]
    fn atomics_32() {
        unsafe {
            // AtomicI32
            assert_eq!(
                transmute::<_, [u8; 4]>(AtomicI32::new(0x01020304)),
                transmute::<_, [u8; 4]>(AtomicI32_ne::new(0x01020304)),
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 4]>(AtomicI32_le::new(0x01020304)),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                transmute::<_, [u8; 4]>(AtomicI32_be::new(0x01020304)),
            );

            // AtomicU32
            assert_eq!(
                transmute::<_, [u8; 4]>(AtomicU32::new(0x01020304)),
                transmute::<_, [u8; 4]>(AtomicU32_ne::new(0x01020304)),
            );
            assert_eq!(
                [0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 4]>(AtomicU32_le::new(0x01020304)),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04],
                transmute::<_, [u8; 4]>(AtomicU32_be::new(0x01020304)),
            );
        }
    }

    #[cfg(target_has_atomic = "64")]
    #[test]
    fn atomics_64() {
        unsafe {
            // AtomicI64
            assert_eq!(
                transmute::<_, [u8; 8]>(AtomicI64::new(0x0102030405060708)),
                transmute::<_, [u8; 8]>(AtomicI64_ne::new(0x0102030405060708)),
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 8]>(AtomicI64_le::new(0x0102030405060708)),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                transmute::<_, [u8; 8]>(AtomicI64_be::new(0x0102030405060708)),
            );

            // AtomicU64
            assert_eq!(
                transmute::<_, [u8; 8]>(AtomicU64::new(0x0102030405060708)),
                transmute::<_, [u8; 8]>(AtomicU64_ne::new(0x0102030405060708)),
            );
            assert_eq!(
                [0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01],
                transmute::<_, [u8; 8]>(AtomicU64_le::new(0x0102030405060708)),
            );
            assert_eq!(
                [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
                transmute::<_, [u8; 8]>(AtomicU64_be::new(0x0102030405060708)),
            );
        }
    }
}
