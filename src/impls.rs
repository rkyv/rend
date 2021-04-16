use crate::Primitive;
use core::{
    num::{
        NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI128, NonZeroU16, NonZeroU32, NonZeroU64,
        NonZeroU128
    },
    // sync::atomic::{
    //     AtomicI16, AtomicI32, AtomicI64, AtomicU16, AtomicU32, AtomicU64
    // },
};

macro_rules! impl_integer {
    ($ne:ty) => {
        impl Primitive for $ne {
            #[inline(always)]
            fn swap_endian(self) -> Self {
                self.swap_bytes()
            }
        }
    }
}

impl_integer!(i16);
impl_integer!(i32);
impl_integer!(i64);
impl_integer!(i128);
impl_integer!(u16);
impl_integer!(u32);
impl_integer!(u64);
impl_integer!(u128);

macro_rules! impl_float {
    ($ne:ty) => {
        impl Primitive for $ne {
            #[inline(always)]
            fn swap_endian(self) -> Self {
                <$ne>::from_bits(self.to_bits().swap_bytes())
            }
        }
    }
}

impl_float!(f32);
impl_float!(f64);

impl Primitive for char {
    #[inline(always)]
    fn swap_endian(self) -> Self {
        unsafe { core::char::from_u32_unchecked((self as u32).swap_bytes()) }
    }
}

macro_rules! impl_nonzero {
    ($ne:ty) => {
        impl Primitive for $ne {
            #[inline(always)]
            fn swap_endian(self) -> Self {
                unsafe { <$ne>::new_unchecked(self.get().swap_bytes()) }
            }
        }
    }
}

impl_nonzero!(NonZeroI16);
impl_nonzero!(NonZeroI32);
impl_nonzero!(NonZeroI64);
impl_nonzero!(NonZeroI128);
impl_nonzero!(NonZeroU16);
impl_nonzero!(NonZeroU32);
impl_nonzero!(NonZeroU64);
impl_nonzero!(NonZeroU128);

// macro_rules! impl_atomic {
//     ($ne:ty) => {
//         impl Primitive for $ne {
//             #[inline(always)]
//             fn convert_endian(self) -> Self {
//                 <$ne>::new(self.load(::core::sync::atomic::Ordering::Relaxed).swap_bytes())
//             }
//         }
//     }
// }

// impl_atomic!(AtomicI16);
// impl_atomic!(AtomicI32);
// impl_atomic!(AtomicI64);
// impl_atomic!(AtomicU16);
// impl_atomic!(AtomicU32);
// impl_atomic!(AtomicU64);
