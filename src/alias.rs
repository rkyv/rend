use crate::{BigEndian, Endian, LittleEndian};
use core::num::{
    NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroU128, NonZeroU16, NonZeroU32,
    NonZeroU64,
};
#[cfg(has_atomics)]
use core::sync::atomic::{AtomicI16, AtomicI32, AtomicU16, AtomicU32};
#[cfg(has_atomics_64)]
use core::sync::atomic::{AtomicI64, AtomicU64};

/// Alias for `Endian<i16, BigEndian>`
#[allow(non_camel_case_types)]
pub type i16_be = Endian<i16, BigEndian>;
/// Alias for `Endian<i16, LittleEndian>`
#[allow(non_camel_case_types)]
pub type i16_le = Endian<i16, LittleEndian>;

/// Alias for `Endian<i32, BigEndian>`
#[allow(non_camel_case_types)]
pub type i32_be = Endian<i32, BigEndian>;
/// Alias for `Endian<i32, LittleEndian>`
#[allow(non_camel_case_types)]
pub type i32_le = Endian<i32, LittleEndian>;

/// Alias for `Endian<i64, BigEndian>`
#[allow(non_camel_case_types)]
pub type i64_be = Endian<i64, BigEndian>;
/// Alias for `Endian<i64, LittleEndian>`
#[allow(non_camel_case_types)]
pub type i64_le = Endian<i64, LittleEndian>;

/// Alias for `Endian<i128, BigEndian>`
#[allow(non_camel_case_types)]
pub type i128_be = Endian<i128, BigEndian>;
/// Alias for `Endian<i128, LittleEndian>`
#[allow(non_camel_case_types)]
pub type i128_le = Endian<i128, LittleEndian>;

/// Alias for `Endian<u16, BigEndian>`
#[allow(non_camel_case_types)]
pub type u16_be = Endian<u16, BigEndian>;
/// Alias for `Endian<u16, LittleEndian>`
#[allow(non_camel_case_types)]
pub type u16_le = Endian<u16, LittleEndian>;

/// Alias for `Endian<u32, BigEndian>`
#[allow(non_camel_case_types)]
pub type u32_be = Endian<u32, BigEndian>;
/// Alias for `Endian<u32, LittleEndian>`
#[allow(non_camel_case_types)]
pub type u32_le = Endian<u32, LittleEndian>;

/// Alias for `Endian<u64, BigEndian>`
#[allow(non_camel_case_types)]
pub type u64_be = Endian<u64, BigEndian>;
/// Alias for `Endian<u64, LittleEndian>`
#[allow(non_camel_case_types)]
pub type u64_le = Endian<u64, LittleEndian>;

/// Alias for `Endian<u128, BigEndian>`
#[allow(non_camel_case_types)]
pub type u128_be = Endian<u128, BigEndian>;
/// Alias for `Endian<u128, LittleEndian>`
#[allow(non_camel_case_types)]
pub type u128_le = Endian<u128, LittleEndian>;

/// Alias for `Endian<f32, BigEndian>`
#[allow(non_camel_case_types)]
pub type f32_be = Endian<f32, BigEndian>;
/// Alias for `Endian<f32, LittleEndian>`
#[allow(non_camel_case_types)]
pub type f32_le = Endian<f32, LittleEndian>;

/// Alias for `Endian<f64, BigEndian>`
#[allow(non_camel_case_types)]
pub type f64_be = Endian<f64, BigEndian>;
/// Alias for `Endian<f64, LittleEndian>`
#[allow(non_camel_case_types)]
pub type f64_le = Endian<f64, LittleEndian>;

/// Alias for `Endian<char, BigEndian>`
#[allow(non_camel_case_types)]
pub type char_be = Endian<char, BigEndian>;
/// Alias for `Endian<char, LittleEndian>`
#[allow(non_camel_case_types)]
pub type char_le = Endian<char, LittleEndian>;

/// Alias for `Endian<NonZeroI16, BigEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroI16_be = Endian<NonZeroI16, BigEndian>;
/// Alias for `Endian<NonZeroI16, LittleEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroI16_le = Endian<NonZeroI16, LittleEndian>;

/// Alias for `Endian<NonZeroI32, BigEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroI32_be = Endian<NonZeroI32, BigEndian>;
/// Alias for `Endian<NonZeroI32, LittleEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroI32_le = Endian<NonZeroI32, LittleEndian>;

/// Alias for `Endian<NonZeroI64, BigEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroI64_be = Endian<NonZeroI64, BigEndian>;
/// Alias for `Endian<NonZeroI64, LittleEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroI64_le = Endian<NonZeroI64, LittleEndian>;

/// Alias for `Endian<NonZeroI128, BigEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroI128_be = Endian<NonZeroI128, BigEndian>;
/// Alias for `Endian<NonZeroI128, LittleEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroI128_le = Endian<NonZeroI128, LittleEndian>;

/// Alias for `Endian<NonZeroU16, BigEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroU16_be = Endian<NonZeroU16, BigEndian>;
/// Alias for `Endian<NonZeroU16, LittleEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroU16_le = Endian<NonZeroU16, LittleEndian>;

/// Alias for `Endian<NonZeroU32, BigEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroU32_be = Endian<NonZeroU32, BigEndian>;
/// Alias for `Endian<NonZeroU32, LittleEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroU32_le = Endian<NonZeroU32, LittleEndian>;

/// Alias for `Endian<NonZeroU64, BigEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroU64_be = Endian<NonZeroU64, BigEndian>;
/// Alias for `Endian<NonZeroU64, LittleEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroU64_le = Endian<NonZeroU64, LittleEndian>;

/// Alias for `Endian<NonZeroU128, BigEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroU128_be = Endian<NonZeroU128, BigEndian>;
/// Alias for `Endian<NonZeroU128, LittleEndian>`
#[allow(non_camel_case_types)]
pub type NonZeroU128_le = Endian<NonZeroU128, LittleEndian>;

/// Alias for `Endian<AtomicI16, BigEndian>`
#[cfg(has_atomics)]
#[allow(non_camel_case_types)]
pub type AtomicI16_be = Endian<AtomicI16, BigEndian>;
/// Alias for `Endian<AtomicI16, LittleEndian>`
#[cfg(has_atomics)]
#[allow(non_camel_case_types)]
pub type AtomicI16_le = Endian<AtomicI16, LittleEndian>;

/// Alias for `Endian<AtomicI32, BigEndian>`
#[cfg(has_atomics)]
#[allow(non_camel_case_types)]
pub type AtomicI32_be = Endian<AtomicI32, BigEndian>;
/// Alias for `Endian<AtomicI32, LittleEndian>`
#[cfg(has_atomics)]
#[allow(non_camel_case_types)]
pub type AtomicI32_le = Endian<AtomicI32, LittleEndian>;

/// Alias for `Endian<AtomicI64, BigEndian>`
#[cfg(has_atomics_64)]
#[allow(non_camel_case_types)]
pub type AtomicI64_be = Endian<AtomicI64, BigEndian>;
/// Alias for `Endian<AtomicI64, LittleEndian>`
#[cfg(has_atomics_64)]
#[allow(non_camel_case_types)]
pub type AtomicI64_le = Endian<AtomicI64, LittleEndian>;

/// Alias for `Endian<AtomicU16, BigEndian>`
#[cfg(has_atomics)]
#[allow(non_camel_case_types)]
pub type AtomicU16_be = Endian<AtomicU16, BigEndian>;
/// Alias for `Endian<AtomicU16, LittleEndian>`
#[cfg(has_atomics)]
#[allow(non_camel_case_types)]
pub type AtomicU16_le = Endian<AtomicU16, LittleEndian>;

/// Alias for `Endian<AtomicU32, BigEndian>`
#[cfg(has_atomics)]
#[allow(non_camel_case_types)]
pub type AtomicU32_be = Endian<AtomicU32, BigEndian>;
/// Alias for `Endian<AtomicU32, LittleEndian>`
#[cfg(has_atomics)]
#[allow(non_camel_case_types)]
pub type AtomicU32_le = Endian<AtomicU32, LittleEndian>;

/// Alias for `Endian<AtomicU64, BigEndian>`
#[cfg(has_atomics_64)]
#[allow(non_camel_case_types)]
pub type AtomicU64_be = Endian<AtomicU64, BigEndian>;
/// Alias for `Endian<AtomicU64, LittleEndian>`
#[cfg(has_atomics_64)]
#[allow(non_camel_case_types)]
pub type AtomicU64_le = Endian<AtomicU64, LittleEndian>;
