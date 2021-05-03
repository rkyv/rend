use crate::{Endianness, Primitive};

/// The big-endian endianness
pub struct BigEndian;

impl Endianness for BigEndian {
    #[inline(always)]
    fn swap_native<T: Primitive>(value: T) -> T {
        #[cfg(target_endian = "big")]
        {
            value
        }
        #[cfg(target_endian = "little")]
        {
            value.swap_endian()
        }
    }
}

/// The little-endian endianness
pub struct LittleEndian;

impl Endianness for LittleEndian {
    #[inline(always)]
    fn swap_native<T: Primitive>(value: T) -> T {
        #[cfg(target_endian = "big")]
        {
            value.swap_endian()
        }
        #[cfg(target_endian = "little")]
        {
            value
        }
    }
}
