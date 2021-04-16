use crate::{ConvertEndian, Endianness};

/// The big-endian endianness
pub struct BigEndian;

impl Endianness for BigEndian {
    #[inline(always)]
    fn convert_native<T: ConvertEndian>(value: T) -> T {
        #[cfg(target_endian = "big")]
        { value }
        #[cfg(target_endian = "little")]
        { value.convert_endian() }
    }
}

/// The little-endian endianness
pub struct LittleEndian;

impl Endianness for LittleEndian {
    #[inline(always)]
    fn convert_native<T: ConvertEndian>(value: T) -> T {
        #[cfg(target_endian = "big")]
        { value.convert_endian() }
        #[cfg(target_endian = "little")]
        { value }
    }
}
