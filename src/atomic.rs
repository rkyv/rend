use core::sync::atomic::{AtomicI16, AtomicI32, AtomicI64, AtomicU16, AtomicU32, AtomicU64};

macro_rules! impl_atomic {
    ($ne:ty) => {
        // TODO: generate wrapper types and implement methods
    }
}

impl_atomic!(AtomicI16);
impl_atomic!(AtomicI32);
impl_atomic!(AtomicI64);
impl_atomic!(AtomicU16);
impl_atomic!(AtomicU32);
impl_atomic!(AtomicU64);
