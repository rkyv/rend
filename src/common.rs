macro_rules! impl_integer {
    ($name:ident: $endian:ident $prim:ty) => {
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

macro_rules! impl_signed_integer_traits {
    ($name:ident: $endian:ident $prim:ty) => {
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
        impl_partial_eq_and_eq!(for $name: $prim);
        impl_partial_ord_and_ord!(for $name: $prim);
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

macro_rules! impl_unsigned_integer_traits {
    ($name:ident: $endian:ident $prim:ty) => {
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
        impl_partial_eq_and_eq!(for $name: $prim);
        impl_partial_ord_and_ord!(for $name: $prim);
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

macro_rules! impl_float {
    ($name:ident: $endian:ident $prim:ty as $prim_int:ty) => {
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

macro_rules! impl_char {
    ($name:ident: $endian:ident) => {
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
        impl_partial_eq_and_eq!(for $name: char);
        impl_partial_ord_and_ord!(for $name: char);

        #[cfg(feature = "bytecheck")]
        // SAFETY: `check_bytes` only returns `Ok` if the code point contained
        // within the endian-aware `char` represents a valid `char`.
        unsafe impl<C> bytecheck::CheckBytes<C> for $name
        where
            C: bytecheck::rancor::Fallible + ?Sized,
            C::Error: bytecheck::rancor::Trace,
            char: bytecheck::CheckBytes<C>,
        {
            #[inline]
            unsafe fn check_bytes(
                value: *const Self,
                context: &mut C,
            ) -> Result<(), C::Error> {
                use bytecheck::rancor::ResultExt as _;

                // SAFETY: `value` points to a `Self`, which has the same size
                // as a `u32` and is at least as aligned as one.
                let u = unsafe { *value.cast::<u32>() };
                let c = swap_endian!($endian u);
                // SAFETY: `value` points to a valid endian-aware `char` type if
                // `c` is a valid `char`.
                unsafe {
                    char::check_bytes(&c as *const u32 as *const char, context)
                        .with_trace(|| $crate::context::ValueCheckContext {
                            inner_name: "char",
                            outer_name: core::stringify!($name),
                        })
                }
            }
        }
    };
}

macro_rules! impl_nonzero {
    ($name:ident: $endian:ident $prim:ty as $prim_int:ty) => {
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
        impl_partial_eq_and_eq!(for $name: $prim);
        impl_partial_ord_and_ord!(for $name: $prim);
        impl_fmt!(UpperHex for $name);

        #[cfg(feature = "bytecheck")]
        // SAFETY: `check_bytes` only returns `Ok` if `value` points to a valid
        // non-zero value, which is the only requirement for `NonZero` integers.
        unsafe impl<C> bytecheck::CheckBytes<C> for $name
        where
            C: bytecheck::rancor::Fallible + ?Sized,
            C::Error: bytecheck::rancor::Trace,
            $prim: bytecheck::CheckBytes<C>,
        {
            #[inline]
            unsafe fn check_bytes(
                value: *const Self,
                context: &mut C,
            ) -> Result<(), C::Error> {
                use bytecheck::rancor::ResultExt as _;

                // SAFETY: `value` points to a `Self`, which has the same size
                // as a `$prim` and is at least as aligned as one. Note that the
                // bit pattern for 0 is always the same regardless of
                // endianness.
                unsafe {
                    <$prim>::check_bytes(value.cast(), context)
                        .with_trace(|| $crate::context::ValueCheckContext {
                            inner_name: core::stringify!($prim),
                            outer_name: core::stringify!($name),
                        })
                }
            }
        }
    };
}
