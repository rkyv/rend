macro_rules! impl_struct {
    (@$class:ident $endian:ident<$ne:ty> ($($const:ident)?)) => {
        impl $endian<$ne> {
            /// Creates a new value from a native-endian value
            #[inline]
            pub $($const)? fn new(value: $ne) -> Self {
                Self {
                    value: swap_bytes!(@$class $endian<$ne> value)
                }
            }

            /// Converts the value to a native-endian value
            #[inline]
            pub $($const)? fn to_ne(self) -> $ne {
                swap_bytes!(@$class $endian<$ne> self.value)
            }

            #[inline]
            #[allow(dead_code)]
            fn swap_endian(&mut self) {
                self.value = swap_bytes!(@$class $endian<$ne> self.value);
            }
        }
    };
    (@signed_int $endian:ident<$ne:ty>) => {
        impl_struct!(@signed_int $endian<$ne> (const));

        const _: () = {
            type Endian = $endian<$ne>;
            type Native = $ne;

            impl_binop!(Add::add);
            impl_binassign!(AddAssign::add_assign);
            impl_fmt!(Binary);
            impl_binop!(BitAnd::bitand);
            impl_binassign!(BitAndAssign::bitand_assign);
            impl_binop!(BitOr::bitor);
            impl_binassign!(BitOrAssign::bitor_assign);
            impl_binop!(BitXor::bitxor);
            impl_binassign!(BitXorAssign::bitxor_assign);
            impl_fmt!(Debug);
            impl_default!();
            impl_fmt!(Display);
            impl_binop!(Div::div);
            impl_binassign!(DivAssign::div_assign);
            impl_eq!();
            impl_from!();
            impl_hash!();
            impl_fmt!(LowerExp);
            impl_fmt!(LowerHex);
            impl_binop!(Mul::mul);
            impl_binassign!(MulAssign::mul_assign);
            impl_unop!(Neg::neg);
            impl_unop!(Not::not);
            impl_fmt!(Octal);
            impl_ord!();
            impl_partial_eq!();
            impl_partial_ord!();
            impl_product!();
            impl_binop!(Rem::rem);
            impl_binassign!(RemAssign::rem_assign);
            impl_binop!(Shl::shl);
            impl_binassign!(ShlAssign::shl_assign);
            impl_binop!(Shr::shr);
            impl_binassign!(ShrAssign::shr_assign);
            impl_binop!(Sub::sub);
            impl_binassign!(SubAssign::sub_assign);
            impl_sum!();
            impl_fmt!(UpperExp);
            impl_fmt!(UpperHex);
        };
    };
    (@unsigned_int $endian:ident<$ne:ty>) => {
        impl_struct!(@unsigned_int $endian<$ne> (const));

        const _: () = {
            type Endian = $endian<$ne>;
            type Native = $ne;

            impl_binop!(Add::add);
            impl_binassign!(AddAssign::add_assign);
            impl_fmt!(Binary);
            impl_binop!(BitAnd::bitand);
            impl_binassign!(BitAndAssign::bitand_assign);
            impl_binop!(BitOr::bitor);
            impl_binassign!(BitOrAssign::bitor_assign);
            impl_binop!(BitXor::bitxor);
            impl_binassign!(BitXorAssign::bitxor_assign);
            impl_fmt!(Debug);
            impl_default!();
            impl_fmt!(Display);
            impl_binop!(Div::div);
            impl_binassign!(DivAssign::div_assign);
            impl_eq!();
            impl_from!();
            impl_hash!();
            impl_fmt!(LowerExp);
            impl_fmt!(LowerHex);
            impl_binop!(Mul::mul);
            impl_binassign!(MulAssign::mul_assign);
            impl_unop!(Not::not);
            impl_fmt!(Octal);
            impl_ord!();
            impl_partial_eq!();
            impl_partial_ord!();
            impl_product!();
            impl_binop!(Rem::rem);
            impl_binassign!(RemAssign::rem_assign);
            impl_binop!(Shl::shl);
            impl_binassign!(ShlAssign::shl_assign);
            impl_binop!(Shr::shr);
            impl_binassign!(ShrAssign::shr_assign);
            impl_binop!(Sub::sub);
            impl_binassign!(SubAssign::sub_assign);
            impl_sum!();
            impl_fmt!(UpperExp);
            impl_fmt!(UpperHex);
        };
    };
    (@float $endian:ident<$ne:ty>) => {
        impl_struct!(@float $endian<$ne> ());

        const _: () = {
            type Endian = $endian<$ne>;
            type Native = $ne;

            impl_binop!(Add::add);
            impl_binassign!(AddAssign::add_assign);
            impl_fmt!(Debug);
            impl_default!();
            impl_fmt!(Display);
            impl_binop!(Div::div);
            impl_binassign!(DivAssign::div_assign);
            impl_eq!();
            impl_from!();
            impl_fmt!(LowerExp);
            impl_binop!(Mul::mul);
            impl_binassign!(MulAssign::mul_assign);
            impl_unop!(Neg::neg);
            impl_partial_eq!();
            impl_partial_ord!();
            impl_product!();
            impl_binop!(Rem::rem);
            impl_binassign!(RemAssign::rem_assign);
            impl_binop!(Sub::sub);
            impl_binassign!(SubAssign::sub_assign);
            impl_sum!();
            impl_fmt!(UpperExp);
        };
    };
    (@char $endian:ident<$ne:ty>) => {
        impl_struct!(@char $endian<$ne> ());

        const _: () = {
            type Endian = $endian<$ne>;
            type Native = $ne;

            impl_fmt!(Debug);
            impl_default!();
            impl_fmt!(Display);
            impl_eq!();
            impl_from!();
            impl_hash!();
            impl_ord!();
            impl_partial_eq!();
            impl_partial_ord!();
        };
    };
    (@nonzero $endian:ident<$ne:ty>) => {
        impl_struct!(@nonzero $endian<$ne> (const));

        const _: () = {
            type Endian = $endian<$ne>;
            type Native = $ne;

            impl_fmt!(Binary);
            impl_binop!(@nonzero BitOr::bitor);
            impl_binassign!(@nonzero BitOrAssign::bitor_assign);
            impl_fmt!(Debug);
            impl_fmt!(Display);
            impl_eq!();
            impl_from!();
            impl_hash!();
            impl_fmt!(LowerHex);
            impl_fmt!(Octal);
            impl_ord!();
            impl_partial_eq!();
            impl_partial_ord!();
            impl_fmt!(UpperHex);
        };
    };
    (@atomic $endian:ident<$ne:ty>) => {
        impl $endian<$ne> {
            /// Stores a value into the atomic integer if the current value is the same as the
            /// `current` value.
            #[inline]
            pub fn compare_exchange(
                &self,
                current: Primitive<$ne>,
                new: Primitive<$ne>,
                success: Ordering,
                failure: Ordering,
            ) -> Result<Primitive<$ne>, Primitive<$ne>> {
                match self.value.compare_exchange(
                    swap_bytes!(@atomic $endian<$ne> current),
                    swap_bytes!(@atomic $endian<$ne> new),
                    success,
                    failure,
                ) {
                    Ok(x) => Ok(swap_bytes!(@atomic $endian<$ne> x)),
                    Err(x) => Err(swap_bytes!(@atomic $endian<$ne> x)),
                }
            }

            /// Adds to the current value, returning the previous value.
            #[inline]
            pub fn fetch_add(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                self.fetch_update(order, order, |x| Some(x + val)).unwrap()
            }

            /// Bitwise "and" with the current value.
            #[inline]
            pub fn fetch_and(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                swap_bytes!(@atomic $endian<$ne> self.value.fetch_and(swap_bytes!(@atomic $endian<$ne> val), order))
            }

            /// Maximum with the current value.
            #[inline]
            pub fn fetch_max(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                self.fetch_update(order, order, |x| Some(<Primitive<$ne>>::max(x, val)))
                    .unwrap()
            }

            /// Minimum with the current value.
            #[inline]
            pub fn fetch_min(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                self.fetch_update(order, order, |x| Some(<Primitive<$ne>>::min(x, val)))
                    .unwrap()
            }

            /// Bitwise "nand" with the current value.
            #[inline]
            pub fn fetch_nand(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                swap_bytes!(@atomic $endian<$ne> self.value.fetch_nand(swap_bytes!(@atomic $endian<$ne> val), order))
            }

            /// Bitwise "or" with the current value.
            #[inline]
            pub fn fetch_or(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                swap_bytes!(@atomic $endian<$ne> self.value.fetch_or(swap_bytes!(@atomic $endian<$ne> val), order))
            }

            /// Subtracts from the current value, returning the previous value.
            #[inline]
            pub fn fetch_sub(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                self.fetch_update(order, order, |x| Some(x - val)).unwrap()
            }

            /// Fetches the value, and applies a function to it that returns an optional new value.
            /// Returns a `Result` of `Ok(previous_value)` if the function returned `Some(_)`, else
            /// `Err(previous_value)`.
            #[inline]
            pub fn fetch_update<F: FnMut(Primitive<$ne>) -> Option<Primitive<$ne>>>(
                &self,
                set_order: Ordering,
                fetch_order: Ordering,
                mut f: F,
            ) -> Result<Primitive<$ne>, Primitive<$ne>> {
                self.value.fetch_update(set_order, fetch_order, |x| {
                    f(swap_bytes!(@atomic $endian<$ne> x)).map(|x| swap_bytes!(@atomic $endian<$ne> x))
                })
            }

            /// Bitwise "xor" with the current value.
            #[inline]
            pub fn fetch_xor(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                swap_bytes!(@atomic $endian<$ne> self.value.fetch_xor(swap_bytes!(@atomic $endian<$ne> val), order))
            }

            /// Consumes the atomic and returns the contained value.
            #[inline]
            pub fn into_inner(self) -> Primitive<$ne> {
                swap_bytes!(@atomic $endian<$ne> self.value.into_inner())
            }

            /// Loads a value from the atomic integer.
            #[inline]
            pub fn load(&self, order: Ordering) -> Primitive<$ne> {
                swap_bytes!(@atomic $endian<$ne> self.value.load(order))
            }

            /// Creates a new atomic integer
            #[inline]
            pub const fn new(value: Primitive<$ne>) -> Self {
                Self {
                    value: <$ne>::new(swap_bytes!(@atomic $endian<$ne> value)),
                }
            }

            /// Stores a value into the atomic integer.
            #[inline]
            pub fn store(&self, val: Primitive<$ne>, order: Ordering) {
                self.value.store(swap_bytes!(@atomic $endian<$ne> val), order);
            }

            /// Stores a value into the atomic integer, returning the previous value.
            #[inline]
            pub fn swap(&self, val: Primitive<$ne>, order: Ordering) -> Primitive<$ne> {
                swap_bytes!(@atomic $endian<$ne> self.value.swap(swap_bytes!(@atomic $endian<$ne> val), order))
            }
        }

        impl core::fmt::Debug for $endian<$ne> {
            #[inline]
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                swap_bytes!(@atomic $endian<$ne> self.load(Ordering::Relaxed)).fmt(f)
            }
        }

        impl Default for $endian<$ne> {
            #[inline]
            fn default() -> Self {
                Self::new(Primitive::<$ne>::default())
            }
        }

        impl From<Primitive<$ne>> for $endian<$ne> {
            #[inline]
            fn from(value: Primitive<$ne>) -> Self {
                Self::new(value)
            }
        }

        #[cfg(feature = "std")]
        impl ::std::panic::RefUnwindSafe for $endian<$ne> {}

        unsafe impl Sync for $endian<$ne> {}
    };
}
