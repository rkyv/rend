macro_rules! impl_unop {
    ($trait:ident::$fn:ident) => {
        impl ::core::ops::$trait for Endian {
            type Output = <Native as ::core::ops::$trait>::Output;

            #[inline]
            fn $fn(self) -> Self::Output {
                self.to_ne().$fn()
            }
        }
    };
}

macro_rules! impl_binop {
    ($trait:ident::$fn:ident) => {
        impl_binop!(@both $trait::$fn (Endian, Native));
        impl_binop!(@both $trait::$fn (&'_ Endian, Native));
        impl_binop!(@both $trait::$fn (Endian, &'_ Native));
        impl_binop!(@both $trait::$fn (&'_ Endian, &'_ Native));

        impl_binop!(@one $trait::$fn (Endian, Endian));
        impl_binop!(@one $trait::$fn (&'_ Endian, Endian));
        impl_binop!(@one $trait::$fn (Endian, &'_ Endian));
        impl_binop!(@one $trait::$fn (&'_ Endian, &'_ Endian));
    };
    (@nonzero $trait:ident::$fn:ident) => {
        impl_binop!(@both $trait::$fn (Endian, Native));
        impl_binop!(@both $trait::$fn (&'_ Endian, Native));

        impl_binop!(@one $trait::$fn (Endian, Endian));
        impl_binop!(@one $trait::$fn (&'_ Endian, Endian));
        impl_binop!(@one $trait::$fn (Endian, &'_ Endian));
        impl_binop!(@one $trait::$fn (&'_ Endian, &'_ Endian));
    };
    (@both $trait:ident::$fn:ident ($self:ty, $other:ty)) => {
        impl ::core::ops::$trait<$other> for $self {
            type Output = Native;

            #[inline]
            fn $fn(self, other: $other) -> Self::Output {
                self.to_ne().$fn(other)
            }
        }

        impl ::core::ops::$trait<$self> for $other {
            type Output = Native;

            #[inline]
            fn $fn(self, other: $self) -> Self::Output {
                self.$fn(other.to_ne())
            }
        }
    };
    (@one $trait:ident::$fn:ident ($self:ty, $other:ty)) => {
        impl ::core::ops::$trait<$other> for $self {
            type Output = Native;

            #[inline]
            fn $fn(self, other: $other) -> Self::Output {
                self.to_ne().$fn(other.to_ne())
            }
        }
    };
}

macro_rules! impl_binassign {
    ($trait:ident::$fn:ident) => {
        impl ::core::ops::$trait<Native> for Endian {
            #[inline]
            fn $fn(&mut self, other: Native) {
                self.swap_endian();
                self.value.$fn(other);
                self.swap_endian();
            }
        }

        impl ::core::ops::$trait<Endian> for Endian {
            #[inline]
            fn $fn(&mut self, other: Endian) {
                self.swap_endian();
                self.value.$fn(other.to_ne());
                self.swap_endian();
            }
        }

        impl ::core::ops::$trait<&'_ Native> for Endian {
            #[inline]
            fn $fn(&mut self, other: &'_ Native) {
                self.swap_endian();
                self.value.$fn(other);
                self.swap_endian();
            }
        }

        impl ::core::ops::$trait<&'_ Endian> for Endian {
            #[inline]
            fn $fn(&mut self, other: &'_ Endian) {
                self.swap_endian();
                self.value.$fn(other.to_ne());
                self.swap_endian();
            }
        }
    };
    (@nonzero $trait:ident::$fn:ident) => {
        impl ::core::ops::$trait<Native> for Endian {
            #[inline]
            fn $fn(&mut self, other: Native) {
                self.swap_endian();
                self.value.$fn(other);
                self.swap_endian();
            }
        }

        impl ::core::ops::$trait<Endian> for Endian {
            #[inline]
            fn $fn(&mut self, other: Endian) {
                self.swap_endian();
                self.value.$fn(other.to_ne());
                self.swap_endian();
            }
        }
    };
}

macro_rules! impl_fmt {
    ($trait:ident) => {
        impl ::core::fmt::$trait for Endian {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                ::core::fmt::$trait::fmt(&self.to_ne(), f)
            }
        }
    };
}

macro_rules! impl_default {
    () => {
        impl Default for Endian {
            #[inline]
            fn default() -> Self {
                Self::new(Native::default())
            }
        }
    };
}

macro_rules! impl_eq {
    () => {
        impl Eq for Endian {}
    };
}

macro_rules! impl_from {
    () => {
        impl From<Native> for Endian {
            fn from(value: Native) -> Self {
                Self::new(value)
            }
        }
    };
}

macro_rules! impl_hash {
    () => {
        impl Hash for Endian {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.to_ne().hash(state);
            }
        }
    };
}

macro_rules! impl_ord {
    () => {
        impl Ord for Endian {
            #[inline]
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                self.to_ne().cmp(&other.to_ne())
            }
        }
    };
}

macro_rules! impl_partial_eq {
    () => {
        impl PartialEq for Endian {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                self.value.eq(&other.value)
            }
        }

        impl PartialEq<Native> for Endian {
            #[inline]
            fn eq(&self, other: &Native) -> bool {
                self.to_ne().eq(other)
            }
        }
    };
}

macro_rules! impl_partial_ord {
    () => {
        impl PartialOrd for Endian {
            #[inline]
            fn partial_cmp(&self, other: &Self) -> Option<::core::cmp::Ordering> {
                self.to_ne().partial_cmp(&other.to_ne())
            }
        }

        impl PartialOrd<Native> for Endian {
            #[inline]
            fn partial_cmp(&self, other: &Native) -> Option<::core::cmp::Ordering> {
                self.to_ne().partial_cmp(other)
            }
        }
    };
}

macro_rules! impl_product {
    () => {
        impl ::core::iter::Product for Endian {
            #[inline]
            fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
                Self::new(iter.map(|x| x.to_ne()).product())
            }
        }
    };
}

macro_rules! impl_sum {
    () => {
        impl ::core::iter::Sum for Endian {
            #[inline]
            fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
                Self::new(iter.map(|x| x.to_ne()).sum())
            }
        }
    };
}
