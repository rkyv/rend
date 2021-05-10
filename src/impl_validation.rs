macro_rules! impl_validation {
    (@always: $te:ident) => {
        impl<C: ?Sized> CheckBytes<C> for $te {
            type Error = Unreachable;

            #[inline]
            unsafe fn check_bytes<'a>(
                value: *const Self,
                _: &mut C,
            ) -> Result<&'a Self, Self::Error> {
                Ok(&*value)
            }
        }
    };
    (@char: $te:ident, $teu32:ty) => {
        impl<C: ?Sized> CheckBytes<C> for $te {
            type Error = CharCheckError;

            #[inline]
            unsafe fn check_bytes<'a>(
                value: *const Self,
                context: &mut C,
            ) -> Result<&'a Self, Self::Error> {
                let as_u32 = &*<$tu32>::check_bytes(value.cast(), context)?;
                let c = as_u32.to_ne();
                char::try_from(c).map_err(|_| CharCheckError { invalid_value: c})?;
                Ok(&*value)
            }
        }
    };
    (@nonzero: $te:ident, $prim:ty) => {
        impl<C: ?Sized> CheckBytes<C> for $te {
            type Error = NonZeroCheckError;

            #[inline]
            unsafe fn check_bytes<'a>(
                value: *const Self,
                context: &mut C,
            ) -> Result<&'a Self, Self::Error> {
                if *$prim::check_bytes(value.cast(), context)? == 0 {
                    Err(NonZeroCheckError::IsZero)
                } else {
                    Ok(&*value)
                }
            }
        }
    };
}