use crate::{Queryable, TreeIter, Value, ValueConvertError};
use std::borrow::Cow;
use std::collections::*;
use std::convert::{TryFrom, TryInto};
use std::hash::Hash;
use std::marker::PhantomData;

macro_rules! value_int {
    ($($ty: ty $( => $via: ty)?),* $(,)?) => {
        $(
            impl From<$ty> for Value {
                fn from(v: $ty) -> Value {
                    $( let v: $via = v.into(); )?
                    Value::Int(v.try_into().unwrap_or_else(|e| {
                        panic!("{} {} could not be converted to i64: {}", stringify!($ty), v, e);
                    }))
                }
            }

            impl From<&$ty> for Value {
                fn from(v: &$ty) -> Value {
                    Value::from(*v)
                }
            }

            impl TryFrom<Value> for $ty {
                type Error = ValueConvertError;
                fn try_from(value: Value) -> Result<Self, Self::Error> {
                    Self::try_from(&value)
                }
            }

            impl TryFrom<&Value> for $ty {
                type Error = ValueConvertError;
                fn try_from(value: &Value) -> Result<Self, Self::Error> {
                    match value {
                        &Value::Int(v) => {
                            $( let v: $via = v.try_into().map_err(|_|ValueConvertError)?; )?
                            v.try_into().map_err(|_| ValueConvertError)
                        },
                        Value::String(s) => s.parse().map_err(|_| ValueConvertError),
                        &Value::Bool(b) => if b {
                            1.try_into().map_err(|_| ValueConvertError)
                        } else {
                            0.try_into().map_err(|_| ValueConvertError)
                        }
                        Value::Array(_) => Err(ValueConvertError),
                        &Value::Float(f) => if *f >= i64::min_value() as f64 && *f <= i64::max_value() as f64 {
                            <$ty>::try_from(Value::Int(*f as i64))
                        } else {
                            Err(ValueConvertError)
                        }
                    }
                }
            }

            impl<'a> Queryable<'a> for $ty {
                fn data<'f>(&self) -> Option<Value> {
                    Some((*self).into())
                }
            }
        )*
    }
}

value_int!(
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    usize,
    isize,
    std::num::NonZeroU8 => u8,
    std::num::NonZeroI8 => i8,
    std::num::NonZeroU16 => u16,
    std::num::NonZeroI16 => i16,
    std::num::NonZeroU32 => u32,
    std::num::NonZeroI32 => i32,
    std::num::NonZeroU64 => u64,
    std::num::NonZeroI64 => i64,
    std::num::NonZeroU128 => u128,
    std::num::NonZeroI128 => i128,
    std::num::NonZeroUsize => usize,
    std::num::NonZeroIsize => isize,
);

macro_rules! value_float {
    ($($ty: tt),* $(,)?) => {
        $(
            impl From<$ty> for Value {
                fn from(v: $ty) -> Value {
                    Value::Float(v.try_into().unwrap_or_else(|e| {
                        panic!("{} {} could not be converted to i64: {}", stringify!($ty), v, e);
                    }))
                }
            }

            impl From<&$ty> for Value {
                fn from(v: &$ty) -> Value {
                    Value::from(*v)
                }
            }

            impl TryFrom<Value> for $ty {
                type Error = ValueConvertError;
                fn try_from(value: Value) -> Result<Self, Self::Error> {
                    Self::try_from(&value)
                }
            }

            impl TryFrom<&Value> for $ty {
                type Error = ValueConvertError;
                fn try_from(value: &Value) -> Result<Self, Self::Error> {
                    match value {
                        &Value::Int(v) => {
                            Ok((v as $ty).into())
                        },
                        Value::String(s) => s.parse().map_err(|_| ValueConvertError),
                        &Value::Bool(b) => if b {
                            Ok(1.0)
                        } else {
                            Ok(0.0)
                        }
                        Value::Array(_) => Err(ValueConvertError),
                        &Value::Float(f) => if *f >= $ty::MIN as f64 && *f <= $ty::MAX as f64 {
                            Ok(*f as $ty)
                        } else {
                            Err(ValueConvertError)
                        }
                    }
                }
            }

            impl<'a> Queryable<'a> for $ty {
                fn data<'f>(&self) -> Option<Value> {
                    Some((*self).into())
                }
            }
        )*
    }
}

value_float!(f32, f64);

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Bool(b)
    }
}

impl From<&bool> for Value {
    fn from(b: &bool) -> Value {
        Value::Bool(*b)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Value {
        Value::String(v.to_owned())
    }
}

impl From<String> for Value {
    fn from(v: String) -> Value {
        Value::String(v)
    }
}

impl From<&String> for Value {
    fn from(v: &String) -> Value {
        Value::String(v.clone())
    }
}

impl TryFrom<Value> for String {
    type Error = ValueConvertError;
    fn try_from(v: Value) -> Result<String, ValueConvertError> {
        match v {
            Value::Int(i) => Ok(i.to_string()),
            Value::String(s) => Ok(s),
            Value::Bool(b) => Ok(b.to_string()),
            Value::Array(arr) => {
                if arr.0.len() == 1 {
                    Ok(arr.0[0].to_string())
                } else {
                    Err(ValueConvertError)
                }
            }
            Value::Float(f) => Ok(f.to_string()),
        }
    }
}

impl<'b, T> From<Cow<'b, T>> for Value
where
    T: ToOwned,
    Value: From<&'b T>,
    Value: From<T::Owned>,
{
    fn from(v: Cow<'b, T>) -> Value {
        Value::from(v.into_owned())
    }
}

impl<'a> Queryable<'a> for bool {
    fn data(&self) -> Option<Value> {
        Some(self.into())
    }
}

impl<'a> Queryable<'a> for &bool {
    fn data(&self) -> Option<Value> {
        Some((*self).into())
    }
}

impl<'a> Queryable<'a> for str {
    fn data(&self) -> Option<Value> {
        Some(self.into())
    }
}

impl<'a> Queryable<'a> for &str {
    fn data(&self) -> Option<Value> {
        Some((*self).into())
    }
}

impl<'a> Queryable<'a> for String {
    fn data(&self) -> Option<Value> {
        Some(self.into())
    }
}

impl<'a> Queryable<'a> for &String {
    fn data(&self) -> Option<Value> {
        Some((*self).into())
    }
}

impl<'a, K, V> Queryable<'a> for HashMap<K, V>
where
    // TODO get rid of the Clone constraint
    K: Queryable<'a> + Hash + Eq + Clone,
    V: Queryable<'a>,
    K: TryFrom<Value>,
{
    fn member<'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'a>> {
        let key = K::try_from(field.clone()).ok()?;
        self.get(&key).map(|v| v as _)
    }

    fn all(&'a self) -> TreeIter<'a> {
        TreeIter::from_queryables(self.values())
    }
}

impl<'a, K, V> Queryable<'a> for BTreeMap<K, V>
where
    K: Queryable<'a> + Ord + Eq + 'a,
    V: Queryable<'a>,
    K: TryFrom<Value>,
{
    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        let key = K::try_from(field.clone()).ok()?;
        self.get(&key).map(|v| v as _)
    }

    fn all(&'a self) -> TreeIter<'a> {
        TreeIter::from_queryables(self.values())
    }
}

impl<'a, V> Queryable<'a> for BTreeSet<V>
where
    V: Queryable<'a> + Ord + Eq,
{
    fn all(&'a self) -> TreeIter<'a> {
        TreeIter::from_queryables(self)
    }
}

impl<'a, V> Queryable<'a> for HashSet<V>
where
    V: Queryable<'a> + Hash + Eq,
{
    fn all(&'a self) -> TreeIter<'a> {
        TreeIter::from_queryables(self)
    }
}

impl<'a, T> Queryable<'a> for Vec<T>
where
    T: Queryable<'a>,
{
    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        let index = usize::try_from(field).ok()?;
        self.get(index).map(|v| v as _)
    }

    fn all(&'a self) -> TreeIter<'a> {
        TreeIter::from_queryables(self)
    }
}

impl<'a, T> Queryable<'a> for VecDeque<T>
where
    T: Queryable<'a>,
{
    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        let index = usize::try_from(field).ok()?;
        self.get(index).map(|v| v as _)
    }

    fn all(&'a self) -> TreeIter<'a> {
        TreeIter::from_queryables(self.iter())
    }
}

impl<'a, T> Queryable<'a> for Box<T>
where
    T: Queryable<'a>,
{
    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        self.as_ref().member(field)
    }

    fn all(&'a self) -> TreeIter<'a> {
        self.as_ref().all()
    }

    fn data(&self) -> Option<Value> {
        self.as_ref().data()
    }
}

impl<'a, T> Queryable<'a> for Option<T>
where
    T: Queryable<'a>,
{
    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        self.as_ref().and_then(|val| val.member(field))
    }

    fn all(&'a self) -> TreeIter<'a> {
        if let Some(val) = self.as_ref() {
            val.all()
        } else {
            TreeIter::empty()
        }
    }

    fn data(&self) -> Option<Value> {
        self.as_ref().and_then(|val| val.data())
    }
}

impl<'a, T, E> Queryable<'a> for Result<T, E>
where
    T: Queryable<'a>,
{
    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        self.as_ref().ok().and_then(|val| val.member(field))
    }

    fn all(&'a self) -> TreeIter<'a> {
        if let Ok(val) = self.as_ref() {
            val.all()
        } else {
            TreeIter::empty()
        }
    }

    fn data(&self) -> Option<Value> {
        self.as_ref().ok().and_then(|val| val.data())
    }
}

impl<'a> Queryable<'a> for std::time::Duration {
    fn data(&self) -> Option<Value> {
        Some(self.as_secs().into())
    }
}

impl<'a, T> Queryable<'a> for std::cmp::Reverse<T>
where
    T: Queryable<'a>,
{
    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        self.0.member(field)
    }

    fn all(&'a self) -> TreeIter<'a> {
        self.0.all()
    }

    fn data(&self) -> Option<Value> {
        self.0.data()
    }
}

macro_rules! impl_tuples {
    ( => ) => {};
    ($t: ident $(,$other_t: ident)* => $ind: tt $(,$other_ind: tt)*) => {
        impl_tuples!(@ $t $(, $other_t)* ($ind) => $ind $(, $other_ind)*);
        impl_tuples!($($other_t),* => $($other_ind),*);
    };
    (@ $($t: ident),+ ($len: tt) => $($ind: tt),+) => {
        impl<'a $(,$t)+> Queryable<'a> for ($($t,)+)
        where
            $($t: Queryable<'a>),+
        {
            fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
                #[allow(clippy::collapsible_match)]
                match field {
                    Value::Int(i) => {
                        match i {
                            $(
                                $ind => Some(&self.$ind as _),
                            )+
                            _ => None
                        }
                    }
                    _ => None
                }
            }

            fn all(&'a self) -> TreeIter<'a> {
                TreeIter(Box::from(vec![$(&self.$ind as _),+].into_iter()))
            }

            fn data(&self) -> Option<Value> {
                None
            }
        }

        impl<$($t),+> TryFrom<Value> for ($($t,)+)
        where
            $($t: TryFrom<Value>),+
        {
            type Error = ValueConvertError;
            fn try_from(value: Value) -> Result<Self, ValueConvertError> {
                match value {
                    #[allow(unused_comparisons)]
                    Value::Array(arr) if arr.len() >= $len => {
                        let mut iter = arr.into_iter();
                        Ok((
                            $(
                                $t::try_from(iter.next().ok_or(ValueConvertError)?)
                                    .map_err(|_| ValueConvertError)?,
                            )+
                        ))
                    }
                    _ => Err(ValueConvertError),
                }
            }
        }
    };
}

impl_tuples!(A, B, C, D, E, F, G, H, I, J, K, L => 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0);

macro_rules! impl_arrays {
    () => {};
    ($size: tt $(,$other_ind: tt)*) => {
        impl_arrays!(@ $size $(, $other_ind)*);
        impl_arrays!($($other_ind),*);
    };
    (@ $size: tt $(,$ind: tt)*) => {
        impl<'a, T> Queryable<'a> for [T; $size]
        where
            T: Queryable<'a>
        {
            fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
                #[allow(clippy::collapsible_match)]
                match field {
                    Value::Int(i) => {
                        match i {
                            $(
                                $ind => Some(&self[$ind] as _),
                            )*
                            _ => None
                        }
                    }
                    _ => None
                }
            }

            fn all(&'a self) -> TreeIter<'a> {
                TreeIter(Box::from(vec![$(&self[$ind] as _),*].into_iter()))
            }

            fn data(&self) -> Option<Value> {
                None
            }
        }

        impl<T> TryFrom<Value> for [T; $size]
        where
            T: TryFrom<Value>,
        {
            type Error = ValueConvertError;
            fn try_from(value: Value) -> Result<Self, ValueConvertError> {
                match value {
                    #[allow(unused_comparisons)]
                    Value::Array(arr) if arr.len() >= $size => {
                        #[allow(unused_variables, unused_mut)]
                        let mut iter = arr.into_iter();
                        Ok([
                            $(
                                impl_arrays!(@foreach ($ind) =>
                                    T::try_from(iter.next().ok_or(ValueConvertError)?)
                                        .map_err(|_| ValueConvertError)?
                                )
                            ),*
                        ])
                    }
                    _ => Err(ValueConvertError),
                }
            }
        }
    };
    (@foreach ($($counter: tt)*) => $($output: tt)*) => {
        $($output)*
    };
}

impl_arrays!(
    32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9,
    8, 7, 6, 5, 4, 3, 2, 1, 0
);

impl<'a, T> Queryable<'a> for PhantomData<T> {}
