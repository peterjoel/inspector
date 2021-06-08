use crate::{Array, Error, NodeOrValueIter, Queryable, Value, ValueIter};
use std::borrow::Cow;
use std::collections::*;
use std::convert::{TryFrom, TryInto};
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

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
                type Error = Error;
                fn try_from(value: Value) -> Result<Self, Self::Error> {
                    Self::try_from(&value)
                }
            }

            impl TryFrom<&Value> for $ty {
                type Error = Error;
                fn try_from(value: &Value) -> Result<Self, Self::Error> {
                    match value {
                        &Value::Int(v) => {
                            $( let v: $via = v.try_into().map_err(|_|Error::TypeError)?; )?
                            v.try_into().map_err(|_| Error::TypeError)
                        },
                        Value::String(s) => s.parse().map_err(|_| Error::TypeError),
                        &Value::Bool(b) => if b {
                            1.try_into().map_err(|_| Error::TypeError)
                        } else {
                            0.try_into().map_err(|_| Error::TypeError)
                        }
                        Value::Array(_) => Err(Error::TypeError),
                        &Value::Float(f) => if *f >= i64::min_value() as f64 && *f <= i64::max_value() as f64 {
                            <$ty>::try_from(Value::Int(*f as i64))
                        } else {
                            Err(Error::TypeError)
                        }
                    }
                }
            }

            impl<'a> Queryable<'a> for $ty {
                fn name(&self) -> &'static str {
                    stringify!($ty)
                }
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
                type Error = Error;
                fn try_from(value: Value) -> Result<Self, Self::Error> {
                    Self::try_from(&value)
                }
            }

            impl TryFrom<&Value> for $ty {
                type Error = Error;
                fn try_from(value: &Value) -> Result<Self, Self::Error> {
                    match value {
                        &Value::Int(v) => {
                            Ok((v as $ty).into())
                        },
                        Value::String(s) => s.parse().map_err(|_| Error::TypeError),
                        &Value::Bool(b) => if b {
                            Ok(1.0)
                        } else {
                            Ok(0.0)
                        }
                        Value::Array(_) => Err(Error::TypeError),
                        &Value::Float(f) => if *f >= $ty::MIN as f64 && *f <= $ty::MAX as f64 {
                            Ok(*f as $ty)
                        } else {
                            Err(Error::TypeError)
                        }
                    }
                }
            }

            impl<'a> Queryable<'a> for $ty {
                fn name(&self) -> &'static str {
                    stringify!($ty)
                }
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
    type Error = Error;
    fn try_from(v: Value) -> Result<String, Error> {
        match v {
            Value::Int(i) => Ok(i.to_string()),
            Value::String(s) => Ok(s),
            Value::Bool(b) => Ok(b.to_string()),
            Value::Array(arr) => {
                if arr.0.len() == 1 {
                    Ok(arr.0[0].to_string())
                } else {
                    Err(Error::TypeError)
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

macro_rules! into_queryable {
    ($ty: ty) => {
        impl<'a> Queryable<'a> for $ty {
            fn name(&self) -> &'static str {
                stringify!($ty)
            }
            fn data(&self) -> Option<Value> {
                Some(self.into())
            }
        }
        impl<'a> Queryable<'a> for &$ty {
            fn name(&self) -> &'static str {
                stringify!($ty)
            }
            fn data(&self) -> Option<Value> {
                Some((*self).into())
            }
        }
    };
}

into_queryable!(bool);
into_queryable!(str);
into_queryable!(String);

impl<'q, K, V> Queryable<'q> for HashMap<K, V>
where
    K: Queryable<'q> + TryFrom<Value> + Into<Value> + Hash + Eq + Clone,
    V: Queryable<'q>,
{
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        let key = K::try_from(field.clone()).ok()?;
        self.get(&key).map(|v| v as _)
    }
    fn keys(&self) -> ValueIter<'_> {
        ValueIter::from_values(self.keys().cloned().map(Into::into))
    }
    fn name(&self) -> &'static str {
        "HashMap"
    }
    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        NodeOrValueIter::from_queryables(self.values())
    }
}

impl<'q, K, V> Queryable<'q> for BTreeMap<K, V>
where
    K: Queryable<'q> + TryFrom<Value> + Into<Value> + Clone + Ord + Eq,
    V: Queryable<'q>,
{
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        let key = K::try_from(field.clone()).ok()?;
        self.get(&key).map(|v| v as _)
    }
    fn keys(&self) -> ValueIter<'_> {
        ValueIter::from_values(self.keys().cloned().map(Into::into))
    }
    fn name(&self) -> &'static str {
        "BTreeMap"
    }

    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        NodeOrValueIter::from_queryables(self.values())
    }
}

impl<'q, V> Queryable<'q> for BTreeSet<V>
where
    V: Queryable<'q> + Ord + Eq,
{
    fn name(&self) -> &'static str {
        "BTreeSet"
    }
    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        NodeOrValueIter::from_queryables(self)
    }
}

impl<'q, V> Queryable<'q> for HashSet<V>
where
    V: Queryable<'q> + Hash + Eq,
{
    fn name(&self) -> &'static str {
        "HashSet"
    }
    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        NodeOrValueIter::from_queryables(self)
    }
}

impl<'q, T> Queryable<'q> for Vec<T>
where
    T: Queryable<'q>,
{
    fn name(&self) -> &'static str {
        "Vec"
    }
    fn keys(&self) -> ValueIter<'_> {
        ValueIter::from_values(0..self.len())
    }
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        let index = usize::try_from(field).ok()?;
        self.get(index).map(|v| v as _)
    }

    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        NodeOrValueIter::from_queryables(self)
    }
}

impl<'q, T> Queryable<'q> for VecDeque<T>
where
    T: Queryable<'q>,
{
    fn name(&self) -> &'static str {
        "VecDeque"
    }
    fn keys(&self) -> ValueIter<'_> {
        ValueIter::from_values(0..self.len())
    }
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        let index = usize::try_from(field).ok()?;
        self.get(index).map(|v| v as _)
    }

    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        NodeOrValueIter::from_queryables(self)
    }
}

impl<'q, T> Queryable<'q> for Box<T>
where
    T: Queryable<'q>,
{
    fn name(&self) -> &'static str {
        self.as_ref().name()
    }
    fn keys(&self) -> ValueIter<'_> {
        self.as_ref().keys()
    }
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        self.as_ref().member(field)
    }

    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        self.as_ref().all()
    }

    fn data(&self) -> Option<Value> {
        self.as_ref().data()
    }
}

impl<'q, T> Queryable<'q> for Option<T>
where
    T: Queryable<'q>,
{
    fn name(&self) -> &'static str {
        "Option"
    }
    fn keys(&self) -> ValueIter<'_> {
        if let Some(v) = self {
            v.keys()
        } else {
            ValueIter::empty()
        }
    }
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        self.as_ref().and_then(|val| val.member(field))
    }

    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        if let Some(val) = self.as_ref() {
            val.all()
        } else {
            NodeOrValueIter::empty()
        }
    }

    fn data(&self) -> Option<Value> {
        self.as_ref().and_then(|val| val.data())
    }
}

impl<'q, T, E> Queryable<'q> for Result<T, E>
where
    T: Queryable<'q>,
{
    fn name(&self) -> &'static str {
        "Result"
    }
    fn keys(&self) -> ValueIter<'_> {
        if let Ok(v) = self {
            v.keys()
        } else {
            ValueIter::empty()
        }
    }
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        self.as_ref().ok().and_then(|val| val.member(field))
    }

    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        if let Ok(val) = self.as_ref() {
            val.all()
        } else {
            NodeOrValueIter::empty()
        }
    }

    fn data(&self) -> Option<Value> {
        self.as_ref().ok().and_then(|val| val.data())
    }
}

impl<'q> Queryable<'q> for std::time::Duration {
    fn name(&self) -> &'static str {
        "Duration"
    }
    fn data(&self) -> Option<Value> {
        Some(self.as_secs().into())
    }
}

impl<'q, T> Queryable<'q> for std::cmp::Reverse<T>
where
    T: Queryable<'q>,
{
    fn keys(&self) -> ValueIter<'_> {
        self.0.keys()
    }
    fn name(&self) -> &'static str {
        self.0.name()
    }
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        self.0.member(field)
    }

    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
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
        impl<'q $(,$t)+> Queryable<'q> for ($($t,)+)
        where
            $($t: Queryable<'q>),+
        {
            fn keys(&self) -> ValueIter<'_> {
                ValueIter::from_values(0..$len)
            }
            fn name(&self) -> &'static str {
                stringify!(($($t,)+))
            }
            fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
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

            fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
                NodeOrValueIter::from_nodes(vec![$(&self.$ind as _),+])
            }
        }

        impl<$($t),+> TryFrom<Value> for ($($t,)+)
        where
            $($t: TryFrom<Value>),+
        {
            type Error = Error;
            fn try_from(value: Value) -> Result<Self, Error> {
                match value {
                    #[allow(unused_comparisons)]
                    Value::Array(arr) if arr.len() >= $len => {
                        let mut iter = arr.into_iter();
                        Ok((
                            $(
                                $t::try_from(iter.next().ok_or(Error::TypeError)?)
                                    .map_err(|_| Error::TypeError)?,
                            )+
                        ))
                    }
                    _ => Err(Error::TypeError),
                }
            }
        }

        impl<$($t),+> From<($($t,)+)> for Value
        where
            $($t: Into<Value>),+
        {
            fn from(v: ($($t,)+)) -> Self {
                let mut values = vec![$((v.$ind.into()),)+];
                // TODO: something more efficient than this
                values.reverse();
                Value::Array(Array(Rc::from(values)))
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
        impl<'q, T> Queryable<'q> for [T; $size]
        where
            T: Queryable<'q>
        {
            fn keys(&self) -> ValueIter<'_> {
                ValueIter::from_values(0..$size)
            }
            fn name(&self) -> &'static str {
                stringify!([T; $size])
            }
            fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
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

            fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
                NodeOrValueIter::from_nodes(vec![$(&self[$ind] as _),*])
            }
        }

        impl<T> TryFrom<Value> for [T; $size]
        where
            T: TryFrom<Value>,
        {
            type Error = Error;
            fn try_from(value: Value) -> Result<Self, Error> {
                match value {
                    #[allow(unused_comparisons)]
                    Value::Array(arr) if arr.len() >= $size => {
                        #[allow(unused_variables, unused_mut)]
                        let mut iter = arr.into_iter();
                        Ok([
                            $(
                                impl_arrays!(@foreach ($ind) =>
                                    T::try_from(iter.next().ok_or(Error::TypeError  )?)
                                        .map_err(|_| Error::TypeError   )?
                                )
                            ),*
                        ])
                    }
                    _ => Err(Error::TypeError   ),
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

impl<'a, T> Queryable<'a> for PhantomData<T> {
    fn name(&self) -> &'static str {
        "PhantomData"
    }
}
