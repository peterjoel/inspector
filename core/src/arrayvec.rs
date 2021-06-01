use super::{Queryable, TreeIter, Value};
use arrayvec::{Array, ArrayVec};

impl<'a, T> Queryable<'a> for ArrayVec<T>
where
    T: Array,
    <T as Array>::Item: Queryable<'a>,
{
    fn name(&self) -> &'static str {
        "ArrayVec"
    }
    fn member<'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'a>> {
        match field {
            &Value::Int(i) if i >= 0 => self.get(i as usize).map(|val| val as _),
            Value::String(s) => {
                if let Ok(i) = s.parse::<usize>() {
                    self.get(i).map(|val| val as _)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    fn all(&'a self) -> TreeIter<'a> {
        TreeIter::from_queryables(self)
    }
}
