use super::{Queryable, Value};
use arrayvec::{Array, ArrayVec};

impl<'a, T> Queryable<'a> for ArrayVec<T>
where
    T: Array,
    <T as Array>::Item: Queryable<'a>,
{
    fn keys(&'a self) -> Box<dyn Iterator<Item = Value> + 'a> {
        Box::from((0..self.len()).map(Value::from))
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
    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::from(self.iter().map(|v| v as _))
    }
    fn data(&self) -> Option<Value> {
        None
    }
}
