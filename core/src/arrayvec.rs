use super::{NodeOrValueIter, Queryable, Value};
use arrayvec::{Array, ArrayVec};
use std::convert::TryFrom;

impl<'q, T> Queryable<'q> for ArrayVec<T>
where
    T: Array,
    <T as Array>::Item: Queryable<'q>,
{
    fn name(&self) -> &'static str {
        "ArrayVec"
    }
    fn member<'a, 'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'q>> {
        let index = usize::try_from(field).ok()?;
        self.get(index).map(|v| v as _)
    }
    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        NodeOrValueIter::from_queryables(self)
    }
}
