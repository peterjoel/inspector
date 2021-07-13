use super::{Node, NodeIter, Queryable, Value, ValueIter};
use arrayvec::{Array, ArrayVec};
use std::convert::TryFrom;

impl<T> Queryable for ArrayVec<T>
where
    T: Array,
    <T as Array>::Item: Queryable,
{
    fn name(&self) -> &'static str {
        "ArrayVec"
    }
    fn keys(&self) -> ValueIter<'_> {
        ValueIter::from_values(0..self.len())
    }
    fn member<'f>(&self, field: &'f Value) -> Option<Node<'_>> {
        let index = usize::try_from(field).ok()?;
        self.get(index).map(|v| Node::Queryable(v as _))
    }
    fn all(&self) -> NodeIter<'_> {
        NodeIter::from_queryables(self)
    }
}
