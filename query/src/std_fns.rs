use super::{TreeIter, Value, ValueIter};

pub fn count<'a>(input: TreeIter<'a>) -> ValueIter<'a> {
    ValueIter::once(input.count())
}

pub fn sum<'a>(input: TreeIter<'a>) -> ValueIter<'a> {
    ValueIter::once(
        input
            .filter_map(|q| q.data())
            .filter_map(Value::into_int)
            .sum::<i64>(),
    )
}
