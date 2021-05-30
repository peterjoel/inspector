use super::{Queryable, Value};
use uuid::Uuid;

impl<'a> Queryable<'a> for Uuid {
    fn data(&self) -> Option<Value> {
        Some(Value::String(format!("{}", self)))
    }
}

#[test]
fn test_uuid() {
    use super::*;
    let id = Uuid::from_u128(1);

    assert_eq!(
        id.data(),
        Some(Value::from("00000000-0000-0000-0000-000000000001"))
    );
}
