use super::{Queryable, Value, ValueConvertError};
use std::convert::TryFrom;
use std::str::FromStr;
use uuid::Uuid;

impl<'a> Queryable<'a> for Uuid {
    fn data(&self) -> Option<Value> {
        Some(Value::String(format!("{}", self)))
    }
}

impl TryFrom<Value> for Uuid {
    type Error = ValueConvertError;
    fn try_from(value: Value) -> Result<Self, ValueConvertError> {
        match value {
            Value::String(s) => Uuid::from_str(&s).map_err(|_| ValueConvertError),
            _ => Err(ValueConvertError),
        }
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
