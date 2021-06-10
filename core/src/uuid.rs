use super::{Error, Queryable, Value};
use std::convert::TryFrom;
use std::str::FromStr;
use uuid::Uuid;

impl<'a> Queryable<'a> for Uuid {
    fn name(&self) -> &'static str {
        "Uuid"
    }
    fn data(&self) -> Option<Value> {
        Some(Value::String(format!("{}", self)))
    }
}

impl TryFrom<Value> for Uuid {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Uuid::from_str(&s).map_err(|_| Error::TypeError),
            _ => Err(Error::TypeError),
        }
    }
}

impl From<Uuid> for Value {
    fn from(uuid: Uuid) -> Value {
        Value::String(uuid.to_string())
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
