//! Quick-n-dirty type to allow floats to be used in things like HashMaps
//! and generally not be a pain in the arse.
//! For the purpose of ordering. all NaN values are considered to be less
//! than -infinity. NaN values are equal to each other and have the same hash.
//! Note that it is NOT OK to implement `Borrow<f64>` for this type. That
//! would make things like HashMaps unsound.

use std::cmp::{Eq, Ord, Ordering};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

#[derive(Clone, Copy)]
pub struct WellBehavedF64(f64);

// Chosen by a series of fair dice rolls. Guaranteed to be random.
const NAN_HASH_VAL: &[u8; 32] = b"ab3ec7e21d3aeea1ae0e06dc759f89a6";

impl Hash for WellBehavedF64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.0.is_nan() {
            NAN_HASH_VAL.hash(state)
        } else {
            self.0.to_le_bytes().hash(state)
        }
    }
}

impl PartialEq for WellBehavedF64 {
    fn eq(&self, other: &Self) -> bool {
        if self.0.is_nan() && other.0.is_nan() {
            true
        } else {
            self.0.eq(&other.0)
        }
    }
}

impl Eq for WellBehavedF64 {}

impl PartialOrd for WellBehavedF64 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WellBehavedF64 {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.0.is_nan(), other.0.is_nan()) {
            (true, true) => Ordering::Equal,
            (false, false) => self.0.partial_cmp(&other.0).unwrap(),
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
        }
    }
}

impl AsRef<f64> for WellBehavedF64 {
    fn as_ref(&self) -> &f64 {
        &self.0
    }
}

impl Deref for WellBehavedF64 {
    type Target = f64;
    fn deref(&self) -> &f64 {
        &self.0
    }
}

impl DerefMut for WellBehavedF64 {
    fn deref_mut(&mut self) -> &mut f64 {
        &mut self.0
    }
}

impl fmt::Debug for WellBehavedF64 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl fmt::Display for WellBehavedF64 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl From<f64> for WellBehavedF64 {
    fn from(float: f64) -> Self {
        Self(float)
    }
}

impl From<f32> for WellBehavedF64 {
    fn from(float: f32) -> Self {
        Self(float as f64)
    }
}
