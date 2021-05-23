use std::collections::*;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter;
use std::str::FromStr;
use velcro::vec;
use velcro::*;

fn main() {
    let root = hash_map! {
        1: vec![btree_map!{ 55 : 1}, btree_map!{ 66 : 1, 100: 3}],
        2: vec![],
        8: vec![btree_map!{ 4: 99}, btree_map!{ 6: 1110, 7: 1}],
    };

    // root/8/1/6
    let x = root
        .member("8")
        .and_then(|v| v.member("1"))
        .and_then(|v| v.member("6"));

    println!("x = {:?}", x.unwrap().data());
}

trait Queryable<'a> {
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(iter::empty())
    }
    fn member(&'a self, _field: &str) -> Option<&'a dyn Queryable<'a>> {
        None
    }
    fn data(&self) -> Option<String> {
        None
    }
    fn display(&self) -> Option<String> {
        None
    }
}

impl<'a> Queryable<'a> for i32 {
    fn data(&self) -> Option<String> {
        Some(self.to_string())
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}

impl<'a> Queryable<'a> for &str {
    fn data(&self) -> Option<String> {
        Some(self.to_string())
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}

impl<'a> Queryable<'a> for String {
    fn data(&self) -> Option<String> {
        Some(self.to_string())
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}

impl<'a, K: Queryable<'a> + Hash + Eq + FromStr + Debug, V: Queryable<'a>> Queryable<'a>
    for HashMap<K, V>
where
    <K as FromStr>::Err: Debug,
{
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(self.keys().filter_map(|k| k.display()))
    }
    fn member(&'a self, field: &str) -> Option<&'a dyn Queryable<'a>> {
        K::from_str(field)
            .ok()
            .and_then(|field| self.get(&field))
            .map(|v| v as _)
    }
}

impl<'a, K: Queryable<'a> + Ord + Eq + FromStr, V: Queryable<'a>> Queryable<'a> for BTreeMap<K, V> {
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(self.keys().filter_map(|k| k.display()))
    }
    fn member(&'a self, field: &str) -> Option<&'a dyn Queryable<'a>> {
        K::from_str(field)
            .ok()
            .and_then(|field| self.get(&field))
            .map(|v| v as _)
    }
}

impl<'a, T: Queryable<'a>> Queryable<'a> for Vec<T> {
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from((0..self.len()).map(|v| v.to_string()))
    }
    fn member(&'a self, field: &str) -> Option<&'a dyn Queryable<'a>> {
        usize::from_str(field)
            .ok()
            .and_then(|index| self.get(index))
            .map(|v| v as _)
    }
}
