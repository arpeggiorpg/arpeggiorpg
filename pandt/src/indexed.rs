use std::collections::HashMap;
use std::fmt;
use std::hash;

use serde::de;
use serde::ser;


pub trait DeriveKey {
  type KeyType: hash::Hash + Eq;
  fn derive_key(&self) -> Self::KeyType;
}

#[derive(Eq, PartialEq)]
pub struct IndexedHashMap<V: DeriveKey> {
  data: HashMap<<V as DeriveKey>::KeyType, V>,
}

impl<V> Clone for IndexedHashMap<V>
  where V: DeriveKey + Clone,
        <V as DeriveKey>::KeyType: Clone
{
  fn clone(&self) -> Self {
    IndexedHashMap { data: self.data.clone() }
  }
}

impl<V> fmt::Debug for IndexedHashMap<V>
  where V: DeriveKey + fmt::Debug,
        <V as DeriveKey>::KeyType: fmt::Debug
{
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    self.data.fmt(f)
  }
}

impl<V> ser::Serialize for IndexedHashMap<V>
  where V: DeriveKey + ser::Serialize,
        <V as DeriveKey>::KeyType: ser::Serialize
{
  fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where S: ser::Serializer
  {
    self.data.serialize(s)
  }
}

impl<V> de::Deserialize for IndexedHashMap<V>
  where V: DeriveKey + de::Deserialize,
        <V as DeriveKey>::KeyType: de::Deserialize
{
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: de::Deserializer
  {
    let hm: HashMap<<V as DeriveKey>::KeyType, V> = de::Deserialize::deserialize(deserializer)?;
    Ok(IndexedHashMap { data: hm })
  }
}

// Deserialize

impl<V: DeriveKey> IndexedHashMap<V> {
  pub fn new() -> IndexedHashMap<V> {
    IndexedHashMap { data: HashMap::new() }
  }

  fn insert(&mut self, v: V) -> Option<V> {
    self.data.insert(v.derive_key(), v)
  }

  fn get<'a>(&'a self, k: &<V as DeriveKey>::KeyType) -> Option<&'a V> {
    self.data.get(k)
  }

  fn remove<Q: ?Sized>(&mut self, k: &<V as DeriveKey>::KeyType) -> Option<V> {
    self.data.remove(k)
  }
}
