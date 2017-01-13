//! Contains `SmallString`, a 32-byte stack-allocated string.
use string_wrapper::StringWrapper;
use serde;

// this module is total bikeshedding and completely unnecessary, I only wrote it for the
// experience.

/// SmallString is a 32-byte stack-allocated string which implements Copy and
/// serde's Serialize/Deserialize traits.
/// Hopefully StringWrapper will get good enough to obviate this type.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct SmallString(StringWrapper<[u8; 32]>);
impl SmallString {
    pub fn new(string: &str) -> Self {
        let mut s = StringWrapper::new([0u8; 32]);
        s.push_str(string);
        SmallString(s)
    }

    pub fn to_string(&self) -> String {
        String::from_utf8(self.0.buffer().to_vec()).unwrap()
    }
}

impl serde::Serialize for SmallString {
    fn serialize<S: serde::Serializer>(&self, serializer: &mut S) -> Result<(), S::Error> {
        self.to_string().serialize(serializer)
    }
}

impl serde::Deserialize for SmallString {
    fn deserialize<D: serde::Deserializer>(deserializer: &mut D) -> Result<Self, D::Error> {
        let s: String = serde::Deserialize::deserialize(deserializer)?;
        Ok(SmallString::new(&s))
    }
}
