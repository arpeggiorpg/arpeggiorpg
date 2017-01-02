// use std::fmt;

use serde::{Serialize, Serializer, Deserialize, Deserializer};
use serde;
#[cfg(test)]
use serde_json;
#[cfg(test)]
use serde_json::error as SJE;

#[derive(Clone, Eq, PartialEq, Debug)]
// TODO: Serialize as {"cursor": 0, "data": [...]} and deserialize same, and ensure that data is
// always non-empty
pub struct NonEmptyWithCursor<T> {
    most: Vec<T>,
    head: T,
    cursor: usize,
}

impl<T> NonEmptyWithCursor<T> {
    pub fn new(head: T) -> NonEmptyWithCursor<T> {
        NonEmptyWithCursor {
            most: vec![],
            head: head,
            cursor: 0,
        }
    }
    pub fn get_current(&self) -> &T {
        if self.cursor == 0 {
            &self.head
        } else {
            &self.most[self.cursor - 1]
        }
    }

    pub fn push(&mut self, t: T) {
        self.most.push(t)
    }

    pub fn set_cursor(&mut self, cursor: usize) -> Option<()> {
        if self.most.len() < cursor {
            None
        } else {
            self.cursor = cursor;
            Some(())
        }
    }
    pub fn get_cursor(&self) -> usize {
        self.cursor
    }

    pub fn len(&self) -> usize {
        1 + self.most.len()
    }
}

impl<T> Serialize for NonEmptyWithCursor<T>
    where T: Serialize
{
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: Serializer
    {
        let mut state = serializer.serialize_map(Some(2))?;
        serializer.serialize_map_key(&mut state, "cursor")?;
        serializer.serialize_map_value(&mut state, self.cursor)?;
        serializer.serialize_map_key(&mut state, "data")?;
        let mut seqstate = serializer.serialize_seq(Some(self.len()))?;
        serializer.serialize_seq_elt(&mut seqstate, &self.head)?;
        for elt in self.most.iter() {
            serializer.serialize_seq_elt(&mut seqstate, elt)?;
        }
        serializer.serialize_seq_end(seqstate)?;
        serializer.serialize_map_end(state)
    }
}


#[derive(Deserialize)]
struct SerializedNE<T> {
    cursor: usize,
    data: Vec<T>,
}

// Can't figure out how to use custom error types in serde deserialization

// #[derive(Eq, PartialEq, Debug)]
// pub enum DeserializeNonEmptyError {
//     InvalidCursor(usize, usize),
//     Empty,
// }
//
// impl fmt::Display for DeserializeNonEmptyError {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             &DeserializeNonEmptyError::InvalidCursor(cursor, size) => {
//                 write!(f,
//                        "Cursor of {} should not be larger than length {}",
//                        cursor,
//                        size)
//             }
//             &DeserializeNonEmptyError::Empty => write!(f, "data must have at least one element"),
//         }
//     }
// }

impl<T> Deserialize for NonEmptyWithCursor<T>
    where T: Deserialize
{
    fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error>
        where D: Deserializer
    {
        let mut x: SerializedNE<T> = Deserialize::deserialize(deserializer)?;
        if x.data.len() == 0 {
            return Err(serde::de::Error::invalid_length(0));
        }
        let most: Vec<T> = x.data.drain(1..).collect();
        if x.cursor > most.len() {
            return Err(serde::de::Error::invalid_value(&format!("Cursor of {} should not be \
                                                                larger than length {}",
                                                                x.cursor,
                                                                most.len() + 1)));
        }
        let res: NonEmptyWithCursor<T> = NonEmptyWithCursor {
            cursor: x.cursor,
            head: x.data.pop().unwrap(),
            most: most,
        };
        Ok(res)

    }
}


#[test]
fn test_nonempty_set_cursor() {
    let mut ne: NonEmptyWithCursor<i32> = NonEmptyWithCursor::new(1);
    assert_eq!(ne.get_cursor(), 0);
    assert_eq!(ne.get_current(), &1);

    assert_eq!(ne.set_cursor(0), Some(()));
    assert_eq!(ne.get_cursor(), 0);
    assert_eq!(ne.get_current(), &1);

    assert_eq!(ne.set_cursor(1), None);
    assert_eq!(ne.get_cursor(), 0);
    assert_eq!(ne.get_current(), &1);

    ne.push(5);
    assert_eq!(ne.get_cursor(), 0);
    assert_eq!(ne.get_current(), &1);

    assert_eq!(ne.set_cursor(1), Some(()));
    assert_eq!(ne.get_cursor(), 1);
    assert_eq!(ne.get_current(), &5);
}


#[test]
fn test_nonempty_serialize_deserialize() {
    let mut ne: NonEmptyWithCursor<i32> = NonEmptyWithCursor::new(5);
    ne.push(50);
    ne.push(55);
    assert_eq!(serde_json::to_string(&ne).unwrap(),
               "{\"cursor\":0,\"data\":[5,50,55]}");
    match serde_json::from_str("{\"cursor\":0,\"data\":[5,50,55]}") {
        Ok(ne2) => assert_eq!(ne, ne2),
        Err(e) => panic!("Couldn't parse json: {}", e),
    }
}

#[test]
fn test_deserialize_invalid_cursor() {
    let res: Result<NonEmptyWithCursor<i32>, _> = serde_json::from_str("{\"cursor\":2,\"data\":\
                                                                        [5]}");
    let exmsg = "Cursor of 2 should not be larger than length 1";
    match res {
        Ok(x) => panic!("Should not have parsed to {:?}", x),
        // TODO: position here is 0, 0 because our parser is dumb.
        Err(SJE::Error::Syntax(SJE::ErrorCode::InvalidValue(ref msg), 0, 0)) if msg == exmsg => {}
        Err(e) => panic!("Should not have got any other error: {:?}", e),
    }
}

#[test]
fn test_deserialize_invalid_empty() {
    let res: Result<NonEmptyWithCursor<i32>, _> = serde_json::from_str("{\"cursor\":0,\"data\":\
                                                                        []}");
    match res {
        Ok(x) => panic!("Should not have parsed to {:?}", x),
        // TODO: position here is 0, 0 because our parser is dumb.
        Err(SJE::Error::Syntax(SJE::ErrorCode::InvalidLength(0), 0, 0)) => {}
        Err(e) => panic!("Should not have got any other error: {}", e),
    }
}
