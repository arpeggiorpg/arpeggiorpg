#![deny(missing_docs)]
//! A non-empty vector with a cursor.

#[cfg(feature="use_serde")]
#[macro_use]
extern crate serde_derive;
#[cfg(feature="use_serde")]
extern crate serde;
#[cfg(feature="use_serde")]
#[cfg(test)]
extern crate serde_json;

use std::error;
use std::fmt;

#[cfg(feature="use_serde")]
use serde::{Deserialize, Deserializer};
#[cfg(feature="use_serde")]
#[cfg(test)]
use serde_json::error as SJE;


/// A non-empty vector with a cursor. NO operations panic.
#[derive(Clone, Eq, PartialEq, Debug)]
#[cfg_attr(feature="use_serde", derive(Serialize))]
pub struct NonEmptyWithCursor<T> {
    cursor: usize,
    data: NonEmpty<T>,
}


impl<T> NonEmptyWithCursor<T> {
    // *** Cursor methods
    /// Create a new NonEmptyWithCursor with a single element and cursor set to 0.
    #[inline]
    pub fn new(head: T) -> Self {
        NonEmptyWithCursor {
            cursor: 0,
            data: NonEmpty::new(head),
        }
    }

    /// Construct a new NonEmptyWithCursor from the first element and a vector of the rest of the
    /// elements.
    #[inline]
    pub fn new_with_rest(head: T, rest: Vec<T>) -> Self {
        NonEmptyWithCursor {
            cursor: 0,
            data: NonEmpty::new_with_rest(head, rest),
        }
    }

    /// Construct a NonEmptyWithCursor from a vector. Returns None if the vector is empty.
    #[inline]
    pub fn from_vec(vec: Vec<T>) -> Option<Self> {
        NonEmpty::from_vec(vec).map(|ne| {
            NonEmptyWithCursor {
                cursor: 0,
                data: ne,
            }
        })
    }

    /// Get the current element, as determined by the cursor.
    #[inline]
    pub fn get_current(&self) -> &T {
        self.data.get(self.cursor).unwrap()
    }

    /// Get a mutable reference to the current element.
    #[inline]
    pub fn get_current_mut(&mut self) -> &mut T {
        let i = self.cursor;
        self.data.get_mut(i).unwrap()
    }

    /// Set the cursor. Returns None if the cursor is out of bounds.
    #[inline]
    pub fn set_cursor(&mut self, cursor: usize) -> Option<()> {
        if self.data.len() > cursor {
            self.cursor = cursor;
            Some(())
        } else {
            None
        }
    }

    /// Increment the cursor by one, and wrap around to 0 if it goes past the end of the vector.
    #[inline]
    pub fn next_circular(&mut self) {
        let newcursor = self.cursor + 1;
        self.cursor = if newcursor >= self.data.len() {
            0
        } else {
            newcursor
        }
    }

    /// Get the current cursor.
    #[inline]
    pub fn get_cursor(&self) -> usize {
        self.cursor
    }

    /// Remove an element by index, adjusting the cursor so that it points at the same element.
    ///
    /// # Examples
    ///
    /// ```
    /// use nonempty::NonEmptyWithCursor;
    /// let mut ne = NonEmptyWithCursor::new_with_rest(1, vec![2, 3, 4]);
    /// ne.remove(1).unwrap();
    /// assert_eq!(ne.get_cursor(), 0); // No adjustment needed for cursor
    /// assert_eq!(ne, NonEmptyWithCursor::new_with_rest(1, vec![3, 4]));
    /// ```
    ///
    /// ```
    /// use nonempty::NonEmptyWithCursor;
    /// let mut ne = NonEmptyWithCursor::new_with_rest(1, vec![2, 3, 4]);
    /// ne.set_cursor(1);
    /// ne.remove(0).unwrap();
    /// assert_eq!(ne.get_cursor(), 0); // Cursor adjusted left
    /// assert_eq!(ne, NonEmptyWithCursor::new_with_rest(2, vec![3, 4]));
    /// ```
    ///
    /// ```
    /// use nonempty::NonEmptyWithCursor;
    /// let mut ne = NonEmptyWithCursor::new_with_rest(1, vec![2, 3, 4]);
    /// ne.set_cursor(1);
    /// ne.remove(1).unwrap();
    /// assert_eq!(ne.get_cursor(), 0); // Cursor remained the same, pointing at the next element
    /// assert_eq!(ne, NonEmptyWithCursor::new_with_rest(1, vec![3, 4]));
    /// ```
    pub fn remove(&mut self, index: usize) -> Result<(), Error> {
        if index <= self.cursor {
            self.cursor -= 1;
        }
        self.data.remove(index)
    }

    // *** Pass-through methods
    /// Get the length of the underlying non-empty vector.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Iterate over the elements, providing &T.
    #[inline]
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.data.iter()
    }

    /// Iterate over the elements, providing &mut T.
    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> {
        self.data.iter_mut()
    }

    /// Get an immutable reference to an arbitrary element, by index.
    #[inline]
    pub fn get(&self, idx: usize) -> Option<&T> {
        self.data.get(idx)
    }

    /// Get a mutable reference to an arbitrary element, by index.
    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        self.data.get_mut(idx)
    }

    /// Append an element.
    #[inline]
    pub fn push(&mut self, t: T) {
        self.data.push(t)
    }
}

/// A non-empty vector. NO operations panic.
// The canonical representation is something like (A, Vec<A>), but this layout actually makes
// it MUCH easier to implement the various methods, and perhaps more optimized.
#[derive(Clone, Eq, PartialEq, Debug)]
#[cfg_attr(feature="use_serde", derive(Serialize))]
pub struct NonEmpty<T>(Vec<T>);

impl<T> NonEmpty<T> {
    /// Construct a new NonEmpty. The first element is necessary.
    #[inline]
    pub fn new(head: T) -> Self {
        NonEmpty(vec![head])
    }

    /// Construct a new NonEmpty from the first element and a vector of the rest of the elements.
    #[inline]
    pub fn new_with_rest(head: T, rest: Vec<T>) -> Self {
        let mut v = vec![head];
        v.extend(rest);
        NonEmpty(v)
    }

    /// Construct a new NonEmpty from a Vec, if it has at least one element.
    #[inline]
    pub fn from_vec(vec: Vec<T>) -> Option<Self> {
        if vec.len() >= 1 {
            Some(NonEmpty(vec))
        } else {
            None
        }
    }

    /// Iterate over the elements, providing &T.
    #[inline]
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.0.iter()
    }

    /// Iterate over the elements, providing &mut T.
    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> {
        self.0.iter_mut()
    }

    /// Get an immutable reference to an arbitrary element, by index.
    #[inline]
    pub fn get(&self, idx: usize) -> Option<&T> {
        self.0.get(idx)
    }

    /// Get a mutable reference to an arbitrary element, by index.
    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        self.0.get_mut(idx)
    }

    /// Append an element.
    #[inline]
    pub fn push(&mut self, t: T) {
        self.0.push(t)
    }

    /// Remove a single element matching a predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use nonempty::{NonEmpty, Error};
    ///
    /// let mut ne = NonEmpty::new_with_rest(1, vec![2]);
    /// // Removing an existing element:
    /// ne.remove(0).unwrap();
    /// assert_eq!(ne, NonEmpty::new(2));
    /// // Removing a non-existent element:
    /// assert_eq!(ne.remove(1), Err(Error::OutOfBounds{index: 1, length: 1}));
    /// // Removing the last element:
    /// assert_eq!(ne.remove(0), Err(Error::RemoveLastElement));
    ///
    /// assert_eq!(ne, NonEmpty::new(2)); // The NonEmpty is unchanged
    /// ```
    pub fn remove(&mut self, idx: usize) -> Result<(), Error> {
        if idx == 0 && self.len() == 1 {
            Err(Error::RemoveLastElement)
        } else if idx >= self.len() {
            Err(Error::OutOfBounds {
                index: idx,
                length: self.len(),
            })
        } else {
            self.0.remove(idx);
            Ok(())
        }
    }

    /// Return the total length.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

/// An Error that may be returned by some operations on NonEmpty.
#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    /// Tried to access an element outside the bounds of the NonEmpty.
    OutOfBounds {
        /// The attempted index
        index: usize,
        /// The length of the NonEmpty
        length: usize,
    },
    /// Tried to remove the last element of the NonEmpty.
    RemoveLastElement,
}

impl fmt::Display for Error {
    fn fmt(&self, fmter: &mut fmt::Formatter) -> fmt::Result {
        write!(fmter, "{}", format!("{:?}", self))
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "A Game Error occurred"
    }
}

// *** Deserializing NonEmptyWithCursor.
// This is way more work than it should be. We just want to *validate* the data after parsing,
// while using the normal parser. It'd be nice to just pass off a derived Deserialize, but we have
// no way to do that.
#[cfg(feature="use_serde")]
#[derive(Deserialize)]
struct FakeNEC<T> {
    cursor: usize,
    data: Vec<T>,
}

#[cfg(feature="use_serde")]
impl<T> Deserialize for NonEmptyWithCursor<T>
    where T: Deserialize
{
    fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error>
        where D: Deserializer
    {
        let x: FakeNEC<T> = Deserialize::deserialize(deserializer)?;
        if x.data.len() == 0 {
            Err(serde::de::Error::invalid_length(0))
        } else if x.cursor >= x.data.len() {
            Err(serde::de::Error::invalid_value(&format!("Cursor of {} out of bounds for vec of \
                                                          length {}",
                                                         x.cursor,
                                                         x.data.len())))
        } else {
            let res: NonEmptyWithCursor<T> = NonEmptyWithCursor {
                cursor: x.cursor,
                data: NonEmpty::from_vec(x.data).unwrap(),
            };
            Ok(res)
        }
    }
}

// *** Likewise for NonEmpty
#[cfg(feature="use_serde")]
impl<T> Deserialize for NonEmpty<T>
    where T: Deserialize
{
    fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error>
        where D: Deserializer
    {
        let x: Vec<T> = Deserialize::deserialize(deserializer)?;
        if x.len() == 0 {
            Err(serde::de::Error::invalid_length(0))
        } else {
            Ok(NonEmpty(x))
        }
    }
}


#[cfg(feature="use_serde")]
#[test]
fn test_serialize_deserialize_nonempty() {
    let ne: NonEmpty<i32> = NonEmpty::new_with_rest(5, vec![50, 55]);
    assert_eq!(serde_json::to_string(&ne).unwrap(), "[5,50,55]");
    let parsed: Result<NonEmpty<i32>, _> = serde_json::from_str("[5,50,55]");
    assert_eq!(parsed.unwrap(), ne);
}

#[cfg(feature="use_serde")]
#[test]
fn test_deserialize_invalid_nonempty() {
    let parsed: Result<NonEmpty<i32>, _> = serde_json::from_str("[]");
    match parsed {
        Ok(x) => panic!("Somehow this parsed: {:?}", x),
        Err(SJE::Error::Syntax(SJE::ErrorCode::InvalidLength(0), 0, 0)) => {}
        _ => panic!("Unexpected error"),
    }
}


#[test]
fn test_set_cursor() {
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

#[cfg(feature="use_serde")]
#[test]
fn test_serialize_deserialize_with_cursor() {
    let ne: NonEmptyWithCursor<i32> = NonEmptyWithCursor::new_with_rest(5, vec![50, 55]);
    assert_eq!(serde_json::to_string(&ne).unwrap(),
               "{\"cursor\":0,\"data\":[5,50,55]}");
    match serde_json::from_str("{\"cursor\":0,\"data\":[5,50,55]}") {
        Ok(ne2) => assert_eq!(ne, ne2),
        Err(e) => panic!("Couldn't parse json: {}", e),
    }
}

#[cfg(feature="use_serde")]
#[test]
fn test_deserialize_invalid_cursor() {
    let res: Result<NonEmptyWithCursor<i32>, _> = serde_json::from_str("{\"cursor\":1,\"data\":\
                                                                        [5]}");
    let exmsg = "Cursor of 1 out of bounds for vec of length 1";
    match res {
        Ok(x) => panic!("Should not have parsed to {:?}", x),
        // TODO: position here is 0, 0 because our parser is dumb.
        Err(SJE::Error::Syntax(SJE::ErrorCode::InvalidValue(ref msg), 0, 0)) if msg == exmsg => {}
        Err(e) => panic!("Should not have got any other error: {:?}", e),
    }
}

#[cfg(feature="use_serde")]
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

#[test]
fn test_iter() {
    let ne: NonEmpty<i32> = NonEmpty::new_with_rest(5, vec![50, 55]);
    let v: Vec<&i32> = ne.iter().collect();
    assert_eq!(v, vec![&5, &50, &55]);
}


#[cfg(test)]
#[derive(Debug)]
struct A(());

#[test]
fn test_non_clonable() {
    NonEmpty::new(A(()));
}
