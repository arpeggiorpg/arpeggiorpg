use serde::{Serialize, Serializer};

#[derive(Clone, Eq, PartialEq, Debug, Deserialize)]
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
