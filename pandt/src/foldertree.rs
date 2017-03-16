use rose_tree::{RoseTree, NodeIndex, ROOT};

use serde::ser::{Serialize, Serializer, SerializeMap};
use serde::de;


type LabeledTree<T> = RoseTree<(String, T)>;

#[derive(Debug, Clone)]
pub struct FolderTree<T> {
  pub tree: LabeledTree<T>,
}

impl<T> FolderTree<T> {
  pub fn new(rootNode: T) -> FolderTree<T> {
    FolderTree { tree: RoseTree::new(("".to_string(), rootNode)).0 }
  }
}

impl<T: Eq> Eq for FolderTree<T> {}
impl<T: PartialEq> PartialEq for FolderTree<T> {
  fn eq(&self, other: &FolderTree<T>) -> bool {
    true
  }
}


// this will require escaping "data" (or "node" or whatever we call the node-key)
// usr:
//  data:
//    scenes: scene1, scene2, scene3
//    creatures: c1, c2, c3
// home:
//  radix:


// home:
//   data:
//     scenes: scene1, scene2
//   children:
//     radix:
//       data:
//         scenes: scene1, scene2
//       children: {}

#[derive(Serialize)]
struct SerializerHelper<'a, T: 'a> {
  data: &'a T,
  children: ChildrenSerializer<'a, T>,
}

struct ChildrenSerializer<'a, T: 'a> {
  tree: &'a LabeledTree<T>,
  index: NodeIndex,
}

impl<'a, T: Serialize> Serialize for ChildrenSerializer<'a, T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer
  {
    let children_indices: Vec<NodeIndex> = self.tree.children(self.index).collect();
    let mut map = serializer.serialize_map(Some(children_indices.len()))?;
    for idx in children_indices {
      let (name, node) = self.tree[idx];
      map.serialize_key(&name)?;
      let helper = SerializerHelper {
        data: &node,
        children: ChildrenSerializer {
          tree: &self.tree,
          index: idx,
        },
      };
      map.serialize_value(&helper)?;
    }
    map.end()
  }
}

impl<T: Serialize> Serialize for FolderTree<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer
  {
    let root_idx = NodeIndex::new(ROOT);
    let (_, root) = self.tree[root_idx];
    let helper = SerializerHelper {
      data: &root,
      children: ChildrenSerializer {
        tree: &self.tree,
        index: root_idx,
      },
    };
    helper.serialize(serializer)
  }
}

mod test {
}

// impl<T: de::Deserialize> de::Deserialize for FolderTree<T> {
//   fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: de::Deserializer {}
// }
