use rose_tree::{RoseTree, NodeIndex, ROOT};

use serde::ser::{Serialize, Serializer, SerializeMap};
use serde::de;


type LabeledTree<T> = RoseTree<(String, T)>;

#[derive(Debug, Clone)]
pub struct FolderTree<T> {
  pub tree: LabeledTree<T>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FolderPath(Vec<String>);

impl FolderPath {
  pub fn from_vec(segs: Vec<String>) -> FolderPath {
    FolderPath(segs)
  }
}

impl<T> FolderTree<T> {
  pub fn new(root_node: T) -> FolderTree<T> {
    FolderTree { tree: RoseTree::new(("".to_string(), root_node)).0 }
  }

  pub fn get(&self, path: FolderPath) -> Option<&T> {
    if path.0.len() == 0 {
      return None;
    }
    let mut cur_idx = NodeIndex::new(ROOT);

    fn find_child<A>(tree: &LabeledTree<A>, idx: NodeIndex, name: String) -> Option<NodeIndex> {
      let children_indices = tree.children(idx);
      for cidx in children_indices {
        let child = &tree[cidx];
        if child.0 == name {
          return Some(cidx);
        }
      }
      None
    }
    for seg in path.0 {
      match find_child(&self.tree, cur_idx, seg) {
        Some(cidx) => cur_idx = cidx,
        None => return None
      }
    }
    Some(&self.tree[cur_idx].1)
  }
}

impl<T: Eq> Eq for FolderTree<T> {}
impl<T: PartialEq> PartialEq for FolderTree<T> {
  fn eq(&self, other: &FolderTree<T>) -> bool {
    panic!("Implement equality")
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
      let &(ref name, ref node) = &self.tree[idx];
      let children_serializer = ChildrenSerializer { tree: &self.tree, index: idx};
      let helper = SerializerHelper {
        data: node,
        children: children_serializer,
      };
      map.serialize_key(&name)?;
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
    let &(_, ref root) = &self.tree[root_idx];
    let helper = SerializerHelper {
      data: root,
      children: ChildrenSerializer {
        tree: &self.tree,
        index: root_idx,
      },
    };
    helper.serialize(serializer)
  }
}

mod test {}

// impl<T: de::Deserialize> de::Deserialize for FolderTree<T> {
//   fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: de::Deserializer {}
// }
