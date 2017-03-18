use std::collections::{HashMap, HashSet};

use rose_tree::{RoseTree, NodeIndex, ROOT};

use serde::ser::{Serialize, Serializer, SerializeMap};
use serde::de;


error_chain! {
  types { FolderTreeError, FolderTreeErrorKind, FolderTreeResultExt; }

  foreign_links {
  }

  errors {
    FolderNotFound(path: FolderPath) {
      description("A folder wasn't found.")
      display("The folder {} doesn't exist", path.to_string())
    }
    FolderExists(path: FolderPath) {
      description("A folder already existed when trying to insert a new folder node.")
      display("The folder {} already exists", path.to_string())
    }
  }
}

use std::iter::FromIterator;
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FT3<T> {
  nodes: HashMap<FolderPath, T>,
  children: HashMap<FolderPath, HashSet<String>>,
}

impl<T> FT3<T> {
  pub fn new(root: T) -> FT3<T> {
    let path = FolderPath::from_vec(vec![]);
    FT3 {
      nodes: HashMap::from_iter(vec![(path.clone(), root)]),
      children: HashMap::from_iter(vec![(path, HashSet::new())]),
    }
  }

  pub fn make_folder(&mut self, path: &FolderPath, new_child: String, node: T)
                     -> Result<(), FolderTreeError> {
    self.make_child(path, new_child, node)?;
    Ok(())
  }

  pub fn make_folders(&mut self, path: &FolderPath, node: T)
    where T: Clone + ::std::fmt::Debug
  {
    let mut cur_path = FolderPath::from_vec(vec![]);
    for seg in &path.0 {
      let exists = {
        println!("Checking for {:?} on {:?}", cur_path, self);
        let children = self.children
          .get(&cur_path)
          .expect(&format!("BUG: parent didn't have children: {:?}", cur_path));
        children.contains(seg)
      };
      if !exists {
        println!("Making child: {:?} {:?} {:?}", cur_path, seg, node);
        self.make_child(&cur_path, seg.clone(), node.clone())
          .expect("make_child must succeed since we know the child doesn't exist here");
      }
      cur_path = cur_path.child(seg.clone());
    }
  }

  pub fn get(&self, path: &FolderPath) -> Result<&T, FolderTreeError> {
    self.nodes.get(path).ok_or_else(|| FolderTreeErrorKind::FolderNotFound(path.clone()).into())
  }

  pub fn get_mut(&mut self, path: &FolderPath) -> Result<&mut T, FolderTreeError> {
    self.nodes.get_mut(path).ok_or_else(|| FolderTreeErrorKind::FolderNotFound(path.clone()).into())
  }

  /// Make a child.
  /// Panics if the parent_id doesn't exist yet.
  /// Returns an error if the child already exists.
  fn make_child(&mut self, parent: &FolderPath, new_child: String, node: T)
                -> Result<FolderPath, FolderTreeError> {
    let new_full_path = parent.child(new_child.clone());
    {
      let mut parent_children =
        self.children.get_mut(&parent).expect("folder must always have children");
      if parent_children.contains(&new_child) {
        return Err(FolderTreeErrorKind::FolderExists(new_full_path.clone()).into());
      }
      parent_children.insert(new_child);
    }
    self.nodes.insert(new_full_path.clone(), node);
    self.children.insert(new_full_path.clone(), HashSet::new());
    Ok(new_full_path)
  }
}

type LabeledTree<T> = RoseTree<(String, T)>;

#[derive(Debug, Clone)]
pub struct FolderTree<T> {
  pub tree: LabeledTree<T>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FolderPath(Vec<String>);

impl FolderPath {
  // this is test-only for now because it can't represent the empty path (which refers to the root
  // node). Eventually I can figure out a better syntax to support.
  #[cfg(test)]
  pub fn from_str(path: &str) -> FolderPath {
    FolderPath(path.split("/").map(|s| s.to_string()).collect())
  }
  pub fn from_vec(segs: Vec<String>) -> FolderPath {
    FolderPath(segs)
  }

  pub fn to_string(&self) -> String {
    self.0.join("/")
  }

  pub fn child(&self, seg: String) -> FolderPath {
    let mut new = self.clone();
    new.0.push(seg);
    new
  }
}

impl<T> FolderTree<T> {
  pub fn new(root_node: T) -> FolderTree<T> {
    FolderTree { tree: RoseTree::new(("".to_string(), root_node)).0 }
  }

  pub fn make_dir(&mut self, path: &FolderPath, new_path_name: String, node: T)
                  -> Result<(), FolderTreeError> {
    let idx = self.get_path_idx(&path).ok_or(FolderTreeErrorKind::FolderNotFound(path.clone()))?;
    if self.find_child_idx(idx, &new_path_name).is_some() {
      let mut full_path = path.clone();
      full_path.0.push(new_path_name);
      return Err(FolderTreeErrorKind::FolderExists(full_path).into());
    }
    Ok(())
  }

  pub fn make_dirs(&mut self, path: &FolderPath, node: T)
    where T: Clone
  {
    let mut cur_idx = NodeIndex::new(ROOT);
    for seg in &path.0 {
      match self.find_child_idx(cur_idx, &seg) {
        Some(child) => cur_idx = child,
        None => {
          cur_idx = self.tree.add_child(cur_idx, (seg.clone(), node.clone()));
        }
      }
    }
  }

  fn find_child_idx(&self, idx: NodeIndex, name: &str) -> Option<NodeIndex> {
    let children_indices = self.tree.children(idx);
    for cidx in children_indices {
      let child = &self.tree[cidx];
      if child.0 == name {
        return Some(cidx);
      }
    }
    None
  }

  fn get_path_idx(&self, path: &FolderPath) -> Option<NodeIndex> {
    let mut cur_idx = NodeIndex::new(ROOT);

    for seg in &path.0 {
      match self.find_child_idx(cur_idx, &seg) {
        Some(cidx) => cur_idx = cidx,
        None => return None,
      }
    }
    Some(cur_idx)
  }

  pub fn get(&self, path: &FolderPath) -> Option<&T> {
    self.get_path_idx(path).map(|idx| &self.tree[idx].1)
  }

  pub fn get_mut(&mut self, path: &FolderPath) -> Option<&mut T> {
    self.get_path_idx(path).map(move |idx| &mut self.tree[idx].1)
  }
}

impl<T: Eq> Eq for FolderTree<T> {}
impl<T: PartialEq> PartialEq for FolderTree<T> {
  fn eq(&self, other: &FolderTree<T>) -> bool {
    panic!("Implement equality")
  }
}

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
      let children_serializer = ChildrenSerializer {
        tree: &self.tree,
        index: idx,
      };
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

#[derive(Deserialize)]
struct DeserializeHelper<T> {
  data: T,
  children: HashMap<String, Box<DeserializeHelper<T>>>,
}

impl<T> DeserializeHelper<T> {
  fn to_folder_tree(self) -> FolderTree<T> {
    let mut tree: FolderTree<T> = FolderTree::new(self.data);
    // for (k, v) in self.children.into_iter() {
    //   tree.make_dir(FolderPath::from_vec(vec![k]), v.data)
    //     .expect("there shouldn't be any way for there to be multiple children with the same name \
    //              after deserialization!");
    // }
    tree
  }
}


impl<T: de::Deserialize> de::Deserialize for FolderTree<T> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: de::Deserializer
  {
    let helper: DeserializeHelper<T> = de::Deserialize::deserialize(deserializer)?;
    Ok(helper.to_folder_tree())
  }
}

#[cfg(test)]
mod test {
  use foldertree::{FolderTree, FolderPath, FolderTreeError, FolderTreeErrorKind, FT3};
  use serde_json;

  #[test]
  fn get_root_ft3() {
    let ftree = FT3::new("Root node!".to_string());
    assert_eq!(ftree.get(&FolderPath::from_vec(vec![])).unwrap(), &"Root node!".to_string())
  }

  #[test]
  fn get_nonexistent_ft3() {
    let ftree = FT3::new("Root node!".to_string());
    let path = FolderPath::from_str("a");
    match ftree.get(&path) {
      Ok(x) => panic!("Should not have been successful: {:?}", x),
      Err(FolderTreeError(FolderTreeErrorKind::FolderNotFound(errpath), _)) => {
        assert_eq!(errpath, path)
      }
      Err(x) => panic!("Got unexpected error: {:?}", x),
    }
  }

  #[test]
  fn make_dirs_ft3() {
    let mut ftree = FT3::new("Root node!".to_string());
    ftree.make_folders(&FolderPath::from_str("usr/bin"), "Folder!".to_string());
    assert_eq!(ftree.get(&FolderPath::from_str("usr")).unwrap(), &"Folder!".to_string());
    match ftree.get(&FolderPath::from_str("bin")) {
      Ok(x) => panic!("unexpected success: {:?}", x),
      Err(FolderTreeError(FolderTreeErrorKind::FolderNotFound(errpath), _)) => {
        assert_eq!(errpath, FolderPath::from_str("bin"));
      }
      Err(x) => panic!("Unexpected error: {:?}", x),
    }
    assert_eq!(ftree.get(&FolderPath::from_str("usr/bin")).unwrap(), &"Folder!".to_string());
  }

  #[test]
  fn make_dir_existing_ft3() {
    let mut ftree = FT3::new("Root node!".to_string());
    ftree.make_folders(&FolderPath::from_str("foo"), "Folder".to_string());
    let result =
      ftree.make_folder(&FolderPath::from_vec(vec![]), "foo".to_string(), "other folder".to_string());
    match result {
      Ok(x) => panic!("Got some successful result when I shouldn't have: {:?}", x),
      Err(FolderTreeError(FolderTreeErrorKind::FolderExists(path), _)) => {
        assert_eq!(path, FolderPath::from_str("foo"))
      }
      Err(x) => panic!("Got some unexpected error {:?}", x),
    }
  }

  #[test]
  fn get_mut_root_ft3() {
    let mut ftree = FT3::new("Root node!".to_string());
    {
      let mut root_node = ftree.get_mut(&FolderPath::from_vec(vec![])).unwrap();
      assert_eq!(root_node, &mut "Root node!".to_string());
      root_node.push_str(" Okay.");
    }
    let root_node = ftree.get(&FolderPath::from_vec(vec![])).unwrap();
    assert_eq!(root_node, &"Root node! Okay.".to_string());
  }

  // #[test]
  // fn serialize_json_ft3() {
  //   let mut ftree = FT3::new("Root node!".to_string());
  //   ftree.make_folders(&FolderPath::from_str("usr/bin"), "Folder!".to_string());
  //   let json = serde_json::to_value(&ftree).unwrap();

  //   let expected = json!({
  //       "data": "Root node!",
  //       "children": {
  //         "usr": {
  //           "data": "Folder!",
  //           "children": {
  //             "bin": {
  //               "data": "Folder!",
  //               "children": {}}}}}});
  //   assert_eq!(json, expected);
  // }



  #[test]
  fn get_root() {
    let ftree = FolderTree::new("Root node!".to_string());
    assert_eq!(ftree.get(&FolderPath::from_vec(vec![])), Some(&"Root node!".to_string()))
  }

  #[test]
  fn get_nonexistent() {
    let ftree = FolderTree::new("Root node!".to_string());
    assert_eq!(ftree.get(&FolderPath::from_str("a")), None);
  }

  #[test]
  fn make_dirs() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree.make_dirs(&FolderPath::from_str("usr/bin"), "Folder!".to_string());
    assert_eq!(ftree.get(&FolderPath::from_str("usr")), Some(&"Folder!".to_string()));
    assert_eq!(ftree.get(&FolderPath::from_str("bin")), None);
    assert_eq!(ftree.get(&FolderPath::from_str("usr/bin")), Some(&"Folder!".to_string()));
  }

  #[test]
  fn make_dir_existing() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree.make_dirs(&FolderPath::from_str("foo"), "Folder".to_string());
    let result =
      ftree.make_dir(&FolderPath::from_vec(vec![]), "foo".to_string(), "other folder".to_string());
    match result {
      Ok(x) => panic!("Got some successful result when I shouldn't have: {:?}", x),
      Err(FolderTreeError(FolderTreeErrorKind::FolderExists(path), _)) => {
        assert_eq!(path, FolderPath::from_str("foo"))
      }
      Err(x) => panic!("Got some unexpected error {:?}", x),
    }
  }

  #[test]
  fn get_mut_root() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    {
      let mut root_node = ftree.get_mut(&FolderPath::from_vec(vec![])).unwrap();
      assert_eq!(root_node, &mut "Root node!".to_string());
      root_node.push_str(" Okay.");
    }
    let root_node = ftree.get(&FolderPath::from_vec(vec![])).unwrap();
    assert_eq!(root_node, &"Root node! Okay.".to_string());
  }

  #[test]
  fn serialize_json() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree.make_dirs(&FolderPath::from_str("usr/bin"), "Folder!".to_string());
    let json = serde_json::to_value(&ftree).unwrap();

    let expected = json!({
        "data": "Root node!",
        "children": {
          "usr": {
            "data": "Folder!",
            "children": {
              "bin": {
                "data": "Folder!",
                "children": {}}}}}});
    assert_eq!(json, expected);
  }

  #[test]
  fn deserialize_json() {
    let json = json!({
        "data": "Root node!",
        "children": {
          "usr": {
            "data": "Folder!",
            "children": {
              "bin": {
                "data": "Folder!",
                "children": {}}}}}});
    let json = serde_json::to_string(&json).unwrap();
    let ftree: FolderTree<String> = serde_json::from_str(&json).unwrap();

    assert_eq!(ftree.get(&FolderPath::from_str("usr")), Some(&"Folder!".to_string()));
    assert_eq!(ftree.get(&FolderPath::from_str("bin")), None);
    assert_eq!(ftree.get(&FolderPath::from_str("usr/bin")), Some(&"Folder!".to_string()));

  }
}
