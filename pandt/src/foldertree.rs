use std::collections::{HashMap, HashSet};

use serde::ser::{Serialize, Serializer, SerializeMap};
use serde::de;


error_chain! {
  types { FolderTreeError, FolderTreeErrorKind, FolderTreeResultExt; }

  foreign_links {
  }

  errors {
    InvalidFolderPath(path: String) {
      description("Couldn't parse a folder path")
      display("The string '{}' is not a valid folder path", path)
    }
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
#[derive(Debug, Eq, PartialEq, Clone, Serialize)]
pub struct FolderTree<T> {
  nodes: HashMap<FolderPath, T>,
  children: HashMap<FolderPath, HashSet<String>>,
}

impl<T> FolderTree<T> {
  pub fn new(root: T) -> FolderTree<T> {
    let path = FolderPath::from_vec(vec![]);
    FolderTree {
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
        let children = self.children
          .get(&cur_path)
          .expect(&format!("BUG: parent didn't have children: {:?}", cur_path));
        children.contains(seg)
      };
      if !exists {
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


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FolderPath(Vec<String>);

impl FolderPath {
  pub fn from_str(path: &str) -> Result<FolderPath, FolderTreeError> {
    let segments: Vec<&str> = path.split("/").collect();
    if segments.len() == 0 {
      Ok(FolderPath(vec![]))
    } else {
      if segments[0] != "" {
        Err(FolderTreeErrorKind::InvalidFolderPath(path.to_string()).into())
      } else {
        Ok(FolderPath(segments.iter().skip(1).map(|s| s.to_string()).collect()))
      }
    }
  }

  pub fn to_string(&self) -> String {
    let mut s = String::new();
    for seg in self.0.iter() {
      s.push_str("/");
      s.push_str(seg);
    }
    s
  }

  pub fn from_vec(segs: Vec<String>) -> FolderPath {
    FolderPath(segs)
  }

  pub fn child(&self, seg: String) -> FolderPath {
    let mut new = self.clone();
    new.0.push(seg);
    new
  }
}

impl Serialize for FolderPath {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer
  {
    self.to_string().serialize(serializer)
  }
}

impl de::Deserialize for FolderPath {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: de::Deserializer
  {
    let st: String = de::Deserialize::deserialize(deserializer)?;
    match FolderPath::from_str(&st) {
      Ok(x) => Ok(x),
      Err(FolderTreeError(FolderTreeErrorKind::InvalidFolderPath(p), _)) => {
        Err(de::Error::invalid_value(de::Unexpected::Str(&p), &"must begin with /"))
      }
      Err(x) => {
        Err(de::Error::invalid_value(de::Unexpected::Str(&st),
                                     &format!("Unknown error: {:?}", x).as_ref()))
      }
    }
  }
}


// #[derive(Serialize)]
// struct SerializerHelper<'a, T: 'a> {
//   data: &'a T,
//   children: ChildrenSerializer<'a, T>,
// }

// struct ChildrenSerializer<'a, T: 'a> {
//   tree: &'a LabeledTree<T>,
//   index: NodeIndex,
// }

// impl<'a, T: Serialize> Serialize for ChildrenSerializer<'a, T> {
//   fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where S: Serializer
//   {
//     let children_indices: Vec<NodeIndex> = self.tree.children(self.index).collect();
//     let mut map = serializer.serialize_map(Some(children_indices.len()))?;
//     for idx in children_indices {
//       let &(ref name, ref node) = &self.tree[idx];
//       let children_serializer = ChildrenSerializer {
//         tree: &self.tree,
//         index: idx,
//       };
//       let helper = SerializerHelper {
//         data: node,
//         children: children_serializer,
//       };
//       map.serialize_key(&name)?;
//       map.serialize_value(&helper)?;
//     }
//     map.end()
//   }
// }

// impl<T: Serialize> Serialize for FolderTree<T> {
//   fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where S: Serializer
//   {
//     let root_idx = NodeIndex::new(ROOT);
//     let &(_, ref root) = &self.tree[root_idx];
//     let helper = SerializerHelper {
//       data: root,
//       children: ChildrenSerializer {
//         tree: &self.tree,
//         index: root_idx,
//       },
//     };
//     helper.serialize(serializer)
//   }
// }

// #[derive(Deserialize)]
// struct DeserializeHelper<T> {
//   data: T,
//   children: HashMap<String, Box<DeserializeHelper<T>>>,
// }

// impl<T> DeserializeHelper<T> {
//   fn to_folder_tree(self) -> FolderTree<T> {
//     let mut tree: FolderTree<T> = FolderTree::new(self.data);
//     // for (k, v) in self.children.into_iter() {
//     //   tree.make_dir(FolderPath::from_vec(vec![k]), v.data)
//     //     .expect("there shouldn't be any way for there to be multiple children with the same name \
//     //              after deserialization!");
//     // }
//     tree
//   }
// }


// impl<T: de::Deserialize> de::Deserialize for FolderTree<T> {
//   fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where D: de::Deserializer
//   {
//     let helper: DeserializeHelper<T> = de::Deserialize::deserialize(deserializer)?;
//     Ok(helper.to_folder_tree())
//   }
// }

#[cfg(test)]
mod test {
  use foldertree::{FolderTree, FolderPath, FolderTreeError, FolderTreeErrorKind};
  use serde_json;

  fn fpath(s: &str) -> FolderPath {
    FolderPath::from_str(s).unwrap()
  }

  #[test]
  fn get_root() {
    let ftree = FolderTree::new("Root node!".to_string());
    assert_eq!(ftree.get(&fpath("")).unwrap(), &"Root node!".to_string())
  }

  #[test]
  fn get_nonexistent() {
    let ftree = FolderTree::new("Root node!".to_string());
    let path = fpath("/a");
    match ftree.get(&path) {
      Ok(x) => panic!("Should not have been successful: {:?}", x),
      Err(FolderTreeError(FolderTreeErrorKind::FolderNotFound(errpath), _)) => {
        assert_eq!(errpath, path)
      }
      Err(x) => panic!("Got unexpected error: {:?}", x),
    }
  }

  #[test]
  fn make_dirs() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree.make_folders(&fpath("/usr/bin"), "Folder!".to_string());
    assert_eq!(ftree.get(&fpath("/usr")).unwrap(), &"Folder!".to_string());
    match ftree.get(&fpath("/bin")) {
      Ok(x) => panic!("unexpected success: {:?}", x),
      Err(FolderTreeError(FolderTreeErrorKind::FolderNotFound(errpath), _)) => {
        assert_eq!(errpath, fpath("/bin"));
      }
      Err(x) => panic!("Unexpected error: {:?}", x),
    }
    assert_eq!(ftree.get(&fpath("/usr/bin")).unwrap(), &"Folder!".to_string());
  }

  #[test]
  fn make_dir_existing() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree.make_folders(&fpath("/foo"), "Folder".to_string());
    let result = ftree.make_folder(&fpath(""), "foo".to_string(), "other folder".to_string());
    match result {
      Ok(x) => panic!("Got some successful result when I shouldn't have: {:?}", x),
      Err(FolderTreeError(FolderTreeErrorKind::FolderExists(path), _)) => {
        assert_eq!(path, fpath("/foo"))
      }
      Err(x) => panic!("Got some unexpected error {:?}", x),
    }
  }

  #[test]
  fn get_mut_root() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    {
      let mut root_node = ftree.get_mut(&fpath("")).unwrap();
      assert_eq!(root_node, &mut "Root node!".to_string());
      root_node.push_str(" Okay.");
    }
    let root_node = ftree.get(&fpath("")).unwrap();
    assert_eq!(root_node, &"Root node! Okay.".to_string());
  }

  #[test]
  fn serialize_json() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree.make_folders(&fpath("/usr/bin"), "Folder!".to_string());
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

  // #[test]
  // fn deserialize_json() {
  //   let json = json!({
  //       "data": "Root node!",
  //       "children": {
  //         "usr": {
  //           "data": "Folder!",
  //           "children": {
  //             "bin": {
  //               "data": "Folder!",
  //               "children": {}}}}}});
  //   let json = serde_json::to_string(&json).unwrap();
  //   let ftree: FolderTree<String> = serde_json::from_str(&json).unwrap();

  //   assert_eq!(ftree.get(&fpath("/usr")), Some(&"Folder!".to_string()));
  //   assert_eq!(ftree.get(&fpath("/bin")), None);
  //   assert_eq!(ftree.get(&fpath("/usr/bin")), Some(&"Folder!".to_string()));
  // }


  #[test]
  fn folderpath_from_str() {
    assert_eq!(FolderPath::from_str("").unwrap(), FolderPath::from_vec(vec![]));
    assert_eq!(FolderPath::from_str("/foo").unwrap(), FolderPath::from_vec(vec!["foo".to_string()]));
    match FolderPath::from_str("foo") {
      Err(FolderTreeError(FolderTreeErrorKind::InvalidFolderPath(p), _)) => {
        assert_eq!(p, "foo".to_string())
      }
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  #[test]
  fn folderpath_to_str() {
    assert_eq!(FolderPath::from_str("").unwrap().to_string(), "");
    assert_eq!(FolderPath::from_str("/foo/bar").unwrap().to_string(), "/foo/bar");
  }
}
