use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

use serde::ser::{Error, Serialize, SerializeMap, Serializer};
use serde::de;


#[derive(Debug, Fail)]
pub enum FolderTreeError {
  #[fail(display="The string '{}' is not a valid folder path", _0)]
  InvalidFolderPath(String),
  #[fail(display="The folder '{}' doesn't exist", _0)]
  FolderNotFound(FolderPath),
  #[fail(display="The folder '{}' already exists", _0)]
  FolderExists(FolderPath),
  #[fail(display="The folder '{}' was not empty", _0)]
  FolderNotEmpty(FolderPath),
  #[fail(display="The root folder cannot be renamed.")]
  CannotRenameRoot,
  #[fail(display="The root folder cannot be removed.")]
  CannotRemoveRoot,
  #[fail(display="The root folder cannot be removed.")]
  CannotMoveRoot,
  #[fail(display="The root folder has no name.")]
  RootHasNoName,
  #[fail(display="Can't move '{}' to '{}'", _0, _1)]
  ImpossibleMove( FolderPath, FolderPath)
}


// Using *just* a BTreeMap, and dropping the extra `children` sets, would be simpler and faster in
//  some ways, *except* that listing the direct children of a node would be O(descendents).
// However... consider using a BTreeMap for the `nodes` list, and then tree.walk(path) becomes
// trivial with BTreeMap.range. We could also change it to
// `nodes: BTreeMap<FolderPath, (T, HashSet<String>)>`
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FolderTree<T> {
  nodes: HashMap<FolderPath, (T, HashSet<String>)>,
}

impl<T> Default for FolderTree<T>
where
  T: Default,
{
  fn default() -> FolderTree<T> {
    let root: T = Default::default();
    FolderTree::new(root)
  }
}

impl<T> FolderTree<T> {
  pub fn new(root: T) -> FolderTree<T> {
    let path = FolderPath::from_vec(vec![]);
    FolderTree { nodes: HashMap::from_iter(vec![(path.clone(), (root, HashSet::new()))]) }
  }

  /// Make a child folder.
  /// Returns an error if the child already exists.
  pub fn make_folder(
    &mut self, parent: &FolderPath, new_child: String, node: T
  ) -> Result<FolderPath, FolderTreeError> {
    let new_full_path = parent.child(new_child.clone());
    {
      let pdata = self.get_data_mut(parent)?;
      if pdata.1.contains(&new_child) {
        return Err(FolderTreeError::FolderExists(new_full_path.clone()).into());
      }
      pdata.1.insert(new_child);
    }
    self
      .nodes
      .insert(new_full_path.clone(), (node, HashSet::new()));
    Ok(new_full_path)
  }

  pub fn make_folders(&mut self, path: &FolderPath, node: T)
  where
    T: Clone,
  {
    let mut cur_path = FolderPath::from_vec(vec![]);
    for seg in &path.0 {
      let child_path = cur_path.child(seg.clone());
      let exists = self.nodes.contains_key(&child_path);
      if !exists {
        self
          .make_folder(&cur_path, seg.clone(), node.clone())
          .expect("make_child must succeed since we know the child doesn't exist here");
      }
      cur_path = child_path;
    }
  }

  pub fn get(&self, path: &FolderPath) -> Result<&T, FolderTreeError> {
    Ok(&self.get_data(path)?.0)
  }

  pub fn get_mut(&mut self, path: &FolderPath) -> Result<&mut T, FolderTreeError> {
    Ok(&mut self.get_data_mut(path)?.0)
  }

  pub fn get_children(&self, path: &FolderPath) -> Result<&HashSet<String>, FolderTreeError> {
    Ok(&self.get_data(path)?.1)
  }

  /// Remove a folder node. The folder must not have any children. The node data for the folder
  /// will be returned.
  pub fn remove(&mut self, path: &FolderPath) -> Result<T, FolderTreeError> {
    if self.get_children(path)?.is_empty() {
      bail!(FolderTreeError::FolderNotEmpty(path.clone()));
    }
    match path.up() {
      Some((parent, child)) => {
        self
          .nodes
          .get_mut(&parent)
          .expect("Parent must exist")
          .1
          .remove(&child);
        Ok(
          self
            .nodes
            .remove(path)
            .expect("Folder must exist if it had children.")
            .0,
        )
      }
      None => bail!(FolderTreeError::CannotRemoveRoot),
    }
  }

  pub fn rename_folder(
    &mut self, path: &FolderPath, new_name: String
  ) -> Result<(), FolderTreeError> {
    match path.up() {
      Some((parent, basename)) => {
        let new_path = parent.child(new_name.clone());
        if self.nodes.contains_key(&new_path) {
          bail!(FolderTreeError::FolderExists(new_path));
        }
        {
          let mut_data = self.nodes.get_mut(&parent).expect("Parent must exist.");
          mut_data.1.remove(&basename);
          mut_data.1.insert(new_name.clone());
        }
        let data = self.nodes.remove(path).expect("Node must exist.");
        self.nodes.insert(new_path, data);
        Ok(())
      }
      None => bail!(FolderTreeError::CannotRenameRoot),
    }
  }

  pub fn move_folder(
    &mut self, path: &FolderPath, new_parent: &FolderPath
  ) -> Result<(), FolderTreeError> {
    if new_parent.is_child_of(path) {
      bail!(FolderTreeError::ImpossibleMove(path.clone(), new_parent.clone()));
    }
    if !self.nodes.contains_key(new_parent) {
      bail!(FolderTreeError::FolderNotFound(new_parent.clone()));
    }
    if !self.nodes.contains_key(path) {
      bail!(FolderTreeError::FolderNotFound(path.clone()));
    }
    match path.up() {
      Some((old_parent, basename)) => {
        if self.nodes.contains_key(&new_parent.child(basename.clone())) {
          bail!(FolderTreeError::FolderExists(new_parent.child(basename)));
        }
        let descendants = self.walk_paths(path).cloned().collect::<Vec<FolderPath>>();
        for subpath in descendants {
          let relative = subpath.relative_to(&old_parent)?;
          let new_path = new_parent.descendant(relative.0);
          let path_data = self
            .nodes
            .remove(&subpath)
            .ok_or_else(|| FolderTreeError::FolderNotFound(subpath.clone()))?;
          self.nodes.insert(new_path, path_data);
        }

        self
          .nodes
          .get_mut(&old_parent)
          .expect("Parent node must exist")
          .1
          .remove(&basename);
        self
          .nodes
          .get_mut(new_parent)
          .expect("Target directory must exist")
          .1
          .insert(basename);
        Ok(())
      }
      None => bail!(FolderTreeError::CannotMoveRoot),
    }
  }

  fn get_data(&self, path: &FolderPath) -> Result<&(T, HashSet<String>), FolderTreeError> {
    self
      .nodes
      .get(path)
      .ok_or_else(|| FolderTreeError::FolderNotFound(path.clone()).into())
  }

  fn get_data_mut(
    &mut self, path: &FolderPath
  ) -> Result<&mut (T, HashSet<String>), FolderTreeError> {
    self
      .nodes
      .get_mut(path)
      .ok_or_else(|| FolderTreeError::FolderNotFound(path.clone()).into())
  }

  /// Iterate paths to all folders below the given one.
  pub fn walk_paths<'a>(&'a self, parent: &FolderPath) -> impl Iterator<Item = &FolderPath> + 'a {
    let parent: FolderPath = parent.clone();

    let all_nodes = self.nodes.keys().filter(move |p| p.is_child_of(&parent));
    let mut all_nodes = all_nodes.collect::<Vec<_>>();
    all_nodes.sort();
    all_nodes.into_iter()
  }

  /// Extract a subtree from a FolderPath
  // Our representation requires this to be O(n) which kinda sucks, but it probably doesn't matter
  pub fn subtree(&self, path: &FolderPath) -> Result<FolderTree<T>, FolderTreeError>
  where
    T: Clone,
  {
    let folder = self.get(path)?;
    let mut new_tree = FolderTree::new(folder.clone());
    for sub_path in self.walk_paths(path).cloned().collect::<Vec<_>>() {
      let new_path = sub_path.relative_to(path)?;
      if new_path == FolderPath::from_vec(vec![]) {
        continue;
      }
      println!("Subtree: {:?}; Relative path: {:?}", sub_path, new_path);
      let cloned_folder = self.get(&sub_path)?.clone();
      let (new_parent, new_leaf) = new_path
        .up()
        .ok_or_else(|| FolderTreeError::RootHasNoName)?;
      new_tree.make_folder(&new_parent, new_leaf, cloned_folder)?;
    }
    Ok(new_tree)
  }
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FolderPath(Vec<String>);

impl FolderPath {
  pub fn to_string(&self) -> String {
    let mut s = String::new();
    for seg in &self.0 {
      s.push_str("/");
      s.push_str(seg);
    }
    s
  }

  pub fn up(&self) -> Option<(FolderPath, String)> {
    self
      .0
      .split_last()
      .map(|(last, trunk)| (FolderPath::from_vec(trunk.to_vec()), last.clone()))
  }

  pub fn from_vec(segs: Vec<String>) -> FolderPath {
    FolderPath(segs)
  }

  pub fn is_root(&self) -> bool {
    self.0.is_empty()
  }

  pub fn child(&self, seg: String) -> FolderPath {
    let mut new = self.clone();
    new.0.push(seg);
    new
  }

  pub fn descendant(&self, subpath: Vec<String>) -> FolderPath {
    let mut new = self.clone();
    new.0.extend(subpath);
    new
  }

  pub fn is_child_of(&self, other: &FolderPath) -> bool {
    self.0.starts_with(&other.0)
  }

  pub fn relative_to(&self, ancestor: &FolderPath) -> Result<FolderPath, FolderTreeError> {
    if self.is_child_of(ancestor) {
      Ok(FolderPath::from_vec(self.0[ancestor.0.len()..].to_vec()))
    } else {
      bail!("Sorry, relative_to must be passed ancestor.")
    }
  }
}

impl ::std::str::FromStr for FolderPath {
  type Err = FolderTreeError;
  fn from_str(path: &str) -> Result<FolderPath, FolderTreeError> {
    let segments: Vec<&str> = path.split('/').collect();
    if segments.is_empty() {
      Ok(FolderPath(vec![]))
    } else if segments[0] != "" {
      Err(FolderTreeError::InvalidFolderPath(path.to_string()).into())
    } else {
      Ok(FolderPath(segments.iter().skip(1).map(|s| s.to_string()).collect()))
    }
  }
}

impl ::std::fmt::Display for FolderPath {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    write!(f, "[{}]", self.to_string())
  }
}

impl Serialize for FolderPath {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    self.to_string().serialize(serializer)
  }
}

impl<'de> de::Deserialize<'de> for FolderPath {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: de::Deserializer<'de>,
  {
    let st: String = de::Deserialize::deserialize(deserializer)?;
    match st.parse() {
      Ok(x) => Ok(x),
      Err(FolderTreeError::InvalidFolderPath(p)) => {
        Err(de::Error::invalid_value(de::Unexpected::Str(&p), &"must begin with /"))
      }
      Err(x) => Err(de::Error::invalid_value(
        de::Unexpected::Str(&st),
        &format!("Unknown error: {:?}", x).as_ref(),
      )),
    }
  }
}


#[derive(Serialize)]
struct SerializerHelper<'a, T: 'a> {
  data: &'a T,
  children: ChildrenSerializer<'a, T>,
}

struct ChildrenSerializer<'a, T: 'a> {
  tree: &'a FolderTree<T>,
  path: &'a FolderPath,
}

impl<'a, T: Serialize> Serialize for ChildrenSerializer<'a, T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let children = self.tree.get_children(self.path).map_err(
      |e| S::Error::custom(&format!("BUG: couldn't find child while serializing: {:?}", e)),
    )?;
    let mut map = serializer.serialize_map(Some(children.len()))?;
    for child in children {
      let full_path = self.path.child(child.to_string());
      let children_serializer = ChildrenSerializer { tree: self.tree, path: &full_path };
      let helper = SerializerHelper {
        data: self
          .tree
          .get(&full_path)
          .expect("Child node should definitely exist here, since children() returned it"),
        children: children_serializer,
      };
      map.serialize_key(child)?;
      map.serialize_value(&helper)?;
    }
    map.end()
  }
}

impl<T: Serialize> Serialize for FolderTree<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let root_path = FolderPath::from_vec(vec![]);
    let root = self.get(&root_path).expect("Root node must always exist.");
    let helper = SerializerHelper {
      data: root,
      children: ChildrenSerializer { tree: self, path: &root_path },
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
  fn into_folder_tree(self) -> FolderTree<T> {
    let mut paths: Vec<(FolderPath, T)> = vec![];
    self.serialize_tree(FolderPath::from_vec(vec![]), &mut paths);
    let mut iter = paths.into_iter();
    let first = iter
      .next()
      .expect("There must always be at least one element if this data structure exists... right?");
    debug_assert_eq!(first.0, FolderPath::from_vec(vec![]));
    let mut tree = FolderTree::new(first.1);
    for (path, node) in iter {
      let (parent, child_name) = path
        .up()
        .expect("There should always be a parent for these nodes");
      tree
        .make_folder(&parent, child_name, node)
        .expect("Making this folder should be okay... right?");
    }
    tree
  }

  fn serialize_tree(self, path: FolderPath, paths: &mut Vec<(FolderPath, T)>) {
    // TODO: this could be more efficient by making this an iterator instead of something that
    // appends to a vec.
    paths.push((path.clone(), self.data));
    for (k, v) in self.children {
      v.serialize_tree(path.child(k), paths);
    }
  }
}


impl<'de, T: de::Deserialize<'de>> de::Deserialize<'de> for FolderTree<T> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: de::Deserializer<'de>,
  {
    let helper: DeserializeHelper<T> = de::Deserialize::deserialize(deserializer)?;
    Ok(helper.into_folder_tree())
  }
}

#[cfg(test)]
mod test {
  use std::collections::HashSet;
  use std::iter::FromIterator;
  use foldertree::{FolderPath, FolderTree, FolderTreeError, FolderTreeErrorKind};
  use serde_json;

  fn fpath(s: &str) -> FolderPath {
    s.parse().expect("Couldn't parse string as FolderPath")
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
      Err(FolderTreeError::FolderNotFound(errpath)) => {
        assert_eq!(errpath, path)
      }
      Err(x) => panic!("Got unexpected error: {:?}", x),
    }
  }

  #[test]
  fn make_folders() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree.make_folders(&fpath("/usr/bin"), "Folder!".to_string());
    assert_eq!(ftree.get(&fpath("/usr")).unwrap(), &"Folder!".to_string());
    match ftree.get(&fpath("/bin")) {
      Ok(x) => panic!("unexpected success: {:?}", x),
      Err(FolderTreeError::FolderNotFound(errpath)) => {
        assert_eq!(errpath, fpath("/bin"));
      }
      Err(x) => panic!("Unexpected error: {:?}", x),
    }
    assert_eq!(ftree.get(&fpath("/usr/bin")).unwrap(), &"Folder!".to_string());
  }

  #[test]
  fn make_folder_existing() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree.make_folders(&fpath("/foo"), "Folder".to_string());
    let result = ftree.make_folder(&fpath(""), "foo".to_string(), "other folder".to_string());
    match result {
      Ok(x) => panic!("Got some successful result when I shouldn't have: {:?}", x),
      Err(FolderTreeError::FolderExists(path)) => {
        assert_eq!(path, fpath("/foo"))
      }
      Err(x) => panic!("Got some unexpected error {:?}", x),
    }
  }

  #[test]
  fn get_mut_root() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    {
      let root_node = ftree.get_mut(&fpath("")).unwrap();
      assert_eq!(root_node, &mut "Root node!".to_string());
      root_node.push_str(" Okay.");
    }
    let root_node = ftree.get(&fpath("")).unwrap();
    assert_eq!(root_node, &"Root node! Okay.".to_string());
  }

  #[test]
  fn move_folder() {
    let mut ftree = FolderTree::new("Root node".to_string());
    ftree
      .make_folder(&fpath(""), "usr".to_string(), "usr folder".to_string())
      .unwrap();
    ftree
      .make_folder(&fpath(""), "home".to_string(), "home folder".to_string())
      .unwrap();
    ftree.move_folder(&fpath("/usr"), &fpath("/home")).unwrap();
    match ftree.get(&fpath("/usr")) {
      Err(FolderTreeError::FolderNotFound(p)) => {
        assert_eq!(p, fpath("/usr"))
      }
      x => panic!("Bad result: {:?}", x),
    }
    assert_eq!(ftree.get(&fpath("/home/usr")).unwrap(), &"usr folder".to_string());
    assert_eq!(
      ftree.get_children(&fpath("")).unwrap(),
      &HashSet::from_iter(vec!["home".to_string()])
    );
    assert_eq!(
      ftree.get_children(&fpath("/home")).unwrap(),
      &HashSet::from_iter(vec!["usr".to_string()])
    );
  }

  #[test]
  fn move_folder_no_target() {
    let mut ftree = FolderTree::new("Root node".to_string());
    ftree
      .make_folder(&fpath(""), "usr".to_string(), "usr folder".to_string())
      .unwrap();
    match ftree.move_folder(&fpath("/usr"), &fpath("/home")) {
      Err(FolderTreeError::FolderNotFound(p)) => {
        assert_eq!(p, fpath("/home"))
      }
      x => panic!("Bad result: {:?}", x),
    }
  }

  #[test]
  fn move_folder_no_src() {
    let mut ftree = FolderTree::new("Root node".to_string());
    ftree
      .make_folder(&fpath(""), "usr".to_string(), "usr folder".to_string())
      .unwrap();
    match ftree.move_folder(&fpath("/foobar"), &fpath("/usr")) {
      Err(FolderTreeError::FolderNotFound(p)) => {
        assert_eq!(p, fpath("/foobar"))
      }
      x => panic!("Bad result: {:?}", x),
    }
  }

  #[test]
  fn move_folder_duplicate() {
    let mut ftree = FolderTree::new("Root node".to_string());
    ftree
      .make_folder(&fpath(""), "usr".to_string(), "usr folder".to_string())
      .unwrap();
    ftree
      .make_folder(&fpath(""), "home".to_string(), "home folder".to_string())
      .unwrap();
    ftree
      .make_folder(&fpath("/home"), "usr".to_string(), "home/usr folder".to_string())
      .unwrap();

    match ftree.move_folder(&fpath("/usr"), &fpath("/home")) {
      Err(FolderTreeError::FolderExists(p)) => {
        assert_eq!(p, fpath("/home/usr"))
      }
      x => panic!("Bad result: {:?}", x),
    }
  }


  #[test]
  fn move_folder_with_children() {
    let mut ftree = FolderTree::new("Root node".to_string());
    ftree
      .make_folder(&fpath(""), "usr".to_string(), "usr folder".to_string())
      .unwrap();
    ftree
      .make_folder(&fpath("/usr"), "bin".to_string(), "/usr/bin folder".to_string())
      .unwrap();
    ftree
      .make_folder(&fpath("/usr"), "share".to_string(), "/usr/share folder".to_string())
      .unwrap();
    ftree
      .make_folder(&fpath(""), "home".to_string(), "home folder".to_string())
      .unwrap();
    ftree.move_folder(&fpath("/usr"), &fpath("/home")).unwrap();
    for path in vec![fpath("/usr"), fpath("/usr/bin"), fpath("/usr/share")].iter() {
      match ftree.get(path) {
        Err(FolderTreeError::FolderNotFound(p)) => assert_eq!(p, *path),
        x => panic!("Bad result: {:?}", x),
      }
    }
    assert_eq!(ftree.get(&fpath("/home")).unwrap(), &"home folder".to_string());
    assert_eq!(ftree.get(&fpath("/home/usr")).unwrap(), &"usr folder".to_string());
    assert_eq!(ftree.get(&fpath("/home/usr/bin")).unwrap(), &"/usr/bin folder".to_string());
    assert_eq!(ftree.get(&fpath("/home/usr/share")).unwrap(), &"/usr/share folder".to_string());
    assert_eq!(
      ftree.get_children(&fpath("/home")).unwrap(),
      &HashSet::from_iter(vec!["usr".to_string()])
    );
  }

  #[test]
  fn move_folder_to_descendant() {
    let mut ftree = FolderTree::new("Root node".to_string());
    ftree
      .make_folder(&fpath(""), "usr".to_string(), "usr folder".to_string())
      .unwrap();
    ftree
      .make_folder(&fpath("/usr"), "bin".to_string(), "/usr/bin folder".to_string())
      .unwrap();
    match ftree.move_folder(&fpath("/usr"), &fpath("/usr/bin")) {
      Err(FolderTreeError::ImpossibleMove(from, to)) => {
        assert_eq!(from, fpath("/usr"));
        assert_eq!(to, fpath("/usr/bin"));
      }
      x => panic!("Bad result: {:?}", x),
    }
  }

  #[test]
  fn rename_folder_root() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    match ftree.rename_folder(&fpath(""), "foo".to_string()) {
      Err(FolderTreeError::CannotRenameRoot) => {}
      x => panic!("Bad result: {:?}", x),
    }
  }

  #[test]
  fn rename_folder_dup() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree
      .make_folder(&fpath(""), "foo".to_string(), "foo folder".to_string())
      .unwrap();
    ftree
      .make_folder(&fpath(""), "bar".to_string(), "bar folder".to_string())
      .unwrap();
    match ftree.rename_folder(&fpath("/foo"), "bar".to_string()) {
      Err(FolderTreeError::FolderExists(p)) => assert_eq!(p, fpath("/bar")),
      x => panic!("Bad result: {:?}", x),
    }
  }

  #[test]
  fn rename_folder() {
    let mut ftree = FolderTree::new("Root node!".to_string());
    ftree
      .make_folder(&fpath(""), "foo".to_string(), "foo folder".to_string())
      .unwrap();
    ftree
      .rename_folder(&fpath("/foo"), "bar".to_string())
      .unwrap();
    assert_eq!(
      ftree.get_children(&fpath("")).unwrap(),
      &HashSet::from_iter(vec!["bar".to_string()])
    );
    match ftree.get(&fpath("/foo")) {
      Err(FolderTreeError::FolderNotFound(p)) => {
        assert_eq!(p, fpath("/foo"))
      }
      x => panic!("Bad result: {:?}", x),
    }
    assert_eq!(ftree.get(&fpath("/bar")).unwrap(), &"foo folder".to_string());
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

    assert_eq!(ftree.get(&fpath("/usr")).unwrap(), &"Folder!".to_string());
    match ftree.get(&fpath("/bin")) {
      Err(FolderTreeError::FolderNotFound(p)) => {
        assert_eq!(p, fpath("/bin"))
      }
      x => panic!("Unexpected result: {:?}", x),
    }
    assert_eq!(ftree.get(&fpath("/usr/bin")).unwrap(), &"Folder!".to_string());
  }


  #[test]
  fn folderpath_from_str() {
    assert_eq!(fpath(""), FolderPath::from_vec(vec![]));
    assert_eq!(fpath("/foo"), FolderPath::from_vec(vec!["foo".to_string()]));
    match "foo".parse::<FolderPath>() {
      Err(FolderTreeError::InvalidFolderPath(p)) => {
        assert_eq!(p, "foo".to_string())
      }
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  #[test]
  fn folderpath_to_str() {
    assert_eq!(fpath("").to_string(), "");
    assert_eq!(fpath("/foo/bar").to_string(), "/foo/bar");
  }
}
