# IndexedHashMap

IndexedHashMap is a HashMap wrapper for when the objects you want to store have their own name.

e.g., if you have a Thingy that already has a name:

```rust
struct Thingy {
    name: String,
    score: u8,
}
```


and you want to store it in a HashMap, where the key is the name:

```rust
let thingy = Thingy {name: "radix".to_string(), score: 100};
let mut hm = HashMap::new();
hm.insert(thingy.name.clone(), thingy);
```

if you want to avoid keeping track of the key separate from the name, then you
can use IndexedHashMap:

```rust
let mut ihm = IndexedHashMap::new();
ihm.insert(thingy);
ihm.get("radix")
```

Note that there is no `get_mut` method because that would lead to a broken map
if you changed the name. To get around this, use the `mutate` method.
