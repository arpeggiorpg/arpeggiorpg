# "Capability Types"?

I have learned a few things. Briefly:

- Don't put the same thing in different enum variants.
- Don't put phantom types on types with lots of data that will be relevant no
  matter the type parameter.
- Instead, put specific methods on types that are wrapped by variants in a
  "Capability" enum.

Longly, here's the BS that I had:

```rust
pub struct Game<A> { history: ..., combatants: ..., etc: etc, _p: PhantomData}
```

Most operations on a Game worked for any `A`, but a few required specific `A`s.
For example, you could only call the "act" function for Game<Able> (The whole
point of this phantom type parameter was to represent the state of the current
creature). "skip" was available on `Game<Able>` and `Game<Incap>`. These `Able`
and `Incap` types were meaningless phantom types -- they didn't have any values,
they were only used as type markers.

```rust
enum Able {}
enum Incap {}
```

Of course, I had to have functions that could return any kind of `Game<A>` --
or, more accurately, any of a set of specific `Game<A>`s. Like, the `skip`
function would have to move to the player whose turn was next, and that player
might be incapacitated or dead or able to act. So I had to represent this with
an enum:

```rust
pub enum GameVari {
  Able(Game<Able>),
  Incap(Game<Incap>),
}
```

Then, if I wanted to deal with the current state of the game, I had to pattern
match on `GameVari` so I knew what I could perform on it -- whether I could act,
or whatever.

The problem was this was that even if I wanted to deal with data that was
available for any type of Game, I had to pattern match. This got to be extremely
annoying.

## Capability Types

I eventually figured out a much better design, which I called "capability
types". It no longer uses any phantom types, which feels a lot better.

Now, I just have a `Game` type. No more type parameter, and no more `GameVari`
sum type. All of the common data is still available on Game, with easy-to-use
public methods and no extraneous pattern matching. However, I still want a
statically guaranteed, type-safe way to ensure `act` can only be called on Games
when the current player is able to act. How do I do this? With a method called
`capability`:

  ```rust
  pub fn capability(&self) -> GameCapability;`
```

`GameCapability` is an enum a lot like my old `GameVari`, except instead of each
variant wrapping a `Game<...>`, it wraps a specific type like `GameAble` or
`GameIncap`. These types have the `act` and `skip` methods implemented for them,
and in-memory they just hold references to the core `Game` object. The nice
thing about this is that when I have a Game, I can still do all the things I
should be able to do to any type of game, and I only need to pattern match when
I need to do something specific to a particular game state.

### Mutable capabilities

Most of the time I like to write purely functional code, hence the signature for
`capability` that I used above. However, there are times when you may want to
use this pattern with mutable methods. In that case, I've found that this
pattern works best:

```rust
pub fn capability(self) -> GameCapability;
```

Note that this method doesn't take a reference to `self` (the Game value), but
rather it *consumes* self. Then, each variant of GameCapability would wrap a
type that has methods like:

```rust
pub fn act(&mut self) -> Result<(), GameError>
```

and also a method called `done`, which gives back ownership of the Game object:

```rust
pub fn done(self) -> Game;
```

I've found this to be the easiest way to allow mutable capability types without
finding myself in borrow-hell.

# Phone-and-Tablet Roleplaying Game

This is a project to implement a somewhat general game simulation library for turn-based tabletop
games.

# License

MIT-licensed: http://opensource.org/licenses/MIT

# Building/testing/etc

cargo test

# TODO

Also grep the code for XXX, FIXME, or TODO.
