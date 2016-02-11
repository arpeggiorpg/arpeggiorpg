# Phone-and-Tablet Roleplaying Game

This is a project to implement a somewhat general game simulation library for turn-based tabletop
games.

# License

MIT-licensed: http://opensource.org/licenses/MIT

# Building/testing/etc

First, get [stack](https://haskellstack.org/).

To run a little console UI for playing a sample game:

> stack build --exec pandt-exe

To run the tests:

> stack build --tests

If you make changes to the code, I recommend using --pedantic to run the builds, except for the
tests (which use a lot of partial functions).

# TODO

Also grep the code for XXX, FIXME, or TODO.

- fix casting (see promptForCasting and promptForFinishingCast in Interaction.hs)
- combat log should show actual damage, not ability damage
- nothing is done with the combat log from recurring effects
- GM actions
  - during action vetting:
    - modify targets
    - modify effects
  - add/remove conditions
  - add creature (from... what?)
  - remove creature (make sure to remove all references!)
  - undo/game history
  - ability to revert ANY effect/ability/condition in history, reversing its effect on the current
    state of the game. Imagine being able to click "x" next to any previous move in
    Hearthstone-style History UI.
- Implement a basic text UI
  - allow skipping turn
  - check if targets exist
  - cancel an ability
- implement persistence
  - should be a simple auto-deriving of FromJSON/ToJSON
  - except, you know, backwards compatibility
- Implement a basic web UI (GHCJS oh boooy)
- Advanced combat mechanics, in rough order of interest:
  - random ability activation
  - damage absorption (temporary hitpoints)
  - Dispels! (how to target an effect? Or maybe it should be chosen for you?)
  - creature construction?
  - stacking vs refreshing conditions
  - area effects with duration
  - randomization/crits (but still allow entering a value, so players can roll their own dice)
  - movement / geography
  - creature visibility
- Implement three example classes:
  - Warrior
  - Mage
  - Cleric
- Example enemies
- Character balance tester using generative testing to see how builds perform
  in combat vs 1/N enemies, and with 1/N allies
- Geography
  - Represent geography in Game
  - Turn phase when players can move
  - Conditions which trigger an effect when the target moves
  - Abilities that move the caster to a location (in a straight line? within a range?)
  - Abilities that move a target to a location (in a straight line? Within a range?)
  - How to implement Swapblast???
  - Abilities that allow movement vs abilities that instantly move
