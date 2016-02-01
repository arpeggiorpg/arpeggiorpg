# Phone-and-Tablet Roleplaying Game

This is a project to implement a from-scratch tabletop roleplaying game that
uses electronic modeling for the boring parts of game simulation.

# Building/testing/etc

First, get [stack](https://haskellstack.org/).

To run a little console UI for playing a sample game:

> stack build --exec pandt-exe

To run the tests:

> stack build --tests

If you make changes to the code, I recommend using --pedantic to run the builds, except for the
tests (which use a lot of partial functions).

# Roadmap

Also grep the code for XXX, FIXME, or TODO.

- nothing is done with the combat log from recurring effects
- add undo/game history
- creature construction?
- GM actions
  - during action vetting:
    - modify targets
    - modify effects
  - add/remove conditions
  - add creature (from... what?)
  - remove creature (make sure to remove all references!)
- Implement a basic text UI
  - allow skipping turn
  - check if targets exist
  - cancel an ability
- implement persistence
  - should be a simple auto-deriving of FromJSON/ToJSON
  - except, you know, backwards compatibility
- Implement a basic web UI (GHCJS oh boooy)
- Advanced combat mechanics, in rough order of interest:
  - combat log should show actual damage, not ability damage
  - Dispels! (how to target an effect? Or maybe it should be chosen for you?)
  - damage absorption (temporary hitpoints)
  - cooldown
  - stacking vs refreshing conditions
  - area effects with duration
  - randomization
  - crits
  - movement / geography
  - creature visibility
- Design three classes:
  - Warrior
  - Mage
  - Cleric
- Design a bunch of enemies
- Implement a progression system?
- Character balance tester using generative testing to see how builds perform
  in combat vs 1/N enemies, and with 1/N allies
