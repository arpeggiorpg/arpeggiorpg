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
  - Casts can't finish because there's no distinction between the "cast" ability and the "effect" ability
- Redesign GM-vetting
  - Actions/Abilities should not be vetted; game states should be.
    - At the end of each player interaction (turn), effects should be applied to a *tentative* game state
    - that game state should be provided to the GM (only!) to be vetted
    - the GM should have the opportunity to make arbitrary changes to that state at will
    - vetting the state means accepting the tentative state as the current state
- stuns should probably interrupt casting
- should there be D&D-style "concentration"?
- combat log should show actual damage, not ability damage
- nothing is done with the combat log from recurring effects
- GM actions
  - during GMVettingAction (or, actually, during _any_ state?)
    - modify targets
    - modify effects
    - allow editing absolutely anything contained in a Game structure. All data inside must
      implement a GMEdit typeclass?
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
- combat log analysis: show dpr, total damage done/received/healed per character in encounter
- probably give creatures an ID separate from their name, and target based on IDs
