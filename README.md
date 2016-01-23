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

- Implement basic combat stuff:
  - damage [DONE]
  - healing [DONE]
  - death [DONE]
  - reporting of events [DONE]
- model/workflow for abilities being "constructed"
  - player choose ability [DONE]
  - player choose target(s) [DONE]
  - GM modifies targets
  - GM modifies effects on targets
  - GM vets [DONE]
  - GM executes [DONE]
  - GM skips [DONE]
- Implement a basic text UI
  - allow skipping turn
  - check if targets exist
  - cancel an ability
- implement persistence
  - should be a simple auto-deriving of FromJSON/ToJSON
  - except, you know, backwards compatibility
- Implement a basic web UI (GHCJS oh boooy)
- Think more about damage/healing severity
- Advanced combat mechanics, in rough order of interest:
  - DOTs and HOTs [DONE]
  - passage of time [DONE]
  - incapacitation [DONE]
  - abilities that apply a condition to the caster (without explicit targeting)
  - Dispels! (how to target an effect???)
  - damage buff/debuff
  - damage absorb
  - resource consumption
  - cooldown
  - stacking vs refreshing conditions
  - area effects with duration
  - cast times
  - interrupts
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
