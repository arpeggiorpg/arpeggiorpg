# Phone-and-Tablet Roleplaying Game

This is a project to implement a from-scratch tabletop roleplaying game that
uses electronic modeling for (almost) all game simulation.

## Game Design

The core philosophy:

- Players missing NPCs sucks. let's not have that.
- WoW rotations are engaging, but very complex, and mapping them directly to D&D
  makes the game *extremely* slow.
- Choosing an action should be very quick and easy
- Thus, at least taking some of WoW rotation gameplay shouldn't slow the game
  down too much.

Ideas for fun things to do in combat:

- cast times
  - more often for enemies, especially for the really big attacks
  - for players, most abilities have 0 cast time; occasionally 1; rarely 2.
- interrupts to stop enemy casting
- stunning to temporarily incapacitate enemies
- ability cooldowns

Not being able to do things is frustrating and boring! So players should rarely
get interrupted, stunned, or grappled. Mostly those should be things they do to
enemy NPCs.

### Mobility

Ranged characters should want to avoid being next to a melee enemy, since they
can be interrupted or just damaged a lot. Melee characters want to avoid being
away from a ranged character, since they can unload damage at full force then.
So melee must be balanced with ranged to keep the game interesting. Melee
characters should have gap-closers to get in a ranged enemy's face; ranged
characters should have gap-openers to do the opposite.

closers:
- immobilize/slow enemy at range so you can walk up to them
- charge enemy
- deathgrip enemy
openers:
- blink/dash/expeditious retreat
- knockback
- immobilize/slow so they can't catch up with you


## on multi-targeting:

If a power has multiple effects with multiple targets, we allow the ability to
be executed *if* all targets are targetable/vetted. This works because we don't
have a mechanism by which abilities can miss! But if we introduce such a
mechanism, then we have to rethink things. For example, Fistweaver monk in WoW
will *not* cause a heal if they attack an "Immune" target.

For such an ability, we could essentially have a "monadal" Ability or Effect...
Like, a case of Effect which is DoOneEffectAndThenDoAnother, which only does the
second if the first succeeds. For UX purposes, this case should be handled
specially when executing the ability, so you choose all the targets at once (but
at the same time, have the opportunity to change the target?)

# Geometry/spatial modeling

So I'd like to start off WITHOUT a map modeled in the game, while still allowing
adding maps to the game without having to completely rewrite all abilities and
rebalance the game. How do we do this?

In D&D when I play without a map I basically mentally model the following
scenarios:

- close enough to melee without spending a move
- able to move and then melee OR within range of a typical ranged attack
- would have to dedicate an entire turn to moving, to be able to attack next
  round

There are certainly plenty of edge cases, but these handle the large majority of
combat scenarios.

While it's tempting to model this abstractly, I think the best bang for the buck
will be to punt on the problem altogether, and simply rely on GM vetting of all
targeting.

# Roadmap

- Figure out the non-mapped model for geometry/space [CHECK]
- Implement basic combat stuff:
  - damage [CHECK]
  - healing [CHECK]
  - death
  - movement
- model/workflow for abilities being "constructed"
  - player choose ability [DONE]
  - player choose target(s) [DONE]
  - GM vets [DONE]
  - GM executes [DONE]
- Implement a basic text UI
- Implement a basic web UI (GHCJS oh boooy)
- Think more about damage/healing severity
- Advanced combat mechanics, in rough order of interest:
  - passage of time
  - DOTs and HOTs
  - stacking vs refreshing conditions
  - cast times
  - interrupts
  - incapacitation
  - randomization
  - crits
- Design three classes:
  - Warrior
  - Mage
  - Cleric
- Design a bunch of enemies
- Implement a progression system?
- Character balance tester using generative testing to see how builds perform
  in combat vs 1/N enemies, and with 1/N allies
