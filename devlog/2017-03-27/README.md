# P&T Dev Log 2017-03-27

# Summary

Over the last couple of weeks I have been hard at work on the campaign management tools in P&T. I pretty much covered what that means in the [previous dev log](../2017-03-19/README.md). In this update I'll show what I've done with campaign management and saved games, and also talk about some implementation details of pathfinding and folder layout.

# What is P&T?

P&T (name to be changed) will be an extensible digital game system for table-top roleplaying.

What does that mean? Well, think about Pathfinder, HERO, or Dungeons & Dragons: traditional table-top games. Now, imagine that all of their features and systems are implemented as an app on your laptop, phone, or tablet. P&T aims to be something like that.

It's important to note that P&T is *not* just an implementation of some existing ruleset: it's a completely new system. In fact, it's not even really a game system, but a game engine that systems can be built on. The games will come later. Let me reiterate: P&T is not an implementation of D&D, Pathfinder, or any other system, and it doesn't aim to support those systems.

P&T is really very early in development. It can't be used to run games yet, though I am using its campaign-management and tactical map features in my weekly D&D games as a way to eat my own dog food.

# Recent Developments

## Campaign Management

Since the last update, I've completed the folder-organized campaign management system to a functional prototype level. Click the image below to see a video demonstration.

[![Demonstration of P&T campaign manager](https://img.youtube.com/vi/Q8NG7JrgifA/0.jpg)](https://www.youtube.com/watch?v=Q8NG7JrgifA)

In addition to what's shown here, there's also support for creating textual notes in folders.


## Saved Games

I finally added a very basic save/load system which allows saving to files in a folder on your hard drive. With this, I can finally use P&T for my own long-term campaign management, and I've already begun laying out maps for my group's adventure next month.

# Miscellaneous battlemap improvements

In support of my weekly campaign, I added a few features to the tactical map. Most important was the ability to mark creatures as hidden from players, so that I can set up a scene with a bunch of enemies just around a corner, and only show them when the player characters actually get a chance to see them. This is much quicker than having to add them to the scene manually during play! However, while this will probably always be a useful feature, it's a far cry from the dynamic line-of-sight support that I ultimately want to implement.

# Implementation Details

Here I'll cover a bit about the implementation of the game, with links to the open source code, written in Rust and Elm.

## Pathfinding

Even though I haven't been working on it recently, I wanted to write a bit about the pathfinding support in P&T.

[![Video of P&T's pathfinding and automatic movement destination calculation](https://img.youtube.com/vi/2bwhirvr8-U/0.jpg)](https://youtu.be/2bwhirvr8-U)

Above is a video that shows what happens when you move a creature in P&T. One piece of functionality I'm happy with is the way that all possible movement destinations are shown to you when you initiate movement. Then, all you have to do is click where you want to go and the pathfinder takes care of the rest.

I had to spend a bit of time optimizing this code -- my first naive implementation took a noticeable amount of time to calculate all possible movement destinations. I eventually hacked apart the A* implementation from the [pathfinding](https://crates.io/crates/pathfinding) crate to support searching for multiple destinations without having to recalculate the entire path every time. I'm pretty sure this code is still nowhere near optimal, since I basically knew nothing about pathfinding before implementing it, but the basic way it's implemented is:

- get a list of all points that are "open" (i.e. those which aren't filled with blocking terrain) within some distance of the current mover. ([code](https://github.com/radix/pandt/blob/cea6470d2537ca68c8ce7a26d07ee3fed7167aa4/pandt/src/grid.rs#L49))
- run the pathfinding algorithm to all of those points ([code](https://github.com/radix/pandt/blob/cea6470d2537ca68c8ce7a26d07ee3fed7167aa4/pandt/src/grid.rs#L191))
- the pathfinding algorithm uses one single HashMap to keep all the paths to all of the destinations. At the end, we go through all of the destination points and construct the paths from that map. ([code](https://github.com/radix/pandt/blob/cea6470d2537ca68c8ce7a26d07ee3fed7167aa4/pandt/src/grid.rs#L226))

The main way in which I changed the code which I copied from the pathfinding crate was to keep that "parents" HashMap for multiple searches, and then reconstruct all the paths from it at the end. While there's still probably a lot I could do to speed this code up, this optimization brought it to the point where I could comfortably use it in P&T.

## Campaign Object Ownership

As I mentioned in last week's dev log, one thing I was struggling with was how to structure the data in the campaign folde, and correspondingly, how to expose this structure to the user. The question was whether the Folder structure should "own" the data that it contains, or whether that data should be separate and only provide links to the data.

I ended up keeping my data in separate collections in the top-level Game struct([code](https://github.com/radix/pandt/blob/cea6470d2537ca68c8ce7a26d07ee3fed7167aa4/pandt/src/types.rs#L606)), and then have the folder tree just link to those objects by ID. However, instead of exposing the "linkiness" of the folder structure to the Remote Programming Interface/user interface, I hid that entirely. So even though it could be possible to have multiple links to the same data in a folder structure, I hid this functionality so the UI code only things from a folder-ownership perspective.

I also had to spend quite some time ensuring that the folder structure and the flat object store at the root wouldn't get out-of-sync. As one example, any time you delete an item, it scans the entire folder tree for any references to that item to make sure they get cleaned up (theoretically this shouldn't be necessary, since we could pass in the folder path in order to name the resource during deletion, but I wanted to make sure to account for any bugs that might leave extra links throughout the folder structure).

This goes extra for things which can be referenced from other places. For example, Creatures can be referenced from Folders, Scenes, and Combats, so I have to scan and clean up all of those things upon deletion. This scanning is arguably horribly inefficient, but I think it would require quite a huge campaign structure to make the inefficiency noticeable. That said, this could certainly be improved, either by implementing some stronger abstractions (like what a hard drive filesystem might have), or by just switching storage to a relational database and taking advantage of relational consistency features.

## FolderTree

In support of the campaign tree, I implemented a fairly generic [FolderTree data structure](https://github.com/radix/pandt/blob/cea6470d2537ca68c8ce7a26d07ee3fed7167aa4/pandt/src/foldertree.rs). I may publish it as a separate crate some day, if I get some time to polish it up (and convince myself that other people might find it useful).


# Where from here?

While my next big goals remain the same as what I described in the previous dev log (support for Adventure Modules and doing some game system prototyping), there are a few things that I might end up tackling before/during that work, in support of my day-to-day use of the battle map for my D&D game.

**Map annotations:** It would be very handy to have highlighted and annotated spots on the tactical maps. This would let me represent notable features of the environment to my players, and with access-control, also allow me to provide notes to the GM about things like traps and whatnot.

**Creature Size**: Right now, creatures are represented only by their position on the grid, and it is  assumed that one creature fills up exactly one grid square. This is problematic for a few reasons, most importantly because there are larger and smaller creatures in the games I want to run. But there's another reason that I think this could be helpful.

One thing that I would like to do once this is implemented is to make the "default" creature size larger than the grid size, probably by one size increment (so a creature takes up four grid squares). This would allow for much "smoother" edges in non-rectangular environments. One reason this is particularly important is how pathfinding around corners works. Naturally, we don't want to allow going around corners to be as easy as moving at a diagonal, since you would probably clip your shoulder into the wall. But the scale at which creatures and grids align mean that a diagonal wall can only be represented by a series of corners. This means moving along a diagonal wall takes up quite a lot more movement than it should. So scaling down the grid size relative to the creature size would mean I could have much more natural distance used.

Of course, making the grid size finer-grained is probably going to put a lot more strain on my pathfinding algorithm, so I may be forced to spend more time optimizing it.


## End

Thanks for reading, if you have! If you have any interest in a project like this, either as a writer, artist, system designer, programmer, or playtester, please give me a shout at pandt@wordeology.com.
