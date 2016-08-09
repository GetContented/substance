# WIP: Database and DataModel

So that the system can provide a versioning mechanism, the data model must be written to support this. Original prototypical versions were designed without versioning in mind, but this isn't acceptable for our production system.

## Identity

The system will need to be able to retrieve things that have the same identity and different versions. The model will need to be built in a way that makes retrieving the latest version of an identity very fast, and such that changesets are quick to apply and persist.

## Changesets

Changesets are the fundamental data type that this strategy employs. They can be granular, atomic and capable of spanning an entire group of identities at once (where their related characteristic is just that the changes need to be delivered at once). This should be possible with tagged versions: versions that are all connected to a single meta-identity. Much like a pointer, this allows quick updating and rolling back.

The data model needs to support these things at a basic level so that we can provide mechanism for publishing and quick planned updating of websites and documents. We can also employ forward dated publishing by using a date-driven retrieval mechanism.

## Snapshotting

Snapshotting is how we "bake down" our changeset stream at a point in time.

`Block`, and `BlockData` will be identity markers that refer to particular `BlockSnapshot` and `BlockDataSnapshot` entities, which are built out of `BlockChangeset` and `BlockDataChangeset` immutable streams, which are effectively `Migration`s between `Schema` or `BlockData`.

Maybe it's good enough to use textual parsers for the more basic levels of code. Higher level objects won't actually be using written "code" anyway, it'll just be referring to existing code. So, it'll mostly be data structures and references to other pieces of code. (That is, adjustments to this stuff will be easy to spot because we're parsing it so the diffs will be more obvious).

## Languages

Changing using `BlockChangeset` items will simply be textual transformations (like git).

Changing using `BlockDataChangeset` is much easier because we have the structure encoded in the `Schema`. The editing operations will need to be recorded. The general idea is to move meaning out away from the code level as much as possible (by building simple languages out of DSL-like functions), and work with these POL-style (problem-oriented language) semantic level constructs which may eventually allow us to generate code anyway.

At some stage in the future we *may* actually introduce a Haskell parser which will let us do a full metarecursive analysis and full editing on all the code in the system. For the short term this isn't a level of bootstrapping we'll actually be able to do very easily.