# [WIP] Versioning

The system is concerned with capturing the communication of modifications to the items that its users are concerned with, over time. As such, specific intents to modify these items of interest (or identities), are not directly applied when enacted, but rather enacted after first being recording in an immutable historical stream.

As discussed in the [data model](data_model.md), the primary high-level types that the system divides "the world" into are the `Block`, and `BlockData` types. In addition, to glue these two together, there is also the interface between them, the `Schema`, which is a particular type of `Block` that holds types that they both work with.

## Streams, Snapshotting and Efficiency

There are also the streams of immutable changes that capture intent, and can be grouped into larger and larger intent-captures. They can also be grouped vertically into a `Changeset`. These `Block`, `Schema` and `BlockData` and entities are pointers to snapshots of code and data, respectively (`BlockSnapshot`, and `BlockDataSnapshot`). These snapshots store a baked down versions at a point in time. The streams are the versions, and represented by streams of `BlockMigration` and `BlockDataMigration` types.

## Changes to a Block

So, when talking about `Block` changes, we can talk about two kinds of versions that are important: Firstly, changes to the `Schema` version it is written against, the interface (or API if you will) that the code conforms to (which changes less frequently), and secondly, changes to the actual code within the `Block` itself (changes much more frequently, but still less frequently than its data). Thus there is a `BlockSnapshot` stream for each `Block` and therefore also each `Schema` (as a `Schema` is a `Block`). A `Block` is simply an identity pointer that `BlockMigration` records all hold references to, and which points to a one of many `BlockSnapshot` records, which also point to the `Block`.

## Changes to a BlockData

When talking about `BlockData` changes, there are similar versions: the less-frequently changing `Schema`, deciding the structure and form of the data and what fields mean what to the code and how the `JSON` is interpreted, and the version of the `BlockData` itself which is the much more freqently changing data within it. This encapsulates the streams of `BlockDataMigration` and `BlockSnapshot` records.

## Changes to a Schema

There is, finally, the `Schema`, which is primarily (maybe even exclusively) composed of types, and is therefore written in Haskell code and is an interface describing to the `Block` how to use and interpret the `BlockData` into typed values, and describing to the `BlockData` how it should be structured, and what data it can store. Again, the `Schema` is actually itself a type of `Block` (because it is code, written in Haskell). We must be careful to ensure that all items that depend on a `Schema`, for example, are migrated at the same conceptual moment in time as the `Schema` changes come into effect.

## Migrations

The primary way of indicating a change between versions of `Block`, `Schema` or `BlockData` is through a `Migration`. `Block` and `Schema` use Haskell AST as their `Schema`. On top of these `Schema`, the language used to express migrations is written in Haskell. It's a reasonably simple composable extensible transformation language.

## Changesets

The primary method to use for recording changes is intent-driven changesets: the above described language language records intent context by wrapping migrations in a `Changeset`, and somehow connects each to an `Intent`. Thus there is a separation between the marking of `Intent`, and the marking of implementation, the latter of which is what a `Changeset` represents. A `Changeset` wraps migrations: `data Changeset = Changeset [Changeset] | Migration`. That is, it provides an implementation that corresponds to a marking of intent. Thus, it will be represented by a `Block`.

Interestingly, the beginning of this language seems to revolve around intent capture and a relaxed attitude toward correctness (that is, things may be better or worse at being a solution to things). Also useful to note is that using Pictures and Icons for filtering mechanisms might be the "restaurant menu solution" for getting rid of most keyboard typing and (along with the context) letting us easily filter possiblities.

Migrations require back and forth migration code to be written so that data and structure can be moved backwards and forwards in time.

