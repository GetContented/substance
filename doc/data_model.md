# Database and Data Model

## Postgres and Block, BlockImport, BlockData

Postgres is used as a database system to store the versioning system model, data and much of changing parts of the code. The interface that the model provides yields the concepts of `Block`, `BlockImport` and `BlockData`. A `Block` is a self-contained chunk of code similar to a module in many programming langauages.

Each `Block` has a number of `BlockImport` objects related to it, which is a reference to another `Block` in the system such that they get programmatically reified into Haskell `import` statements at the beginning of that `Block` before compilation takes place.

## Schema

Each `Block` also has access to one or more named `BlockData` items, which has data stored in `JSON` format, according to a `Schema`, which is a `Block` that simply has the code required to describe the types and `Aeson` code to describe how to interpret the `BlockData` for that particular `Schema`. (Note: `Schema` are not first-class `Substance` records; rather, they belong to the `Fabricator` layer of `Substance`).

While this is the main model of our system, in order for the system to provide a versioning mechanism, the underlying implementatin of the data model works differently than presented. This is in accord with the intent capturing mechanism the system provides.

## Identity and High Level Versioning

The system needs to be able to retrieve things that have the same identity but with different versions. The model makes retrieving the latest version of an identity very fast, and provides that changesets are quick to apply and persist. This is discussed further in [versioning](versioning.md).

## Changesets

Changesets are a fundamental concept that this strategy employs. They are granular, atomic and yet also capable of spanning an entire group of identities at once (where their related characteristic is simply a requirement that the changes need to be delivered at once). This is possible with tagged versions: versions that are all connected to a single meta-identity. Much like a pointer, this allows for quick updating and rolling back.

The data model supports these things at a basic level so that we can provide mechanism for publishing and planned atomic and speedy updating of websites and documents. It also provides the system with forward-dated publishing by using a context-driven retrieval mechanism (where date is part of that context).

## Snapshotting and Versioning

Snapshotting is how we reify our stream of captured modifications down to a single piece of data.

In fact `Block`, and `BlockData` are simply references to snapshots: they mark identities that refer to particular `BlockSnapshot` and `BlockDataSnapshot` entities respectively, which are built out of the immutable streams of captured intent to make changes, which are effectively `Migration`s for `Block`, `Schema` and `BlockData`.

A snapshot stores a cached version of the reified object, or identity.

## Languages

Changes to `Block` data is in the Haskell language structure. Its syntax is captured in Haskell code in the [Language.Haskell.Exts.Syntax](https://hackage.haskell.org/package/haskell-src-exts-1.18.2/docs/Language-Haskell-Exts-Syntax.html#t:Exp) package, which we'll endeavour to implement a structure editor for while continuing to build a nice way to provide higher and higher possible ways to capture of intent.

Changing `BlockData` is similar, but easier because we will generally have a much simpler "language" that this takes place in, and therefore the structure will be simpler as well. This structure is recorded in the `Schema`. The editing operations will be recorded against specific `Schema` versions. The general idea is to move meaning out away from the code level as much as possible (by building simple languages out of DSL-like functions), and work with these POL-style (problem-oriented language) semantic level constructs which may eventually allow us to generate code anyway.

So, effectively, a "language" is captured in intent-captured versioned `Schema` for a `Block` and its `BlockData`, written in Haskell. That `Schema` then affords the `Block` and the `BlockData` to be freely and independently changed, while recording all intent in versions as activity. The `Block` is written in Haskell, but the `BlockData` is written in the `Schema` as the interface between the data and its code.

When migrating `Schema`, it is required to also write according migrations for `Block`, and `BlockData`. We use type checking here to ensure everything lines up before we can transition to this new version.

Because the entire system is written in Haskell, it's entirely possible that the system may end up being meta-recursive (that is, built from within itself) and somewhat self-hosting at some point in the future.

## Data Formats

Internally, in the database, we store snapshots of `BlockData` as `JSON`, using `Aeson` on the types in `Schema`, and we also store Haskell code in the same manner as its `AST` using `JSON`. This requires that we have `Aeson` instances for both Haskell and the migration language we represent changes to it in.

## Versioning and the Database

While we will make every attempt at making the initial database versioning schema robust and as all-encompassing as possible, realising that it's not likely we can predict every requirement until they arise, we allow that the system can accommodate more than one database and switch between them in order to provide for the whole schema to be adjusted or replaced.
