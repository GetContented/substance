# [WIP] Versioning

The system tracks changes over time by using immutable streams. Primarily, it tracks changes to `Block` (code) and `BlockData` (data) objects by using two processes: diffing with changesets (data that represents the code that changes the code and data), and snapshotting. It tracks a stream of changes (with intent), along with a `Schema` that is itself versioned and tracks meta-changes, to *meaning* over time.

## Changes to a Block

Thus when talking about `Block` changes, we can talk about two kinds of versions that are important: Firstly, changes to the `Schema` version it is written against, the interface (or API if you will) that the code conforms to (changes less frequently), and secondly, changes to the actual code data within the `Block` itself (changes much more frequently).

## Changes to a BlockData

When talking about `BlockData` changes, there are similar versions: the less-frequently changing `Schema`, deciding the structure and form of the data and what fields mean what to the code and how the `JSON` is interpreted, and the version of the `BlockData` itself which is the much more freqently changing data within it.

## Changes to a Schema

There is, finally, the `Schema` itself which is written in code and is an interface describing to the `Block` how to use and interpret the data into typed values, and describing to the `BlockData` how it should be organised.

## Changesets

The primary method to use for recording changes is intent-driven changesets. The changes to `Block` and `BlockData` are relatively trivial to track becuase they simply describe the CRUD on pieces of the text or structured data. However, the changes to `Schema` are a bit more involved, as they generally also require adjustment code called a `Migration`. This type of code is written whenever an adjustment to the `Schema` happens and it describes how to change all the existing data forward and backwards between the current and previous versions of the `Schema` structure.

## Snapshotting and Efficiency

You will probably be wondering how we arrive at particular versions if all we record is changesets. It seems like the more times we change things, the longer it would take to build up to a view of the info in the present. However, the system also manages a current snapshot which is the latest built version of a versioned item. That way we can efficiently transmit changes, but also quickly get the latest state easily.

## Thought / Question

What if we simply slotted our migration model into our existing block/data model?

It seems first we need to get really clear about exactly what our terms are:

1. Database: This is the lowest level data store of the system that exists to persist data, provided by server(s)
2. Substance System: While this includes everything in the system, it is also a layer that provides web serving, compilation and is the lowest level service for the system that powers everything else

2.1 Substance Model: This provides three structures: `Block`, `BlockImport` and `BlockData`, and a basic editor for CRUD on these things. With this mini-system, we build the Fabricator, with the aim that it replace use of this basic editor system for working on productions.
2.3 Fabricator: This uses the basic editor from `Substance` to build up a structure that provides the `Productor` system with editors and all it needs. In here, we can experiment with various sub-systems without having to rebuild the server layer.

From here onward, things are very much still WIP and in flux:
2.3.1 One way of approaching this is that we define a `Schema`, which is code and types that represent a snapshot of the structure of some data at a particular point in time. This approach has worked pretty well for quite a while for developers over time, but a problem with it is that it doesn't necessarily provide well for versioning. A question to consider is... is it very important that we adjust the *lowest* layers of structure?

It seems that the database structure could  be easily made to fit our purposes for a stream based processing system. We *may* need to adjust the way it retrieves (which seems to be only by id and name at the moment), so that we can have an entire stream of `BlockData` objects that we can retrieve and use to event-source process into snapshotted states.

What is a version? It seems that 'versioning' is doing dual-purpose here, a little. We can mean either a whole bunch of changes (with their intents), which mark a particular version of a production, or we can mean the incremental differences between adjustments (which may or may not be named or specified explicitly, yet still need to be tracked somehow).

Another of the problems that seems to be arising is that of "how to track code adjustmen intent" I mean, this is the ultimate question, after all, isn't it? We might track Haskell and changes to intent at *some* later stage, but that won't be for this version... so can we allow for that expandability later?