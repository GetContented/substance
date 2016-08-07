# Versioning

The system tracks changes over time by using immutable streams. Primarily, it tracks changes to `Block` (code) and `BlockData` objects by using two processes: diffing with changesets, and snapshotting. It tracks a stream of changes (with intent), along with a `Schema` that is itself versioned and tracks meta-changes, to *meaning* over time.

## Changes to a Block

Thus when talking about `Block` changes, we can talk about two kinds of versions that are important: Firstly, changes the `Schema` version it is written against, the interface (or API if you will) that the code conforms to (changes less frequently), and secondly, changes to the actual code data within the `Block` itself (changes much more frequently).

## Changes to a BlockData

When talking about `BlockData` changes, there are similar versions: the less-frequently changing `Schema`, deciding the structure and form of the data and what fields mean what to the code and how the `JSON` is interpreted, and the version of the `BlockData` itself which is the much more freqently changing data within it.

## Changes to a Schema

There is, finally, the `Schema` itself which is written in code and is an interface describing to the `Block` how to use and interpret the data into typed values, and describing to the `BlockData` how it should be organised.

## Changesets

The primary method to use for recording changes is intent-driven changesets. The changes to `Block` and `BlockData` are relatively trivial to track becuase they simply describe the CRUD on pieces of the text. However, the changes to `Schema` are a bit more involved, as they generally also require adjustment code called a `Migration`. This type of code is written whenever an adjustment to the `Schema` happens and it describes how to change all the existing data forward and backwards between the current and previous versions of the `Schema` structure.

## Snapshotting and Efficiency

You will probably be wondering how we arrive at particular versions if all we record is changesets. It seems like the more times we change things, the longer it would take to build up to a view of the info at the present. However, the system also manages a current snapthot which is the latest built version of a versioned item. That way we can efficiently transmit changes, but also quickly get the latest state easily.

