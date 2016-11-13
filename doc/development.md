# Development

In order to develop the system, we need to provide the following sets of things:

- types that describe Haskell, and `Aeson` instances for them
  - experiment with the Language-Haskell-Exts-Syntax types in haskell-src-exts and deriving Aeson instances using its Generic instances (just need to make a newtype for the data)
- `Migration` records and functions for transforming records of all types in a version into this one and vice versa
- base database, storage and web functionality
- a `Schema` and `Blocks`
- types that describe the shape of the `BlockData`
- usage functions that allow creation, deletion, retrieval and updating of records
- some form of basic intent capture

