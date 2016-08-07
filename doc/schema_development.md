# Schema Development

In order to develop a `Schema`, we need to provide the following sets of things:

- types that describe the shape of the `BlockData`
- usage functions that allow creation, deletion, retrieval and updating of the `BlockData`
- migration functions for transforming the content of `BlockData` from the previous `Schema` version into this one and vice versa