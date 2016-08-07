# Substance

Substance is a substrate for a new kind of content management system. It sits on the idea that data and code are syntactic aspects of a semantically inseparable whole.

It provides a backend code compilation system, versioning system, database connectivity, initial data model, web and file serving functionality and the initial Basic Product-System Source Code Editor, with which the rest of the Product-System is built. See the diagram at `doc/architecture` for a visual overview of the full system.

## Database and Data Model

Postgres is used as a backing store for the model, which primarily uses the concepts of `Block`, `BlockImport` and `BlockData`. A `Block` is a self-contained chunk of code similar to a module in many programming langauages. `Block` and `BlockData` are essentially stored as `text` in the database.

Each `Block` has a number of `BlockImport` records related to it, which is a reference to another `Block` in the system such that they get programmatically reified into Haskell `import` statements at the beginning of that `Block` before compilation takes place.

Each `Block` also has access to one or more named `BlockData` items, which has data stored in `JSON` format, according to a `Schema`, which is a `Block` that simply has the code required to describe the types and `Aeson` code to describe how to interpret the `BlockData` for that particular `Schema`. (Note: `Schema` are not first-class `Substance` records; rather, they belong to the layer above, (or "within" if you prefer) `Substance`).

## Fabricator

On top of `Substance`, we build the `Fabricator` layer. This consists of the remaining parts to this system. While `Substance` actually provides basic versions of all of these parts in order to bootstrap the providence, they're intended primarily as temproary measures to begin the ongoing development process of the `Productor` system, and should only be used to do that. `Fabricator` only really includes a way to work on building a code editor that can replace itself: including Information Architecture, Templates, Content and Designs enough to do this, at which point other editors can be built in such a way that they can feed back into the `Schema` `BlockData` and `Block` objects of `Fabricator` that makes up the `Productor` system.

## Productor

This system is bootstrapped with the `Fabricator` system as discussed, but then has two main aspects: client-facing product, and the invention of itself as a product.

Using this system, we can adjust itself. This includes changing its `Schema` and `Data` as well as the code that underlies all of this stuff by using the editors that are built in it. The versioning becomes quite important at this level as it lets us experiment with both data, schema and code changes while keeping the system working at different URLs. When a version of the system becomes stable enough to replace the main one, it's simply swapped in. We can decide to enable or disable various versions whenever. Having a `Migration` system that works with the versioning system to have semantic adjustments means that data or code adjustments are atomic and can be replayed as well as reverted.

## Languages

By using Haskell for `Substance`, as well as the underpinning language of the layers that it provides, we can share common libraries of code between layers. The `Fabricator` and `Productor` systems necessitate building other languages that are used via graphical user interfaces. These translate back to Haskell before the final compilation stage, which means the final code can run anywhere Haskell targets, including components that need to be run in the browser.

Even though Haskell is a purely functional language, the end result of this approach is a system that is actually reasonably object-oriented in the sense that it bundles types, code, data and their adjustment code together into one unit.
