# Substance

The purpose of Substance is to allow its users to create and maintain the 'productions' they wish to create. These productions can be live websites, programs or documents. Theoretically they can be anything at all on the data/code spectrum. In the process of allowing this, it also attempts to [capture the intent](doc/intent_capture.md) of its various users (of all kinds), and capture a version trail of the changes over time to these 'productions'.

Substance is, therefore, a substrate for a new kind of 'content' management system (we call a piece of this content a 'production' due to its potentially non-static quality). Substance sits on the idea that data and code are twin aspects of a semantically inseparable whole: this is the idea that content is tied to its context. Its aim is to progress productions in accord with their changing requirements, and its output is capable of being 'productions' that are as 'live' or static as is required. It caters to the fact that it's expected that these productions can and will evolve or change over time.

## Services and Architecture

The services it provides to achieve these aims are: web and file serving functionality, database connectivity, a base [data model](doc/data_model.md), which includes a [versioning system](doc/versioning.md), a backend code compilation system, and connects to the upper (inner?) layers of the rest of the system with an initial Basic Product-System Source Code Editor. This is what is used to build the rest of the Product-System. See the diagram at [`doc/architecture`](doc/architecture.jpg) for a visual overview of the full system.

## Fabricator

On top of `Substance`, we build the `Fabricator` layer. This provides for manually built basic versions of enough of the system to allow bootstrapping temporary scaffolds to allow the replacement of itself with the `Productor` system. This includes Information Architecture, Templates, Content and Designs just-enough to do this, at which point other editors can be built by these tools in such a way that they can replace and feed back into the `Schema`, `BlockData` and `Block` objects of `Fabricator` which will then make up the `Productor` system.

## Productor

This system is bootstrapped with the `Fabricator` system as discussed. It has two main types of productions: 1. client-facing productions, and 2. the continually invention and evolution of the next versions of itself as productions.

Using this system, we can adjust the system itself. This includes changing its `Schema` and `Data` as well as the code that underlies all of this stuff by using the editors that are built with and in it. The versioning becomes extremely important at this level as it lets us experiment with both data, schema and code changes while keeping the system working at different versions and locations (URLs). When a version of the system becomes stable enough to replace the main one, it's simply swapped in. We can decide to enable or disable various versions whenever we like. Having a `Migration` system that works with the versioning system to have semantic adjustments to both data and code means that adjustments are atomic and can be replayed as well as reverted in sync.

## Languages

By using Haskell for `Substance`, as well as the underpinning language of the layers that it provides, we can share common libraries of code between layers. The `Fabricator` and `Productor` systems necessitate building other languages that are used via graphical user interfaces. These translate back to Haskell as a kind of AST before the final compilation stage, which means the final code can run anywhere Haskell targets, including components that need to be run in the browser.

Even though Haskell is a purely functional language, the end result of this approach is a system that is actually reasonably object-oriented in the sense that it bundles types, code, data and their adjustment code together into one unit.
