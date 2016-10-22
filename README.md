# Substance

The purpose of Substance is to allow its users to create and maintain the 'productions' they wish to create. These productions can be live websites, programs or documents: in theory, they could be anything at all on the data/code spectrum. In the process of allowing this, it also attempts to [capture the intent](doc/intent_capture.md) of its various users (of all kinds), and capture a history (as a version trail) of the changes that played out to form these 'productions'.

Substance is, therefore, a substrate for a new kind of 'content' management system. We call pieces of this content 'productions' due to their potentially non-static quality. Substance sits on the idea that data and code are twin aspects of a semantically inseparable whole. This idea is that content is tied inexorably to its context. Its aim is to progress productions in accord with the intent of their creators, and their changing requirements. Its output is capable of being 'productions' that are anywhere on the spectrum between 'live' and static as is required. It caters to the fact that it's expected that these productions can and will evolve or change over time.

## Services and Architecture

The services it provides to achieve these aims are: web and file serving functionality, database connectivity, a base [data model](doc/data_model.md), which includes a [versioning system](doc/versioning.md), a backend code compilation system, and connects to the upper (inner?) layers of the rest of the system with an initial Basic Product-System Source Code Editor. This is then used to build the rest of the Product-System. See the diagram at [`doc/architecture`](doc/architecture.jpg) for a visual overview of the full system.

## Fabricator

On top of `Substance`, we build the `Fabricator` layer. This provides a way to manually build basic versions of enough of a temporary scaffold of the system to bootstrap a replacement of itself as the `Productor` system. `Fabricator` includes Information Architecture, Templates, Content and Designs just-enough to do this, at which point other editors built by these tools replace these, and feed back into the `Schema`, `BlockData` and `Block` objects of `Fabricator` itself which makes up the `Productor` system.

## Productor

As discussed, this system is bootstrapped within the `Fabricator` system. It has two main types of productions: 1. client-facing productions, and 2. the continual invention and evolution of the next versions of itself as productions.

By using this system, we can adjust the system itself. This includes changing its `Schema` and `Data` as well as the code that underlies all it all, by using the editors that are built with and in it. The versioning is extremely important at this level as it lets us experiment with both data, schema and code changes while keeping the system working at different versions and locations (URLs). When a version of the system becomes stable enough to replace the main one, it's simply swapped in. We can decide to enable or disable various versions whenever we like. Having a `Migration` system that works with the versioning system to have semantic adjustments to both data and code means that adjustments are atomic and can be replayed as well as reverted in sync.

## Languages

By using Haskell to write `Substance`, as well as using it as the language for the other layers that it provides, we can share common libraries of code between layers. The `Fabricator` and `Productor` systems necessitate building other languages that are used via graphical user interfaces. These translate back to Haskell as a kind of AST though, which means the final code can run anywhere Haskell targets, including components that need to be run in the browser (via GHCJS or somesuch).

Even though Haskell is a purely functional language, the end result of this approach is a system that is actually reasonably object-oriented in the sense that it bundles types, code, data and their adjustment code together into one logical unit.
