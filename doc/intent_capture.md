## Intent Capture

The system is designed interface->backend, but built backend->interface. We capture intent to whatever degree we can whenever we devlop and record, and use this to inform the way everything back to the core of the system works.

We capture intent by what people do. Some people like to just get in there and do things, and others like to plan (work on the structure first before content). There is a high work cost to pay for translating to a structured thing when a non-trivial amount of work has been put into unstructured work.

If the user works on the meta-content at the same time as the content, it will pay off for them down the line because our system can record the intent properly rather than just unintelligently recording the activity and differences. However, our system does not enforce this. We suggest this by recommending users start with a set of defaults that are constrained to their use-case, and when they try to push against the constraints of the structure, the system connects them to someone capable of adjusting the structure for them in an intent-recording way.

So, changes take place through creation of actions. We don't delete something, we make a deletion event. We don't create something, we create a creation event.

System authors act in the same way.

## Why record intent?

Why go to all the trouble of trying to record intent constantly? Communication. This system is about building 'productions' in such a way that they achieve the aim of their creators. If this intent is recorded, others can help out with those aims and possibly implement them in better ways. One could easily envisage a scenario where the intent and implementation are decoupled: that is, the intent is captured but the implementation isn't. Later it could be filled in by someone who understands how to implement it well.

This collaborative effort is sometimes like open source; with volunteered work, but equally for anyone working on particular things, paid or not. The more all of us understand the intent of anyone trying to do anything, the better, because we can work from a position of greater intelligence in understanding activity. Thus, collaboration of a greater quality is fostered. Much of this can be automatically recorded by providing particular *features* to users, and evolving these features over time. In particular, providing meta-tools to users so that they can shape their *own* intent-capturing and intent-communication experiences.
