# The Language We Want to Build

Effect has shown us a remarkably powerful way to structure programs. I believe those ideas belong
all the way down to systems programming.

I created this project to explore what a low-level language built on that foundation could become:
a language with explicit control over memory and execution, shaped by Effect's approach to
dependencies, errors, concurrency, scopes, and observability.

This document explains why that language should exist and the principles that should guide its
design. It is not a language specification or a promise that every idea described here will survive
contact with implementation. The details will evolve; the direction should remain recognizable.

## Why Effect

I fell in love with Effect's approach to structuring programs: its concurrency model, errors as
values, resource-safe scopes, scheduling primitives, built-in tracing, and integration with logging
and OpenTelemetry.

Most of all, I love its dependency injection model. We have all seen dependency injection in many
shapes; Effect took the familiar idea and, in my view, perfected it.

For me, truly reusable code and genuinely useful interfaces used to live in the same category as
TDD, work-life balance, and up-to-date Notion documents—things I had learned to accept as
best-effort approximations.

Effect changed that. It made reusability feel concrete.

Every effect describes its own dependencies, and those dependencies can be satisfied by any service
that implements the required interface. Implementations are genuinely swappable without forcing the
rest of the program to know which one is being used.

Imagine being able to read a file, write a log, or make a request with the peace of mind that comes
from truly not caring how, where, or by whom it will happen.

Now imagine combining that model with Zig's explicit control over allocation, without passing an
allocator through every layer of the program. With a carefully designed relationship between
scopes, allocations, and explicit lifetimes, I believe we can find a compelling middle ground
between Zig's control and the memory-safety guarantees associated with Rust.

## Familiar to Effect developers

The language should feel immediately familiar to anyone who understands Effect.

Developers will need to learn additional low-level concepts, such as lifetimes and allocation
restrictions, but the overall structure of a program should remain recognizable:

- dependencies are explicit;
- errors are values;
- resources are scoped;
- concurrency is structured;
- services are replaceable;
- tracing and observability are built in.

The syntax will be considerably different from Effect's TypeScript API. Effect had to express its
model within the constraints of JavaScript and TypeScript, and it does that remarkably well. A
greenfield language does not have those constraints, so many patterns that require ceremony in
TypeScript can become direct language features.

The goal is not to reproduce Effect's syntax. It is to preserve the way Effect programs fit
together.

## Bootstrap with Effect, then self-host from evidence

The first compiler will be written in Effect. Yes, JavaScript will initially compile a low-level
language.

The first milestone is to build the smallest coherent version of the language that survives real,
recognizable programs. The TypeScript compiler should be replaced progressively once Silk is
capable of expressing compiler modules without making self-hosting itself choose premature language
features. Native fixed-point rebuilding is the acceptance gate when that work begins.

That transition should be natural, given that most of the primitives are the same. To make the
eventual port easier, the original implementation can deliberately avoid JavaScript patterns that
do not translate cleanly into a low-level environment.

The goal is not to build the entire language before using it, nor to port compiler files merely to
claim progress. The goal is to make each Silk-written module pressure a language model that is
already useful beyond the compiler, preserve equivalence with the stage-0 implementation, and then
develop the language using itself.

## Designed for humans and AI

We welcome AI. Indeed, most lines of code in this compiler will very likely be written by one — a fact we mention calmly, in the spirit of someone noting that most of the lifting at a construction site is done by the crane.

AI spam is a real problem, and I genuinely respect the decision some maintainers have made to ban AI contributions from their repositories entirely. It is a defensible position, arrived at by reasonable people who have seen things no code reviewer should have to see. In my view, though, denying AI contributions because the technology led to spam is rather like requiring everyone to file their taxes by hand, in ink, because Best Buy can't control itself — a policy that punishes the tool for the enthusiasm of its least careful operators.


The language should be intentionally friendly to AI-assisted development.

What is friendlier to something that has been trained on billions of lines of code if not these same languages?
Nobody knows, I don't, but I can certainly speculate.

That means limiting the number of ways to express the same idea. Developers should not have to
supervise an AI's stylistic choices or repeatedly correct arbitrary variations in structure.
Attention should remain focused on behavior, correctness, and design.

This does not mean making the language simplistic. It means making its conventions strong, its
semantics explicit, and its valid patterns easy to discover.

There is one important exception.

What defines Effect, in my view, is its refusal to define itself as either a functional or an
imperative programming library. Effect uses each style where it serves the problem best. It embraces
functional ideas when they provide composition, safety, and strong guarantees, while recognizing
that imperative code is often the simplest and most instinctive way to express a sequence of
operations. It lets developers move seamlessly between both, not as an escape hatch from one
preferred paradigm, but as equally legitimate ways of working within the same model.

It also means providing a strong standard library that covers the essential needs of complex
systems. A focused language should not achieve simplicity by pushing every practical concern onto
its users. Common capabilities such as concurrency, scheduling, collections, streams, resource
management, serialization, networking, observability, and testing should share one coherent design
and work together naturally.

The compiler and language server should expose rich, structured information to tools. Precise
diagnostics, dependable refactoring, excellent navigation, and first-class support for automation
are part of the language, not secondary tooling concerns. The ambition is for this experience to be
even better than Rust's.

## Compiler performance is a language feature

The language should not make the IDE sweat, and the compiler should never become the bottleneck in
an AI-assisted development loop. Fast feedback affects how people think, experiment, and refine
their programs. It is part of the language's usability, not merely an implementation detail.

Every major language feature should therefore be evaluated partly by its compilation cost. We
should be willing to sacrifice some convenience and require more explicit information when doing so
allows the compiler and language server to remain fast and predictable.

Historically, many language conveniences existed to save developers from writing repetitive or
mechanical code. That tradeoff is changing. AI has made work that was once tedious and expensive to
write remarkably cheap to produce and maintain.

The modern development workflow happens increasingly in the developer's mind rather than in their
fingers. The scarce resource is no longer keystrokes. It is attention, understanding, and feedback
time.

The language should optimize accordingly: prefer explicit code that tools can generate cheaply over
implicit behavior that is expensive for the compiler, the IDE, or the developer to understand. AI
can write the ceremony. The compiler should not have to perform archaeology.

## Observability by default

Semantic logging and tracing should be first-class Effect capabilities, distinct from raw standard
output. `Effect.log` should dispatch one complete structured event to an explicit `Logger` service;
the selected provider may render it to standard output, retain it in memory, forward it to
OpenTelemetry, write it to a browser console, or fan it out without changing the calling program.
The event boundary must work in browsers and other hosts that do not expose byte-at-a-time process
streams.

Tracing should build on the same observability model.

A function should be declarable as traceable, given a stable name, and automatically integrated
with the language's logging and telemetry system. Developers should not have to manually thread
tracing infrastructure through their programs.

Observability should be part of the execution model, not a library added after the fact.

## Portable services by default

Common services should describe portable program intent rather than the operating system mechanism
used to fulfill it. A program that reads a complete file or emits one log event should use the same
capability whether its provider is a native host adapter, an in-memory test implementation, or a
browser virtual file system.

Lower-level platform-specific services still matter for programs that need native paths, handles,
mapping, locking, terminal behavior, or another host facility that has no honest portable contract.
They should sit beneath or beside the common service instead of leaking platform details into it.
The standard library and documentation should lead developers toward the portable capability and
make the platform-specific escape hatch explicit.

## Serialization as a first-class capability

Serialization and deserialization should also be first-class language features.

Formats such as JSON, MessagePack, or another protocol still to be chosen should support
bidirectional encoding and decoding in the same spirit as Effect Schema. A single definition should
describe both the program's data and its representation at external boundaries.

Schemas should integrate naturally with validation, errors, tooling, foreign-function interfaces,
and generated bindings.

## Seamless native and WebAssembly integration

WebAssembly is a primary target, not an afterthought.

Calling between this language and JavaScript should feel almost like importing an ordinary
function. Whether JavaScript calls into a compiled module or the compiled module calls JavaScript,
the tooling should generate the necessary bindings and handle serialization automatically.

The same service abstraction should work across environments. A program might use a native standard
output implementation on the operating system and a browser implementation when compiled to
WebAssembly, without requiring the program itself to change.

Effect and this language should work particularly well together. The boundary between them should
be thin, typed, and largely generated.

## Why this might be possible

This project already has a working LLVM builder, providing a foundation for the compiler backend.

Building a complete language, even a deliberately small one, is a long road. But we are not starting
without guidance. Effect provides a rich and well-tested reference for the programming model, while
languages such as Zig and Rust provide lessons in explicit resource management, systems
programming, and memory safety.

Combined with modern AI-assisted development, that makes this project ambitious but plausible.

The goal is to create a small, low-level language with:

- Effect's approach to dependencies, errors, concurrency, scopes, and observability;
- Zig-like clarity and control over allocation;
- strong memory-safety guarantees inspired by Rust;
- excellent native and WebAssembly interoperability;
- first-class schemas and serialization;
- a coherent standard library capable of supporting complex systems;
- fast, predictable compilation and language tooling;
- and a constrained, toolable design that works equally well for humans and AI.

The result should feel like Effect freed from the constraints of TypeScript and rebuilt for
low-level programming.
