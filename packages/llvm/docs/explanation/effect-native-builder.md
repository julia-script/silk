# Why the builder is Effect-native

This explanation is about the package's state and failure model. It does not enumerate IR
operations or give steps for constructing a module.

LLVM construction looks imperative: declare a type, append a block, move an insertion point, emit
an instruction. A direct TypeScript port could expose a mutable class and throw whenever an
operation encounters invalid state. `@silk-lang/llvm` instead treats the module builder as an
Effect-native actor whose state is only reachable through typed operations.

## One owner, many opaque handles

The builder owns the actual module state. Public values such as `Type.Type`, `Function.Function`,
and `Block.Block` are opaque identities into that state, not independently mutable objects. You can
think of them as coordinates that are meaningful only on the map that issued them.

This is why cross-builder use is rejected. A numerically similar type or constant in another
builder is not the same coordinate, even when it renders identically. Ownership checks turn a class
of accidental state corruption into ordinary typed failures.

The alternative would be to embed mutable implementation objects in every public value. That can
make isolated calls look simpler, but it spreads lifetime management across the entire API and
makes deterministic snapshots and transactional rollback harder to guarantee.

## The serialized module gate

Effects may run concurrently, while a module's symbol and value numbering depend on ordered
mutation. The builder therefore serializes mutations through one semaphore. This does not make
arbitrary ordering deterministic across independently scheduled fibers; it guarantees that each
accepted mutation observes and commits against one coherent state.

The gate belongs at the builder boundary because every actor participates in the same module. A
separate lock in `Type`, `Constant`, and `Function` would permit interleavings that violate their
shared tables.

## Function bodies are smaller transactions

A half-built function is not useful module state. Missing terminators, incomplete PHI nodes, or a
late Effect failure should not leave instructions visible to later output. `Function.buildBody`
therefore brackets reservation and draft acquisition, lends the draft to one callback and fiber,
validates the finished control-flow graph, and commits it as a unit. One release path closes the
draft and reservation after success, typed failure, validation failure, defect, or interruption;
the callback's original exit remains observable.

This resembles a database transaction more than a conventional `IRBuilder` object. The body handle
is deliberately unusable after the callback because its only valid states are “being built here”
and “committed as an immutable module body.”

A permanently mutable body builder would allow incremental repair after errors, but it would also
make every renderer and encoder account for arbitrary incomplete states. Transactional bodies keep
that complexity at one boundary and make retries predictable.

## Errors remain values

External input controls names, layouts, types, constants, attributes, and control flow. Invalid
input is therefore expected program behavior, represented by `LlvmError` in the Effect channel.
The `operation` field locates the rejecting actor, `message` gives stable context, and `reason`
distinguishes `InvalidInput`, `InvalidState`, and `WrappedFailure`. Rejected values live on the
semantic reason; JavaScript `cause` is reserved for genuinely wrapped failures.

The distinction is intentional: invalid user IR is recoverable; corrupted internal tables are a
defect. Treating both as thrown `Error` values would erase the boundary and force callers to guess
which failures they can handle.

Inside a serialized mutation, synchronous state transitions return `Result` values. The owning
Effect boundary lifts expected failures into the typed channel while leaving unexpected throws as
defects. This keeps the mutation critical section synchronous without making yieldable errors do
double duty as thrown exceptions.
