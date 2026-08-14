## ADDED Requirements

### Requirement: Effect suspension is explicit lazy composition

The canonical ordinary Silk function
`Effect.suspend<A, !E, ?R>(deferred: once Effect<A ! E ? R>)` SHALL defer execution of `deferred`
until the returned Effect is run. Its result contract SHALL be
`A ! E | OutOfMemory ? R | &mut Allocator`: continuation storage exhaustion is an explicit typed
failure and allocator requirement only at a suspension boundary. The compiler MUST NOT recognize
the public function by actor, module, or operation spelling.

#### Scenario: Keep suspension lazy

- **WHEN** an Effect with observable work is passed to `Effect.suspend` and the returned Effect is not run
- **THEN** the deferred work does not execute and dropping the returned Effect releases its captures exactly once

#### Scenario: Report continuation exhaustion explicitly

- **WHEN** the selected allocator refuses storage for a continuation at `Effect.suspend`
- **THEN** the suspended Effect fails with `OutOfMemory`, the deferred body does not start, and no continuation owner is created

### Requirement: Explicitly suspended Effect cycles are stack safe

A terminating self-recursive or mutually-recursive Effect cycle SHALL use native and WebAssembly
machine stack bounded by a constant independent of logical recursion depth when every recursive
cycle crosses `Effect.suspend`. The guarantee SHALL include non-tail work and owned state retained
after the suspended child completes. Recursion that does not cross `Effect.suspend`, ordinary
function recursion, and recursive Drop hooks SHALL receive no stack-safety guarantee.

#### Scenario: Resume non-tail self-recursion

- **WHEN** a recursive Effect suspends before its recursive run and adds one to the returned value at each of one million levels
- **THEN** native execution returns the expected value without exhausting the machine stack

#### Scenario: Resume mutual recursion

- **WHEN** two Effects call each other through explicit suspension until a finite counter reaches zero
- **THEN** native, WebAssembly, and evaluation produce the same typed result with machine-stack use bounded on native and WebAssembly

#### Scenario: Exclude ordinary recursive cleanup

- **WHEN** an ordinary Drop hook recursively destroys a deep heap-linked value without crossing an Effect suspension boundary
- **THEN** this capability makes no stack-safety promise for that destruction

### Requirement: Source-defined Effect combinators compose across suspension

`Effect.map`, `Effect.flatMap`, outcome reification, recovery, retry, and provision SHALL compose
with a suspended Effect through their existing ordinary Silk definitions and public signatures.
They MUST NOT inspect or expose a pending state, continuation frame, driver token, or private runner
ABI. The suspended Effect's existing failure and requirement rows, including `OutOfMemory` and
`&mut Allocator`, SHALL compose by the ordinary row rules.

#### Scenario: Map after suspension

- **WHEN** a suspended Effect succeeds and its result is transformed with `Effect.map`
- **THEN** the mapper runs once after resumption and receives the original success value

#### Scenario: Flat-map into suspension

- **WHEN** `Effect.flatMap` selects a suspended Effect from an input success
- **THEN** execution waits for the suspended child and preserves the unioned failure and requirement rows without exposing a pending representation

## MODIFIED Requirements

### Requirement: Synchronous Effects retain a suspension-compatible abstraction

The public Effect contract SHALL NOT expose a concrete callback ABI, scheduler object, continuation
frame, runtime requirement record, or complete-or-suspended representation. A closed Effect call
graph that cannot reach the suspension intrinsic MUST NOT contain continuation allocation,
scheduler or fiber linkage, atomic synchronization, a mandatory complete-versus-pending branch, or
a private suspension dispatcher. Existing source-defined combinators SHALL compose with
suspendable Effects without changing their contracts or recognizing a private pending state.

#### Scenario: Run a closed synchronous pipeline

- **WHEN** a closed Effect call graph cannot reach suspension, fork, interruption, or a fiber observation
- **THEN** execution retains its direct synchronous entry and call shape and links no continuation or concurrency runtime solely because it uses library Effect combinators

#### Scenario: Preserve the runner seam under suspension

- **WHEN** a source-defined combinator runs an Effect whose reachable call graph can suspend
- **THEN** the compiler-owned execution boundary resumes the composition without changing the combinator's public signature or exposing a pending state

