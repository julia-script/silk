## MODIFIED Requirements

### Requirement: Synchronous Effects retain a suspension-compatible abstraction

The public Effect contract SHALL NOT expose a concrete callback ABI, scheduler object, coroutine
frame, runtime requirement record, execution-stack allocator, or complete-or-suspended
representation. A closed Effect call graph that cannot reach the suspension intrinsic MUST NOT
contain coroutine-frame transformation, scheduler or fiber linkage, atomic synchronization, a
mandatory complete-versus-pending branch, or a private suspension dispatcher. Existing
source-defined combinators SHALL compose with suspendable Effects without changing their contracts
or recognizing a private pending state.

#### Scenario: Run a closed synchronous pipeline

- **WHEN** a closed Effect call graph cannot reach suspension, fork, interruption, or a fiber observation
- **THEN** execution retains its direct synchronous entry and call shape and links no coroutine or concurrency runtime solely because it uses library Effect combinators

#### Scenario: Preserve the runner seam under suspension

- **WHEN** a source-defined combinator runs an Effect whose reachable call graph can suspend
- **THEN** the compiler-owned execution boundary resumes the composition without changing the combinator's public signature or exposing a pending state

### Requirement: Effect suspension is explicit lazy composition

The canonical ordinary Silk function
`Effect.suspend<A, E, ?R>(deferred: once Effect<A ! E ? R>)` SHALL defer execution of `deferred`
until the returned Effect is run and SHALL transfer its execution through the explicit stack-safe
boundary. Its result contract SHALL be exactly `A ! E ? R`: suspension MUST NOT add an allocation
failure or allocator requirement. Each concrete suspendable invocation SHALL reuse one statically
shaped coroutine frame across its possible suspension states. Dynamic execution-stack exhaustion
SHALL be a fatal trap outside the typed failure channel. The compiler MUST NOT recognize the public
function by actor, module, or operation spelling.

#### Scenario: Keep suspension lazy

- **WHEN** an Effect with observable work is passed to `Effect.suspend` and the returned Effect is not run
- **THEN** the deferred work does not execute and dropping the returned Effect releases its captures exactly once

#### Scenario: Preserve the child channels

- **WHEN** `Effect.suspend` receives `Effect<A ! E ? R>`
- **THEN** the returned Effect has exactly `A ! E ? R` with no `OutOfMemory` member and no `Allocator` requirement introduced by suspension

#### Scenario: Preserve a nested Effect success value

- **WHEN** the deferred child succeeds with `Effect<i32>` as its declared success value
- **THEN** one run of `Effect.suspend` produces that nested `Effect<i32>` value without flattening or running it

#### Scenario: Exhaust private execution storage

- **WHEN** compiled suspended recursion exhausts its finite compiler-owned execution stack
- **THEN** execution traps without producing a typed failure or permitting `Effect.catch` to recover the exhaustion

#### Scenario: Do not interpret suspension as parking

- **WHEN** a running Effect reaches `Effect.suspend`
- **THEN** it transfers synchronous execution of its deferred child without creating a task, parking for a wakeup, yielding scheduler fairness, or adding interruption and cancellation semantics

### Requirement: Explicit suspension covers recursive cycles, not recursive declarations

A terminating self-recursive or mutually recursive Effect graph SHALL use bounded native and Wasm
machine stack when every possible recursive cycle crosses an explicit suspension origin. A
suspension origin on an unrelated or avoidable branch SHALL NOT cover a cycle. Recursive functions
and Effects without a covered cycle SHALL remain valid Silk and MUST NOT receive a mandatory
compiler diagnostic solely because their depth is unbounded.

#### Scenario: Cover mutual recursion with one suspension edge

- **WHEN** every path around a mutually recursive Effect cycle crosses one explicit `Effect.suspend` edge
- **THEN** terminating execution uses bounded native and Wasm machine stack even though the other recursive edges do not suspend

#### Scenario: Leave an uncovered cycle valid

- **WHEN** a recursive Effect cycle can execute without crossing any suspension origin
- **THEN** the compiler accepts the otherwise valid program without promising bounded machine stack

#### Scenario: Ignore suspension on an unrelated branch

- **WHEN** a recursive cycle can avoid a branch containing `Effect.suspend`
- **THEN** that branch does not establish the bounded-machine-stack guarantee for the cycle

### Requirement: Suspension imposes no allocator implementation restriction

An ordinary implementation of the `Allocator` service SHALL be permitted to suspend whenever its
declared Effect contract permits suspension. The compiler MUST NOT apply a suspension-specific
bootstrap, recursion, conformance, or self-hosting restriction to that implementation.

#### Scenario: Suspend inside an allocator operation

- **WHEN** an `Allocator` implementation satisfies its ordinary service contract and one operation reaches `Effect.suspend`
- **THEN** it is checked like any other service implementation and receives no suspension-specific diagnostic

### Requirement: Source-defined Effect combinators compose across suspension

`Effect.map`, `Effect.flatMap`, outcome reification, recovery, retry, provision, and equivalent user
combinators SHALL compose with a suspended Effect through their existing ordinary Silk definitions
and public signatures. They MUST NOT inspect or expose a pending state, coroutine frame, driver
token, or private runner ABI. Suspension SHALL preserve the child's failure and requirement rows
exactly; combinators SHALL compose only the rows contributed by their ordinary inputs and callbacks.

#### Scenario: Map after suspension

- **WHEN** a suspended Effect succeeds and its result is transformed with `Effect.map`
- **THEN** the mapper runs once after resumption and receives the original success value without adding a suspension-specific failure or requirement

#### Scenario: Flat-map into suspension

- **WHEN** `Effect.flatMap` selects a suspended Effect from an input success
- **THEN** execution waits for the suspended child and preserves the ordinarily unioned failure and requirement rows without exposing a pending representation or adding storage channels
