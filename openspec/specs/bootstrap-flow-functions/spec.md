# bootstrap-flow-functions Specification

## Purpose

Define Silk's lazy statically shaped flow values and its exact owned typed-failure channel, including
one-layer execution, propagation, recovery, and separation from unrecoverable traps.

## Requirements

### Requirement: Effect expressions and functions are lazy

Evaluating `effect { ... }` SHALL construct `Effect<A ! E ? R>` without entering its body.
Invoking an `effect fn` SHALL have the same behavior for its entire body. `run` SHALL evaluate exactly
one Effect layer. Ordinary `fn` statements outside an explicit effect block SHALL execute eagerly.

#### Scenario: Preserve an eager setup boundary

- **WHEN** an ordinary function computes one value and returns an effect block that uses it
- **THEN** the setup executes at the call while the block executes only when run

### Requirement: Effect construction has hidden nominal identity

Every `effect {}` construction site SHALL produce one compiler-only nominal Effect instance with a
target-planned capture environment and generated runner. The public structural Effect contract MUST
NOT expose that identity, and the implementation MUST NOT use a universal runtime interpreter or
erase different construction sites merely because their public contracts match.

#### Scenario: Return a delayed computation across a function boundary

- **WHEN** an ordinary function performs eager setup and returns `effect { ... }`
- **THEN** the returned Effect preserves its construction-site identity and captured environment until it is run or dropped

### Requirement: Effect failure rows are normalized owned contracts

The `!` row SHALL be a deterministic normalized set of canonical nominal types. `fail` SHALL stop
the current Effect execution with success type `never`, copying a Copy payload or consuming an
explicitly moved affine payload. Failure payloads MUST be detached owned values with no lexical or
provider borrow.

#### Scenario: Fail with a Copy problem

- **WHEN** an Effect executes `fail problem` for a Copy nominal value
- **THEN** the failure channel receives the copied value without requiring `fail move`

### Requirement: Effect catch subtracts and composes rows

`Effect.catch<E>(effect, handler)` and `effect |> Effect.catch<E>(handler)` SHALL handle only an `E`
present in the protected row, remove that member when coverage is complete, union the handler's
failures, bypass the handler on success, and propagate nonmatching members unchanged.

#### Scenario: Recover through a pipeline

- **WHEN** `relay(0) |> Effect.catch<Problem>(recover)` fails with `Problem`
- **THEN** `recover` owns the payload and its success becomes the pipeline result

### Requirement: Capture access derives repeatability

Copy captures SHALL snapshot at construction, shared captures SHALL permit repeated shared runs,
exclusive captures SHALL require exclusive runs while preserving mutations across runs, and an
Effect whose execution consumes a captured affine owner SHALL be take-once.

#### Scenario: Reject a second consuming run

- **WHEN** one execution consumes a moved capture and the caller runs the same Effect again
- **THEN** ownership rejects the second run and identifies the consumed capture

### Requirement: Retry accepts only repeatable Effects

`Effect.retry` SHALL reconstruct execution-local state for every attempt while reusing captures. It
MUST reject a take-once Effect. Providers acquired inside the retried Effect SHALL be reacquired;
captured providers SHALL be reused.

#### Scenario: Preserve mutable retry state

- **WHEN** a repeatable Effect mutates an exclusive captured counter and is retried
- **THEN** each attempt receives fresh locals while observing the counter changes from earlier attempts

### Requirement: Provision distinguishes shared borrow, exclusive borrow, and acquisition

`Effect.provide` SHALL capture an existing provider through a shared borrow and MUST reject an
Effect whose selected capability-role requirement needs exclusive access. `Effect.provideMut`
SHALL capture an existing provider through an exclusive scoped borrow and SHALL accept any provider
type that conforms to the selected service capability. Neither borrowed operation SHALL imply
provider ownership or per-run cleanup. `Effect.provideWith` SHALL acquire a fresh affine provider
owner per execution and drop every successfully acquired owner after success or typed failure
without replacing the original outcome.

#### Scenario: Provide one shared service

- **WHEN** an Effect with one shared service requirement is composed with `Effect.provide(&service)`
- **THEN** the resulting Effect borrows that provider, removes the selected requirement, and preserves its success, failure, and remaining requirement channels

#### Scenario: Provide one exclusive service implementation

- **WHEN** an Effect requiring exclusive access to capability `C` is composed with `Effect.provideMut(&mut provider)` and the provider type conforms to `C`
- **THEN** the resulting Effect borrows the provider exclusively, removes that capability-role requirement, and writes provider mutations back before the borrow ends

#### Scenario: Catch outside per-run acquisition

- **WHEN** a failing Effect is wrapped by `provideWith` and then by `Effect.catch`
- **THEN** the per-run provider drops before recovery begins

### Requirement: Traps remain outside Effect failure and cleanup

Bounds violations, arithmetic traps, impossible compiler states, and violated unsafe contracts
SHALL remain abnormal termination. `Effect.catch` MUST NOT intercept them, and bootstrap MUST NOT
promise Drop unwinding after a trap.

#### Scenario: Trap bypasses catch and cleanup claims

- **WHEN** a protected Effect divides by zero
- **THEN** execution traps without invoking the typed handler or reporting structured cleanup completion

### Requirement: Effect combinators accept ordinary callable values

`Effect.map`, `flatMap`, `tap`, `catch`, and other higher-order Effect operations SHALL accept
ordinary callable values and automatic sections under explicit callable contracts. Their direct
data-first forms and piped section forms SHALL be semantically identical. `map` SHALL preserve a
returned Effect as a nested success value, while `flatMap` and the Effect-specific behavior of
`tap` SHALL compose execution according to their declared contracts.

#### Scenario: Map with an arithmetic section

- **WHEN** `succeed(2) |> Effect.map(i32.add(2))` is run
- **THEN** the section maps the success to `4` without pipeline-specific callback syntax

#### Scenario: Keep effectful logging out of map

- **WHEN** an effectful logging function is passed to `Effect.map`
- **THEN** its Effect result remains nested rather than being executed implicitly

### Requirement: Callable captures derive composed Effect access

An Effect combinator that stores a callback SHALL incorporate the callback environment's shared,
exclusive, or consuming access into the resulting Effect's run access. Retry MUST reject a composed
Effect whose callback or input Effect is take-once. Dropping the composed Effect without running it
SHALL release the stored callback exactly once.

#### Scenario: Make map take-once

- **WHEN** `Effect.map` captures a mapper that consumes one owned capture
- **THEN** the mapped Effect is take-once and a second run is rejected before invoking the mapper

#### Scenario: Preserve exclusive callback state

- **WHEN** a mapped Effect uses a `mut fn` callback across repeated runs
- **THEN** each run requires exclusive Effect access and observes the callback's retained mutations

### Requirement: Logging remains effectful

Semantic logging SHALL remain an Effect operation with its declared Logger requirement. Ordinary
functions that add logging MUST return or execute an Effect through the existing effect model; this
change MUST NOT introduce an eager non-effect trace or debugging intrinsic.

#### Scenario: Propagate a temporary semantic log honestly

- **WHEN** a previously eager computation adds a Logger operation
- **THEN** its Effect and Logger requirements propagate to the execution boundary rather than bypassing the type system
### Requirement: Effect recipes compose uniformly

Every semantically valid nesting of Effect construction, transformation, recovery, retry, and
service provision SHALL retain the same contract and execution behavior in data-first calls,
left-associated pipelines, explicitly grouped expressions, and stored intermediate values. `run`
SHALL execute exactly the composed outer Effect regardless of source shape. Construction-time
callable and provider evaluation, run-time operation order, failure and requirement rows, capture
access, and cleanup MUST remain equivalent across those forms.

#### Scenario: Map a provided Effect directly from an effectful entry

- **WHEN** an effectful `main` runs `source |> Capability.provide(provider) |> Effect.map(mapper)`
- **THEN** the provider satisfies the source requirement, the mapper receives the success once, and the entry completes with the mapped result

#### Scenario: Reverse the transformation and provision order

- **WHEN** a requirement-preserving transformation is applied before the required provider is supplied
- **THEN** provision satisfies the transformed Effect's requirement and execution agrees with the equivalent provision-first form

#### Scenario: Store a composed pipeline before running it

- **WHEN** a valid multi-combinator Effect pipeline is bound and run later
- **THEN** it behaves like the direct expression while preserving construction-time captures and without introducing a trap

#### Scenario: Preserve affine success through a mapped provided Effect

- **WHEN** a provided Effect succeeds with an affine value that a mapper consumes
- **THEN** the mapper receives ownership exactly once and every remaining owned component is cleaned exactly once
### Requirement: Effect exposes three transformable channels

An Effect contract SHALL treat success `A` and typed failure `E` as covariant output channels and
its access-qualified requirement row `R` as a contravariant input channel. Ordinary Silk library
code SHALL be able to transform either output, adapt an unknown requirement row through a typed
provider, or compose an effectful transformation while preserving every untouched channel and the
input Effect's run access.

#### Scenario: Transform every pure channel

- **WHEN** library code maps `Effect<A ! E ? R>` with `A -> B`, `E -> F`, and a typed requirement adapter from `R2` to `R`
- **THEN** it produces `Effect<B ! F ? R2>` without inspecting a runtime row value or changing execution timing

#### Scenario: Remove one requirement from an unknown remainder

- **WHEN** a provider satisfies one capability-role entry in `Effect<A ! E ? Capability | Rest>`
- **THEN** the resulting Effect has contract `Effect<A ! E ? Rest>` for any normalized `Rest`

### Requirement: Completed Effect outcomes can be reified compositionally

The compiler-owned Effect core SHALL offer an effectful operation that executes exactly one Effect
layer and reifies its completed typed outcome as ordinary `Result<A, E>` data instead of propagating
`E`. The operation SHALL preserve `R`, ownership, cleanup, run access, and lazy timing, and its
contract SHALL remain valid if execution can suspend before producing the Result in a future
runtime. Traps and future interruption MUST NOT be converted into typed `E` values.

#### Scenario: Map both completed branches in library code

- **WHEN** ordinary Silk code reifies `Effect<A ! E ? R>` and matches its Result with success and failure callbacks
- **THEN** either callback can produce the corresponding transformed channel while `R` remains required

#### Scenario: Preserve future suspension transparency

- **WHEN** a future execution suspends before its typed outcome completes
- **THEN** outcome reification waits compositionally and does not expose a pending state as `Result<A, E>`

### Requirement: Standard Effect combinators are library-defined

`map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`, `provide`, `provideMut`, and
`provideWith` SHALL resolve to canonical ordinary Silk declarations. The compiler MUST NOT select
their semantics from their names, actors, library origin, or a dedicated combinator HIR/MIR
operation. Equivalent user code using the compiler-owned Effect core SHALL receive the same typing,
ownership, execution, and cleanup behavior.

#### Scenario: Navigate and compile map as Silk

- **WHEN** a program calls or navigates to `Effect.map`
- **THEN** the target is canonical shipped Silk source compiled through ordinary declaration, callable, ownership, specialization, and lowering paths

#### Scenario: Navigate and compile exclusive provision as Silk

- **WHEN** a program calls either the data-first or piped form of `Effect.provideMut`
- **THEN** the target is canonical shipped Silk source and both forms produce the same contract and execution behavior

#### Scenario: Define an equivalent user combinator

- **WHEN** user source defines the same generic success-channel transformation under another name
- **THEN** it compiles and executes without intrinsic registration or a compiler-recognized operation identity

### Requirement: Synchronous Effects retain a suspension-compatible abstraction

The public Effect contract SHALL NOT expose a concrete callback ABI, scheduler object, continuation
frame, runtime requirement record, or complete-or-suspended representation. Non-suspending programs
MUST NOT require a scheduler, fiber allocation, atomic synchronization, or mandatory suspension
branch, while the compiler-owned execution boundary SHALL remain capable of lowering future
suspendable Effects without changing library combinator contracts.

#### Scenario: Run a closed synchronous pipeline

- **WHEN** a closed Effect call graph cannot suspend, fork, interrupt, or observe a fiber
- **THEN** execution uses the synchronous runtime model and links no concurrency runtime solely because it uses library Effect combinators

#### Scenario: Preserve the future runner seam

- **WHEN** suspension is added later
- **THEN** existing source-defined combinators continue to compose through `run` and outcome reification without matching a scheduler-private pending state
