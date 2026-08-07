## REMOVED Requirements

### Requirement: Flow calls are lazy and run evaluates one layer

**Reason**: The public abstraction is renamed to Effect and `effect {}` becomes the primitive lazy boundary.
**Migration**: Replace `flow fn`, Flow types, and Flow actor operations with `effect fn`, Effect types, and Effect actor operations.

### Requirement: Failure rows are normalized nominal contracts

**Reason**: The contract remains but is now defined for Effect functions and expressions rather than Flow declarations.
**Migration**: Preserve the `!` row while replacing the enclosing Flow spelling with Effect.

### Requirement: Fail transfers one owned payload abortively

**Reason**: Failure transfer now distinguishes ordinary Copy failure values from affine values that require `move`.
**Migration**: Use `fail value` for Copy values and `fail move value` when transferring a non-Copy binding.

### Requirement: Exact-member catch subtracts and composes rows

**Reason**: The actor namespace changes from `Flow.catch` to `Effect.catch` and handlers accept general statically typed Effect functions.
**Migration**: Replace `Flow.catch<E>` with `Effect.catch<E>`.

### Requirement: Traps remain outside typed failure

**Reason**: Trap separation remains but its handler namespace and cleanup guarantees are clarified under Effect.
**Migration**: Use `Effect.catch`; do not rely on trap unwinding or cleanup.

## ADDED Requirements

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
the current Effect execution with success type `Never`, copying a Copy payload or consuming an
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

### Requirement: Provision distinguishes capture from acquisition

`Capability.provide` SHALL capture an existing provider and MUST NOT imply per-run cleanup.
`Capability.provideWith` SHALL acquire a fresh affine provider owner per execution and drop every
successfully acquired owner after success or typed failure without replacing the original outcome.

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
