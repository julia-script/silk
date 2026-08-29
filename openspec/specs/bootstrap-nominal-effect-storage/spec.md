# bootstrap-nominal-effect-storage Specification

## Purpose

Define static inline storage and execution of concretely represented Effect environments inside
move-only nominal values across ownership, suspension, cleanup, and every engine.

## Requirements

### Requirement: Effect representation bounds declare run access

Representation bounds SHALL distinguish shared `Effect<A ! E ? R>`, exclusive
`mut Effect<A ! E ? R>`, and consuming `once Effect<A ! E ? R>`. Shared realizations MAY satisfy
exclusive or consuming bounds, and exclusive realizations MAY satisfy consuming bounds; reverse
admission MUST be rejected.

#### Scenario: Reject a take-only Effect through shared access

- **WHEN** a generic body attempts to run `once Effect<A ! E ? R>` through `&Deferred`
- **THEN** ownership reports that the whole owner must be consumed

### Requirement: Nominals store concrete Effect environments lazily

A specialized Effect representation field SHALL store one concrete runner and environment inline in
the enclosing nominal without running it. The structural Effect contract SHALL retain no standalone
target ABI; only the complete nominal realization contributes build-internal lanes.

#### Scenario: Construct without running

- **WHEN** source constructs `Deferred` from an Effect with owned captures
- **THEN** the captures enter the nominal environment and no Effect body executes

### Requirement: Stored Effects preserve complete contracts

Stored Effects SHALL preserve success type, exact failure and requirement rows, run access, ordered
captures, loans, cleanup, and suspension state through nesting, parameters, results, borrowing, and
whole-value moves. Rows SHALL remain compile-time only.

#### Scenario: Run a stored Effect with exact rows

- **WHEN** `Deferred<A, !E, ?R, F>` is executed under access admitted by `F`
- **THEN** its source-observable result remains `A ! E ? R` with no runtime row dictionary

### Requirement: Stored Effect cleanup is exact

An unrun stored Effect, a successful or failed run, and a suspending run SHALL clean every live
environment field exactly once. A concrete reusable Effect whose captures are all Copy snapshots or
shared borrows MAY participate in an aggregate's sealed Copy proof and then has no cleanup. An owned
affine or exclusive capture SHALL keep the aggregate affine. Direct affine extraction SHALL report
the ordinary aggregate partial-move diagnostic, and scoped captures MUST NOT escape through the
nominal.

#### Scenario: Copy a reusable stored Effect

- **WHEN** an aggregate validly implements `Copy` and its concrete stored Effect has only Copy captures
- **THEN** an ordinary read duplicates the complete aggregate without adding cleanup

#### Scenario: Drop an unrun Effect

- **WHEN** a `Deferred` containing an owned environment leaves scope without execution
- **THEN** its live captures are cleaned once without entering the runner

#### Scenario: Drop an affine unrun Effect

- **WHEN** a stored Effect owns an affine capture and leaves scope without execution
- **THEN** its live capture is cleaned exactly once without entering the runner

### Requirement: Effect storage has suspension-aware cross-engine parity

Evaluator, LLVM, and direct WebAssembly SHALL consume one shared concrete realization containing
runner, layout, cleanup, access, and suspendability. A suspendability or capture-shape edit MUST
invalidate dependent layouts and emitted code. No backend may reconstruct Effect semantics or use a
standalone structural Effect ABI.

#### Scenario: Resume one stored suspending Effect

- **WHEN** a stored Effect suspends and resumes in each engine
- **THEN** result, failure behavior, cleanup trace, and static runner identity agree

### Requirement: Effect layout fences retire case by case

The unavailable-Effect-layout fence SHALL remain for any nominal storage path not proven through
ownership, layout, MIR, evaluator, LLVM, and direct WebAssembly.

#### Scenario: Preserve the fence during partial backend support

- **WHEN** evaluation supports one stored Effect shape but direct WebAssembly does not
- **THEN** compilation rejects that shape before MIR instead of claiming the capability complete

### Requirement: Active variants store concrete Effect environments lazily

An Effect-bounded field in a nominal-union variant SHALL use the same finite specialized runner,
environment, run-access, suspension, ownership, layout, cleanup, and storage-fence rules as an Effect
field in a nominal struct. Construction SHALL remain lazy, and only the active variant's Effect
environment SHALL exist, be runnable after pattern selection, or participate in cleanup.

#### Scenario: Store and run one selected Effect variant

- **WHEN** a concrete variant stores an Effect with owned captures and a consuming pattern selects it
- **THEN** construction runs nothing, selection transfers the exact environment once, and execution preserves its success, failure, requirement, access, suspension, and cleanup facts

#### Scenario: Preserve an unsupported Effect fence

- **WHEN** one reachable variant's Effect environment cannot be realized by every required phase and backend
- **THEN** the complete nominal-union application remains unavailable before MIR rather than gaining a standalone structural Effect ABI
