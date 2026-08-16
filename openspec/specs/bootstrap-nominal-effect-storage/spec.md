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
environment field exactly once. Direct owned extraction of the representation-bearing field MUST be
rejected, and scoped captures MUST NOT escape through the nominal.

#### Scenario: Drop an unrun Effect

- **WHEN** a `Deferred` containing an owned environment leaves scope without execution
- **THEN** its live captures are cleaned once without entering the runner

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
