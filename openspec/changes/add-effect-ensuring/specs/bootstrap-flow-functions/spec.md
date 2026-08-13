## ADDED Requirements

### Requirement: A finalizer runs on every Effect outcome without replacing it

`Effect.ensuring` SHALL run its finalizer after the protected Effect completes with a success and
after it completes with a typed failure, and SHALL then hand on that original success value or that
original typed failure unchanged. It MUST NOT replace the outcome, add to the protected Effect's
failure row, or let a recovering caller observe the outcome before the finalizer has run.

The finalizer SHALL be typed `Effect<() ! never ? S>`, so a finalizer failure is unrepresentable
rather than reconciled against the outcome being preserved. A caller whose release can fail SHALL
recover it into that contract before composing it, and the resulting Effect's requirement row SHALL
be the protected Effect's row widened by the finalizer's own.

The protected Effect's local cleanup SHALL run before the finalizer. The finalizer is acquired
outside the Effect it wraps, so the reverse-acquisition order that governs locals places it last.

A trap SHALL bypass the finalizer, as it bypasses `Effect.catch` and every `Drop` hook. Bootstrap
MUST NOT promise finalizer execution after a trap.

#### Scenario: Finalize after a success

- **WHEN** an Effect that succeeds is wrapped by `Effect.ensuring`
- **THEN** the finalizer runs and the original success value reaches the caller unchanged

#### Scenario: Finalize after a typed failure

- **WHEN** an Effect that fails with a typed failure is wrapped by `Effect.ensuring` and then recovered
- **THEN** the finalizer runs before recovery begins and the recovery handler receives that same failure with its payload intact

#### Scenario: Order the finalizer after local cleanup

- **WHEN** a protected Effect holding an owned local is wrapped by a finalizer that holds owned locals of its own
- **THEN** the protected Effect's local is released first and the finalizer's locals are released afterwards in reverse acquisition order

#### Scenario: Release an owner acquired inside the protected Effect

- **WHEN** the protected Effect acquires an owner inside its own body and then fails with a typed failure
- **THEN** that owner is released exactly once before the finalizer runs, and the finalizer's own owners are released exactly once after it

#### Scenario: Compose a fallible release

- **WHEN** a release that can fail is recovered into `() ! never` and passed as the finalizer
- **THEN** the composition is accepted, the recovery decides what a failed release means, and the protected Effect's outcome is still preserved

#### Scenario: Trap bypasses the finalizer

- **WHEN** a protected Effect divides by zero
- **THEN** execution traps without running the finalizer and without reporting structured cleanup completion

## MODIFIED Requirements

### Requirement: Standard Effect combinators are library-defined

`map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`, `ensuring`, `provide`,
`provideMut`, and `provideWith` SHALL resolve to canonical ordinary Silk declarations. The compiler
MUST NOT select their semantics from their names, actors, library origin, or a dedicated combinator
HIR/MIR operation. Equivalent user code using the compiler-owned Effect core SHALL receive the same
typing, ownership, execution, and cleanup behavior.

#### Scenario: Navigate and compile map as Silk

- **WHEN** a program calls or navigates to `Effect.map`
- **THEN** the target is canonical shipped Silk source compiled through ordinary declaration, callable, ownership, specialization, and lowering paths

#### Scenario: Navigate and compile exclusive provision as Silk

- **WHEN** a program calls either the data-first or piped form of `Effect.provideMut`
- **THEN** the target is canonical shipped Silk source and both forms produce the same contract and execution behavior

#### Scenario: Navigate and compile finalization as Silk

- **WHEN** a program calls or navigates to `Effect.ensuring`
- **THEN** the target is canonical shipped Silk source built from outcome reification and re-raise, with no combinator-specific compiler operation

#### Scenario: Define an equivalent user combinator

- **WHEN** user source defines the same generic success-channel transformation under another name
- **THEN** it compiles and executes without intrinsic registration or a compiler-recognized operation identity
