## ADDED Requirements

### Requirement: Effects can be collected sequentially at a fixed arity

`Effect.zip` and `Effect.zip3` SHALL run their operands in declaration order and collect every
success value into ordinary public data — `Pair<A, B>` and `Triple<A, B, C>` respectively — whose
fields are readable from any module that can see the type.

Execution SHALL stop at the first typed failure. An operand that follows a failed one MUST NOT run,
and it MUST be released by the ordinary local cleanup of the frame the failure propagates out of, so
no unrun operand is stranded.

The result SHALL carry the union of every operand's failure row and the union of every operand's
requirement row, and MUST NOT add a failure or requirement of its own. Collecting the values MUST
NOT allocate.

Arity SHALL be extended by adding a parameter rather than by accepting a collection. Each operand is
a distinct parameter, so no Effect value is stored in runtime-indexed storage and every one of them
stays inside the hidden-identity specialization that erases it before lowering. Bootstrap MUST NOT
promise a combinator that takes a runtime-sized collection of Effects; that requires Effect values
to have a storable target layout, which they do not have.

Both combinators SHALL be ordinary Silk declarations with no intrinsic, no dedicated HIR or MIR
operation, and no compiler-side name recognition.

#### Scenario: Collect two success values in order

- **WHEN** two Effects that both succeed are combined with `Effect.zip`
- **THEN** the first Effect runs before the second and the returned pair carries both success values in that order

#### Scenario: Stop at a first-operand failure

- **WHEN** the first operand of `Effect.zip` fails with a typed failure
- **THEN** the second operand never runs, that same failure reaches the caller with its payload intact, and the unrun second operand is released exactly once

#### Scenario: Propagate a later operand's failure

- **WHEN** the second operand of `Effect.zip` fails after the first has succeeded
- **THEN** the failure reaches the caller unchanged and no pair is constructed

#### Scenario: Collect three success values in order

- **WHEN** three Effects are combined with `Effect.zip3` and the middle one fails
- **THEN** the first operand has already run, the third operand never runs, and the middle operand's failure reaches the caller

#### Scenario: Union every operand's rows

- **WHEN** operands with distinct failure rows and distinct requirement rows are combined
- **THEN** the resulting Effect's failure row is the union of theirs and its requirement row is the union of theirs, with nothing added

#### Scenario: Read the collected values from another module

- **WHEN** a caller in another module projects `first` and `second` from the returned pair
- **THEN** the projection is accepted, because the fields are public

## MODIFIED Requirements

### Requirement: Standard Effect combinators are library-defined

`map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`, `ensuring`, `zip`, `zip3`,
`provide`, `provideMut`, and `provideWith` SHALL resolve to canonical ordinary Silk declarations. The
compiler MUST NOT select their semantics from their names, actors, library origin, or a dedicated
combinator HIR/MIR operation. Equivalent user code using the compiler-owned Effect core SHALL
receive the same typing, ownership, execution, and cleanup behavior.

#### Scenario: Navigate and compile map as Silk

- **WHEN** a program calls or navigates to `Effect.map`
- **THEN** the target is canonical shipped Silk source compiled through ordinary declaration, callable, ownership, specialization, and lowering paths

#### Scenario: Navigate and compile exclusive provision as Silk

- **WHEN** a program calls either the data-first or piped form of `Effect.provideMut`
- **THEN** the target is canonical shipped Silk source and both forms produce the same contract and execution behavior

#### Scenario: Navigate and compile finalization as Silk

- **WHEN** a program calls or navigates to `Effect.ensuring`
- **THEN** the target is canonical shipped Silk source built from outcome reification and re-raise, with no combinator-specific compiler operation

#### Scenario: Navigate and compile sequential collection as Silk

- **WHEN** a program calls or navigates to `Effect.zip` or `Effect.zip3`, in either the data-first or the piped form
- **THEN** the target is canonical shipped Silk source built from ordinary `run` statements, with no combinator-specific compiler operation and no intrinsic

#### Scenario: Define an equivalent user combinator

- **WHEN** user source defines the same generic success-channel transformation under another name
- **THEN** it compiles and executes without intrinsic registration or a compiler-recognized operation identity
