## ADDED Requirements

### Requirement: A conditional combinator selects one suspended branch and never builds the other

`Effect.ifThenElse` SHALL take a `bool` condition and two suspended arms, each a
`once fn() -> Effect<...>`, and SHALL invoke exactly the arm the condition selects. It MUST NOT
invoke the other arm.

Because an arm produces its branch rather than being one, the branch not taken SHALL never be
constructed. This is stronger than the branch's effect not being performed: construction-time work
inside an unselected arm SHALL NOT happen, and an arm whose body is only well-defined under the
condition SHALL be safe to write. A form taking two pre-built `Effect` values would not satisfy
this, because both branches would be evaluated at the call site before either was chosen.

The arm that is not invoked SHALL be released exactly once. No arm can own a resource: a zero-arity
callable is either a named function, which has no environment, or a section, and section
construction supplies "exactly parameters one through the last" to produce "a unary callable
awaiting parameter zero", so it always leaves arity 1 and never 0. A capturing value SHALL
therefore be rejected against an arm's declared contract rather than accepted and leaked.

The result's failure row SHALL be the union of the two arms' failure rows and its requirement row
SHALL be the union of theirs, so a caller discharges whatever either branch could need without
knowing which is selected. Both arms SHALL agree on the success type.

The combinator SHALL be named `ifThenElse`. `if` is lexed unconditionally as a keyword and Silk has
no raw-identifier form, so an `effect fn` named `if` cannot be declared at all — this is a
constraint on the declaration, not one a qualified call spelling could avoid.

#### Scenario: Perform none of the unselected branch's effects

- **WHEN** two arms call a counting service a different number of times and `Effect.ifThenElse` selects one of them
- **THEN** only the selected arm's service calls are observed, in either polarity

#### Scenario: Never construct the unselected branch

- **WHEN** the arms are ordinary functions that perform observable work at invocation before returning their Effects
- **THEN** only the selected arm's construction-time work happens, in either polarity

#### Scenario: Reject an arm that owns a resource

- **WHEN** a value holding an owned resource is supplied where a zero-arity arm is required
- **THEN** it is rejected, because a zero-arity arm has no environment to hold it and therefore nothing to leak

#### Scenario: Union both arms' rows

- **WHEN** the two arms declare different typed failures and different service requirements
- **THEN** the result carries the union of both failure rows and both requirement rows, and either branch's selection is satisfied by discharging that union

#### Scenario: Agree across engines

- **WHEN** a program selecting either branch is run on the evaluator, on Wasm, and through the native toolchain
- **THEN** the three engines produce the same result

## MODIFIED Requirements

### Requirement: Standard Effect combinators are library-defined

`map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`, `ensuring`, `ifThenElse`,
`provide`, `provideMut`, and `provideWith` SHALL resolve to canonical ordinary Silk declarations.
The compiler MUST NOT select their semantics from their names, actors, library origin, or a
dedicated combinator HIR/MIR operation. Equivalent user code using the compiler-owned Effect core
SHALL receive the same typing, ownership, execution, and cleanup behavior.

#### Scenario: Navigate and compile map as Silk

- **WHEN** a program calls or navigates to `Effect.map`
- **THEN** the target is canonical shipped Silk source compiled through ordinary declaration, callable, ownership, specialization, and lowering paths

#### Scenario: Navigate and compile exclusive provision as Silk

- **WHEN** a program calls either the data-first or piped form of `Effect.provideMut`
- **THEN** the target is canonical shipped Silk source and both forms produce the same contract and execution behavior

#### Scenario: Navigate and compile finalization as Silk

- **WHEN** a program calls or navigates to `Effect.ensuring`
- **THEN** the target is canonical shipped Silk source built from outcome reification and re-raise, with no combinator-specific compiler operation

#### Scenario: Navigate and compile the conditional as Silk

- **WHEN** a program calls or navigates to `Effect.ifThenElse`
- **THEN** the target is canonical shipped Silk source built from an ordinary `if` statement over suspended callable arms, with no combinator-specific compiler operation

#### Scenario: Define an equivalent user combinator

- **WHEN** user source defines the same generic success-channel transformation under another name
- **THEN** it compiles and executes without intrinsic registration or a compiler-recognized operation identity
