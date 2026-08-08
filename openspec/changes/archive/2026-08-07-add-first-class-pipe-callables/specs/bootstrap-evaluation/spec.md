## ADDED Requirements

### Requirement: Evaluation executes callable values exactly

The evaluator SHALL construct monomorphic callable environments, preserve capture identity and
ownership, enforce shared, exclusive, and consuming invocation modes, invoke direct and stored
callables, and drop unconsumed environments exactly as specified by MIR. Callable trace events
SHALL be deterministic, bounded, and independent of JavaScript closure identity or garbage
collection.

#### Scenario: Reuse an exclusive callable sequentially

- **WHEN** a `mut fn` callback updates captured state across two legal invocations
- **THEN** evaluation returns both results in order and retains the mutation between calls

#### Scenario: Reject an already consumed callable

- **WHEN** a take-once callable is invoked after its owned capture was consumed
- **THEN** evaluation exposes the phase-owned rejection rather than duplicating or fabricating the capture

### Requirement: Evaluation distinguishes run grouping

The evaluator SHALL execute an ungrouped pipeline inside the operand of `run` and a pipeline outside
a grouped run over the resulting success value. Both forms SHALL preserve their distinct trace
order and one-layer execution behavior.

#### Scenario: Compare grouped and ungrouped run

- **WHEN** one program spells `run effect |> Effect.map(transform)` and another spells `(run effect) |> transform`
- **THEN** evaluation shows composition-before-execution for the first and value-transformation-after-execution for the second
