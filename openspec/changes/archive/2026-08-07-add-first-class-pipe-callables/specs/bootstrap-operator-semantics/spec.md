## ADDED Requirements

### Requirement: Pipelines apply one unary callable

A pipeline SHALL evaluate its completed left expression exactly once, then evaluate the callable
expression on its right and invoke that callable with the left value as its sole argument.
The right side MAY be a named function, automatic leading-argument section, binding, grouped
expression, or other expression with compatible unary callable type. Pipelines SHALL associate
left-to-right and MUST NOT create method lookup, implicit imports, or runtime namespace objects.

#### Scenario: Pipe into an actor section

- **WHEN** a body returns `2 |> I32.add(3)`
- **THEN** `I32.add(3)` first denotes a unary callable and the pipeline invokes it with `2`, producing `5`

#### Scenario: Pipe into a callable binding

- **WHEN** `increment` holds `I32.add(1)` and a body returns `2 |> increment`
- **THEN** the pipeline invokes the stored callable and produces `3`

#### Scenario: Chain applications left-to-right

- **WHEN** a body returns `2 |> I32.add(3) |> I32.multiply(4)`
- **THEN** the first application produces `5` and the second produces `20`

## MODIFIED Requirements

### Requirement: Operator-authored programs reuse the backend-neutral pipeline

Elaboration SHALL erase surface operator sugar into ordinary canonical HIR builtin calls and SHALL
erase pipeline syntax into canonical unary callable application while retaining the complete source
expression span. Lowering SHALL reuse the existing MIR operation and callable-application
vocabulary; interpretation, LLVM emission, and WebAssembly emission SHALL consume that MIR without
a surface-operator-specific or surface-pipeline-specific path. Equivalent programs SHALL have the
same result or trap behavior, and repeated compilation SHALL produce deterministic facts and
encodings.

#### Scenario: Preserve arithmetic traps across execution paths

- **WHEN** an operator-authored program overflows or divides by zero
- **THEN** interpreter, native, and WebAssembly execution all trap at the operator expression's provenance

#### Scenario: Keep MIR backend-neutral

- **WHEN** `40 + 2` and `40 |> I32.add(2)` are lowered for native and WebAssembly targets
- **THEN** both targets consume the same canonical arithmetic and callable plans with target-aware layout

#### Scenario: Repeat operator compilation

- **WHEN** equivalent operator and callable-pipeline programs are compiled repeatedly in fresh processes
- **THEN** syntax, semantic facts, HIR, MIR, diagnostics, symbols, and emitted artifacts are deterministic

## REMOVED Requirements

### Requirement: Pipelines insert one explicit first argument

**Reason**: Pipelines now apply ordinary unary callable values; argument-zero omission belongs to
automatic section construction rather than pipeline-only elaboration.

**Migration**: Treat `value |> Actor.operation(later)` as applying the callable section
`Actor.operation(later)` to `value`; use a stored callable or grouped callable expression where
composition previously could not be expressed.
