## ADDED Requirements

### Requirement: Intrinsic operations resolve only through the sealed namespace

`Intrinsic` SHALL be one compiler-sealed namespace binding. Compiler-provided callable operations
MUST resolve only as qualified members of that namespace and MUST NOT occupy independent actor
bindings such as `i32`, `Allocator`, or `StandardStreams`. Source code SHALL NOT declare, import,
alias, or shadow the reserved `Intrinsic` binding.

#### Scenario: Resolve a qualified scalar intrinsic

- **WHEN** source names `Intrinsic.i32Add`
- **THEN** resolution selects the canonical intrinsic operation rather than a source declaration

#### Scenario: Keep a scalar actor ordinary

- **WHEN** source names `i32.add` after the standard-library wrapper is in scope
- **THEN** resolution selects the source declaration and not the concrete intrinsic directly

#### Scenario: Reject shadowing Intrinsic

- **WHEN** a declaration or import attempts to bind the name `Intrinsic`
- **THEN** the module scope reports a deterministic collision with the sealed namespace

### Requirement: Service names resolve as source declarations

A service identity and its operation names SHALL resolve from the declaring source module with the
same visibility, import, collision, and canonical-identity rules as other declarations. The
compiler MUST NOT synthesize a service binding because a requirement row contains its spelling.

#### Scenario: Import a service explicitly

- **WHEN** a module imports Logger from its canonical source module
- **THEN** Logger requirements and qualified operations resolve to that source declaration
