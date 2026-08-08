## ADDED Requirements

### Requirement: MIR represents callable environments in the structured DAG

MIR SHALL represent monomorphic callable construction, ordered captures, shared, exclusive, or
consuming environment access, direct or indirect application, and cleanup as typed operations and
regions in the existing backend-neutral acyclic control DAG. Verification SHALL reject open generic
callables, mismatched callable signatures, invalid invocation modes, duplicate capture transfers,
and cleanup that can occur before a retained dependency is released.

#### Scenario: Lower a reusable arithmetic section

- **WHEN** a stored `I32.add(2)` section reaches runtime
- **THEN** MIR contains one concrete callable environment and typed unary application without a surface pipeline operation

#### Scenario: Verify a consuming environment

- **WHEN** malformed MIR invokes a take-once environment twice
- **THEN** verification rejects the second application before evaluation or backend emission

### Requirement: MIR run order follows the elaborated operand

Lowering SHALL place `run` around the complete elaborated Effect operand chosen by syntax and HIR,
including every ungrouped pipeline combinator. Grouped run results SHALL remain ordinary values that
may feed later callable applications.

#### Scenario: Retry before run

- **WHEN** source spells `run attempt |> Effect.retry(2)`
- **THEN** MIR constructs the retry composition before entering its run region
