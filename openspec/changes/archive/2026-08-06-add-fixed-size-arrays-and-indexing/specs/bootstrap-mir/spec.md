## ADDED Requirements

### Requirement: MIR carries canonical logical array types

MIR locals, parameters, calls, and function results SHALL accept logical arrays identified by
canonical element type and length. Every reachable array use SHALL reference the same selected layout
and calling shape; MIR MUST NOT replace an array with an untyped scalar bundle.

#### Scenario: Lower an array factory

- **WHEN** a reachable function returns `Array<I32, 3>`
- **THEN** its MIR result and receiving locals retain that exact logical array type

### Requirement: Array construction and checked indexing are explicit MIR operations

MIR SHALL represent complete array construction with ascending canonical element operands. It SHALL
lower each readable Copy place chain to one checked read carrying the root aggregate local, ordered
field or index selectors, every dynamic `I32` index local and canonical length, the final Copy result
type, and exact trap provenance. Non-Copy intermediate aggregates MUST NOT become independently
owned locals. Whole moves, calls, returns, and drops SHALL continue to use ordinary operations over
complete logical values.

#### Scenario: Lower a dynamic index

- **WHEN** HIR indexes an array with a parameter
- **THEN** MIR contains one checked place read that either produces the final Copy value or traps at the index span

### Requirement: MIR verifies array consistency deterministically

Verification SHALL reject array operations whose element count, operand type, index type, canonical
length, selector path, layout entry, calling shape, destination type, or whole-value ownership mode
disagrees. Text encoding SHALL include canonical array types, lengths, selectors, operations, and
provenance in stable order.

#### Scenario: Reject a malformed construction

- **WHEN** an `Array<I32, 3>` construction carries two operands
- **THEN** verification reports the exact completeness violation before evaluation or emission
