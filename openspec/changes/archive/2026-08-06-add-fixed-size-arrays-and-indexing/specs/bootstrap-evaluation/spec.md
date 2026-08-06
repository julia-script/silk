## ADDED Requirements

### Requirement: Evaluation carries immutable complete array values

The evaluator SHALL represent an array by its canonical type and immutable ascending-index element
values. Construction SHALL evaluate each operand exactly once in MIR order; whole moves, parameter
binding, calls, returns, and cleanup SHALL preserve the complete logical value without exposing lane
realization.

#### Scenario: Evaluate a nested array call

- **WHEN** a complete nested array passes through an internal function and returns
- **THEN** every canonical length and element value is preserved without aliasing a partial source

### Requirement: Evaluation checks every dynamic index

Evaluation SHALL compare a dynamic `I32` index against zero and the canonical length before reading
the selected element or continuing a place chain. Failure SHALL produce a deterministic trap with the
index, length, function identity, and exact projection provenance.

#### Scenario: Trap a negative index

- **WHEN** execution indexes an array with `-1`
- **THEN** evaluation blocks at that index operation without reading an element

#### Scenario: Trace a successful indexed field read

- **WHEN** `pairs[index].left` completes
- **THEN** the trace identifies the canonical array, selected index, canonical field, resulting value, and source order
