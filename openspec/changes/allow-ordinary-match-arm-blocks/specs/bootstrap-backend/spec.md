## MODIFIED Requirements

### Requirement: LLVM lowering obeys compiler-owned MIR

LLVM lowering SHALL realize scalar and aggregate layout, ownership cleanup, calls, control flow,
traps, entry termination, C ABI boundaries, and target availability from MIR without inventing a
second semantic model. Textual IR is an inspection artifact and carries no compatibility promise.

#### Scenario: Reject inconsistent MIR

- **WHEN** MIR conflicts with the selected target layout or operation availability
- **THEN** verification rejects it before artifact construction

#### Scenario: Realize eager selected ordinary arms

- **WHEN** verified MIR selects a statement arm that mutates state and completes normally
- **THEN** LLVM performs only that selected statement sequence in source evaluation order and continues with unit, preserving the current computation

#### Scenario: Realize a transfer inside a larger expression

- **WHEN** verified MIR transfers from a statement arm inside an argument or initializer
- **THEN** LLVM emits the corresponding enclosing exit and cleanup without later operand execution, initializer or assignment storage, or a load from an uninitialized match result
