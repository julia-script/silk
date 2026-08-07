## ADDED Requirements

### Requirement: HIR retains exact Usize operations

HIR SHALL retain exact `Usize` literal magnitude, canonical operand and result types, unsigned
operator identity, source evaluation order, and provenance. HIR MUST NOT contain a selected LLVM
integer type, Wasm value type, host-number approximation, or backend instruction.

#### Scenario: Inspect a large literal operation

- **WHEN** an accepted native function adds two `Usize` literals above the 32-bit range
- **THEN** HIR encoding shows both exact values and one canonical checked unsigned addition
