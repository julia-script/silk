## MODIFIED Requirements

### Requirement: HIR represents callable values canonically

HIR SHALL retain every ordered remaining leading parameter, every captured trailing argument's
original parameter ordinal, and the source evaluation order across successive direct section stages.

#### Scenario: Retain three-stage application

- **WHEN** `combine(3)(2)(1)` reaches HIR
- **THEN** section construction captures `3` then `2` once and final application supplies `1` plus those positional captures

### Requirement: HIR retains lexical slice semantics explicitly

HIR slice and reference borrows SHALL carry a named, parameter, pattern, or compiler-owned temporary
root plus the complete ordered field and checked-index selector path. Traversal SHALL include hidden
temporary expressions and runtime selector expressions exactly once.

#### Scenario: Retain an indexed inner-array borrow

- **WHEN** HIR lowers `&mut matrix[index]`
- **THEN** it records `matrix` as the root and the checked runtime array selector without copying the inner array
