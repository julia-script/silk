## ADDED Requirements

### Requirement: Scoped-allocation forms are lossless and bounded

The syntax layer SHALL represent qualified unsafe allocation and `Slot<T>` operations, restricted
drop-hook declarations, and explicit consuming `drop` without erasing their tokens or source spans.
Allocation SHALL reuse the established `flow fn`, failure row, requirement row, role, provision, and
`Scope.scoped` surface rather than inventing an allocation-specific scope block or provider syntax.
Recovery from a missing type argument, operand, or boundary delimiter SHALL remain local and
deterministic.

#### Scenario: Preserve a complete allocation form

- **WHEN** a source file contains a flow wrapped by `Scope.scoped` and qualified unsafe allocation and slot operations
- **THEN** the syntax tree and canonical formatting preserve every operation, type argument, role, wrapper, and source span

#### Scenario: Recover a damaged initialization form

- **WHEN** an initialization operation omits its value or closing delimiter
- **THEN** parsing records explicit missing nodes within that operation and continues at the enclosing statement boundary
