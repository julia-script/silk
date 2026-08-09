## MODIFIED Requirements

### Requirement: Arithmetic evaluates exactly and traps on the pinned conditions

The evaluator SHALL execute every admitted integer width and mode without host-number precision loss. Checked overflow, invalid division/remainder, and invalid shift counts SHALL block with operation identity, reason, and provenance; wrapping, saturating, bitwise, comparison, and conversion behavior SHALL match both backends.

#### Scenario: Evaluate wide arithmetic

- **WHEN** `u64` uses values above JavaScript's exact integer range
- **THEN** evaluation returns the exact result or pinned trap without rounding

#### Scenario: Evaluate checked recovery

- **WHEN** a recoverable checked operation overflows
- **THEN** evaluation constructs `None` rather than trapping

### Requirement: Evaluation preserves exact Usize semantics

The evaluator SHALL represent target-selected `usize` exactly and use canonical unsigned decimal encoding.

#### Scenario: Evaluate native maximum

- **WHEN** native `usize` evaluates its maximum value
- **THEN** evaluation returns `18446744073709551615` exactly

