## ADDED Requirements

### Requirement: Bound receiver syntax shares the explicit bound-operation target

A bound operation reached through `value.op(args)` on a generic receiver SHALL select the same
bound operation, produce the same specialization key, and lower to the same static target as the
explicit `Bound.op(value, args)` form. The receiver SHALL be treated as the bound parameter's
operand; the written arguments SHALL follow.

#### Scenario: Specialize once across spellings

- **WHEN** a generic body calls both `value.print()` and `Printable.print(value)` for `T: Printable`
- **THEN** both record the same bound operation and one specialization per instantiation is emitted
