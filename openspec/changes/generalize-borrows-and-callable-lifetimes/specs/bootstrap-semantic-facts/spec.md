## MODIFIED Requirements

### Requirement: Operator and pipeline facts expose their canonical resolution

Section facts SHALL identify the canonical callable, ordered remaining leading parameters, supplied
trailing arguments, capture access, original parameter ordinals, substitutions, and resulting
callable contract for every non-empty trailing suffix.

#### Scenario: Inspect a staged section

- **WHEN** semantic analysis sees `combine(3)(2)`
- **THEN** the fact retains remaining parameter `a` and captures `c` then `b` with their original ordinals

### Requirement: Slice types and borrows retain canonical semantic facts

Every borrow SHALL retain its stable logical owner and complete field or checked-index selector path.
An owned temporary SHALL receive a deterministic compiler-owned identity rather than requiring a
source binding name.

#### Scenario: Inspect temporary and indexed roots

- **WHEN** one function borrows `&[1, 2]` and another borrows `&matrix[index]`
- **THEN** facts distinguish a hidden temporary owner from a named root plus runtime index selector
