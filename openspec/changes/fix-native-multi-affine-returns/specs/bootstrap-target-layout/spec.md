## ADDED Requirements

### Requirement: Composite affine result lanes remain distinct

For a reachable result type containing multiple generic affine fields, target planning SHALL retain
every field's complete calling-shape subtree, canonical selector path, scalar representation, and
declaration order. Repeated concrete instantiations MUST remain distinct even when their expanded
lane types are identical, and calls and returns MUST preserve that selected lane sequence without
truncation, overlap, or substitution.

#### Scenario: Plan two generic affine result fields

- **WHEN** a reachable function returns a nominal struct containing two concrete generic affine owners
- **THEN** its result shape contains every lane of the first field followed by every lane of the second field with distinct canonical field paths

#### Scenario: Preserve repeated lane types

- **WHEN** both affine fields expand to the same scalar lane representations
- **THEN** the result shape retains both complete field subtrees rather than coalescing or aliasing equal lane types

