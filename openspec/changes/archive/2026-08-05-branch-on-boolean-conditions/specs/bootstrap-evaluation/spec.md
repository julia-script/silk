## ADDED Requirements

### Requirement: Conditionals evaluate exactly one arm

The interpreter SHALL execute user-authored branches: a true condition takes the taken block, a
false condition the otherwise path, and exactly one arm's operations execute per traversal.
Boolean values SHALL be exact (`false` is zero, `true` is one) and comparison operations SHALL
produce them without trapping. Interpreter and native execution SHALL agree arm by arm across
the corpus, including programs whose two arms produce different results.

#### Scenario: Take the true arm

- **WHEN** `main` returns `1` under `if I32.equals(1, 1)` and `0` otherwise
- **THEN** evaluation completes with `1` and the trace shows only the taken path's work

#### Scenario: Take the otherwise path

- **WHEN** the condition compares unequal values
- **THEN** evaluation completes with the fall-through result and the taken arm's operations never execute
