## MODIFIED Requirements

### Requirement: Named functions are first-class callable values

A function with `N` parameters SHALL form a section whenever a call supplies a non-empty trailing
suffix of `K` arguments with `0 < K < N`. The section SHALL await the remaining ordered leading
prefix, and successive direct stages SHALL bind another non-empty trailing suffix without holes,
reordering, or repeated evaluation. Supplying zero or more than the remaining arity SHALL use the
ordinary arity diagnostic; the retired unary-only `SEM0079` diagnostic SHALL NOT be emitted.

#### Scenario: Construct a deeper section

- **WHEN** `combine(a, b, c)` is referenced as `combine(3)`
- **THEN** it produces `fn(A, B) -> C` with parameter `c` captured once

#### Scenario: Apply in stages

- **WHEN** source evaluates `combine(3)(2)(1)`
- **THEN** capture evaluation follows source order and invocation calls `combine(1, 2, 3)`

### Requirement: Sections capture every ownership mode

Each staged argument SHALL be copied, borrowed, or moved exactly once when its stage is constructed,
and its original parameter position SHALL remain explicit independently of capture evaluation order.

#### Scenario: Preserve staged capture positions

- **WHEN** successive stages capture parameters `c` and then `b`
- **THEN** both captures evaluate once in that order while final invocation supplies them as `b, c`
