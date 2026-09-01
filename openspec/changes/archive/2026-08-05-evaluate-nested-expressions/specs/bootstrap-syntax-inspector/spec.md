## ADDED Requirements

### Requirement: Inspect recursive evaluation outcomes

The Syntax Inspector SHALL provide completed, inner-blocked, and nested-cycle evaluation presets
and SHALL render each nested trace event beside the semantic expression and source provenance that
produced it. Successful inner results SHALL be visibly connected to their enclosing positional
bindings, while a blocked inner path SHALL end before any enclosing binding or return that did not
occur. The trace SHALL remain available as an ordered accessible text structure and MUST NOT rely
on indentation, position, or color alone to communicate nesting.

#### Scenario: Inspect a completed nested evaluation

- **WHEN** a developer evaluates the `identity(identity(42))` preset
- **THEN** the inspector displays result `42` and distinguishes the inner call, inner return, outer binding, outer return, and their two call-site spans in trace order

#### Scenario: Inspect an inner blocked outcome

- **WHEN** a nested argument blocks because its target, contract, value, or cycle is unavailable
- **THEN** the inspector shows the exact inner reason and partial trace without displaying an enclosing binding or completed result

#### Scenario: Read nested trace order without graphics

- **WHEN** the nested trace is consumed through its accessible text representation
- **THEN** call depth, event order, values, identities, states, and source ranges communicate the same outcome as the visual trace
