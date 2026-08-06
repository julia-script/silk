## ADDED Requirements

### Requirement: Inspect mutation and structured loops in the unified workbench

The unified `/labs` workbench SHALL inspect mutable-binding and assignment syntax, writable-place
facts, loop regions and lexical transfers, ownership fixed points and cleanup, the canonical control
DAG, evaluation iterations, and target-specific control conversion through facade queries only.
Every graph relationship and state change SHALL have an accessible textual equivalent.

#### Scenario: Follow a continue across representations

- **WHEN** a user selects `continue` inside a loop iteration
- **THEN** coordinated panes identify its loop target, cleanup path, MIR repeat outcome, evaluation event, and emitted target branch

### Requirement: Mutable-loop presets cover valid and invalid states

Browser-local presets SHALL include immutable-write rejection, scalar mutation, field and indexed
assignment, replacement cleanup, zero and multiple iterations, nested loops, conditional `break`,
`continue`, early return, out-of-bounds write, non-`Bool` condition, transfer outside a loop, and
incompatible loop-header ownership without adding a standalone legacy inspector.

#### Scenario: Explore a loop ownership failure

- **WHEN** a preset moves a value on one repeating path without replacement
- **THEN** the workbench retains the condition, path, liveness states, and exact ownership cause without claiming MIR or codegen exists
