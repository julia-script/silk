## ADDED Requirements

### Requirement: Unified Labs explains callable composition

The unified `/labs` workbench SHALL provide coordinated presets for named function values,
automatic leading-argument sections, stored callable bindings, `fn`, `mut fn`, and `once fn`
contracts, Copy and affine captures, ordinary higher-order functions, Effect combinators, pipeline
application, and grouped versus ungrouped `run`. Syntax, semantic facts, ownership, HIR, instances,
MIR, evaluation, native, and Wasm panes SHALL use the same selected source and expose unavailable
downstream paths without fabrication.

#### Scenario: Inspect a mapped arithmetic section

- **WHEN** a developer opens `succeed(2) |> Effect.map(I32.add(2))`
- **THEN** linked panes distinguish section construction, callable capture, Effect composition, run, result `4`, and backend realization

#### Scenario: Inspect a consumed callable

- **WHEN** a preset invokes an owned-capture section twice
- **THEN** ownership and downstream panes identify the consumed environment slot and stop at the responsible phase

#### Scenario: Compare run grouping

- **WHEN** the source switches between `run effect |> Effect.map(f)` and `(run effect) |> f`
- **THEN** every pane shows whether composition occurs before execution or transformation occurs after execution
