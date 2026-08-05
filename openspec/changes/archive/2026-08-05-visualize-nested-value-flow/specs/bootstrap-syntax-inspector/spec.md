## MODIFIED Requirements

### Requirement: Inspect the first complete value-flow path
The Syntax Inspector SHALL derive a visual data-flow projection from existing semantic facts,
connecting each call argument to its mapped target parameter, each resolved parameter reference to
that declaration, each nested call result to its owning outer argument, and each returned expression
to the enclosing function and caller result. After explicit evaluation, the projection SHALL
distinguish static semantic relationships from reachable trace order and exact values. Every group,
node, and edge SHALL retain an accessible text description and exact syntax identity or span. The
view MUST NOT invent evaluation order, runtime values, or relationships absent from semantic facts
or the current evaluation outcome.

#### Scenario: Follow a literal through identity
- **WHEN** the canonical program calls `identity(42)` and all references and contracts are compatible
- **THEN** the view connects literal argument `42` to `identity.value`, the returned `value` reference, the `identity` call result, and `main`'s return in one navigable path

#### Scenario: Follow a nested result into an outer call
- **WHEN** `main` completes `identity(identity(42))`
- **THEN** the view groups both call sites and connects the inner result `42` through the outer argument and parameter to the completed entry result

#### Scenario: Preserve sibling evaluation order
- **WHEN** two nested arguments complete from left to right
- **THEN** their semantic branches remain grouped by argument ordinal and the evaluation overlay identifies the first branch before the second and both before the enclosing bindings

#### Scenario: Navigate from a flow item to syntax
- **WHEN** a developer activates a nested group, node, or edge with source provenance
- **THEN** the inspector identifies and emphasizes the corresponding source span and concrete, semantic, or trace detail without changing the analyzed input

#### Scenario: Read the flow without graphics
- **WHEN** the view is consumed through its accessible text structure
- **THEN** the same nested groups, ordered nodes, relationships, states, values, and source ranges are available without relying on position or color

### Requirement: Incomplete data flow remains explicit
The data-flow view SHALL represent missing, ambiguous, incompatible, unavailable, blocked, and cyclic
relationships as terminal or branched states rather than drawing a successful enclosing path. It
SHALL preserve all known nested provenance and link each stopped edge to the semantic fact,
evaluation reason, trace prefix, or phase-owned diagnostic that explains it. A completed earlier
argument branch SHALL remain visible when a later sibling blocks, but bindings or returns that did
not occur MUST NOT be drawn as evaluated flow.

#### Scenario: Stop at wrong arity
- **WHEN** a resolved call at any nesting depth has an arity-mismatch contract
- **THEN** the view shows any available positional pairs, marks unmatched arguments or parameters, and stops before claiming a valid result from that call or its enclosing call

#### Scenario: Branch at an ambiguous reference
- **WHEN** a parameter or function reference has multiple matches
- **THEN** the view exposes all candidates without selecting a successful edge

#### Scenario: Stop at unavailable syntax or type
- **WHEN** parser recovery or an unresolved type makes a required nested fact unavailable
- **THEN** the affected branch ends at a labeled unavailable state with the available syntax and diagnostic context retained

#### Scenario: Stop at a nested evaluation failure
- **WHEN** an inner argument blocks after an earlier argument completed
- **THEN** the earlier completed branch and partial trace remain visible while the inner reason terminates the enclosing evaluated path before its bindings

#### Scenario: Show a recursive cycle as a closed terminal path
- **WHEN** nested evaluation reports a recursive call cycle
- **THEN** the view lists the ordered declaration cycle and closing call-site span without drawing an infinite or successful path

### Requirement: Data-flow presets remain disposable
The inspector SHALL provide complete flat, complete nested, nested sibling, wrong-arity,
unknown-reference, ambiguous-reference, syntax-damaged, inner-blocked, and nested-cycle data-flow
presets. Flow mode, evaluation overlay, selection, and source emphasis SHALL remain in browser memory
and SHALL reset to the canonical preset on reload.

#### Scenario: Compare complete and incomplete paths
- **WHEN** a developer switches among flat, nested, and blocked data-flow presets
- **THEN** the projection and accessible description recompute locally from each preset's current semantic facts and optional evaluation outcome

#### Scenario: Compare static and evaluated flow
- **WHEN** a developer analyzes and then explicitly evaluates a nested preset
- **THEN** the same semantic relationships remain visible while reachable order, exact values, and any blocked endpoint are added from that outcome

#### Scenario: Reload after selecting a flow node
- **WHEN** the inspector is reloaded after source edits, evaluation, or flow navigation
- **THEN** the canonical source and unselected canonical flow are restored without persisted state
