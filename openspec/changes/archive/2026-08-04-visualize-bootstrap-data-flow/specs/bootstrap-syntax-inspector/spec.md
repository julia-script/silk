## ADDED Requirements

### Requirement: Inspect the first complete value-flow path
The Syntax Inspector SHALL derive a visual data-flow path from existing semantic facts, connecting
each call argument to its mapped target parameter, each resolved parameter reference to that
declaration, and each returned expression to the enclosing function and caller result. Every visual
node and edge SHALL retain an accessible text description and exact syntax identity or span. The
view MUST NOT invent evaluation order, runtime values, or relationships absent from semantic facts.

#### Scenario: Follow a literal through identity
- **WHEN** the canonical program calls `identity(42)` and all references and contracts are compatible
- **THEN** the view connects literal argument `42` to `identity.value`, the returned `value` reference, the `identity` call result, and `main`'s return in one navigable path

#### Scenario: Navigate from a flow item to syntax
- **WHEN** a developer activates a node or edge with source provenance
- **THEN** the inspector identifies and emphasizes the corresponding source span and concrete or semantic detail without changing the analyzed input

#### Scenario: Read the flow without graphics
- **WHEN** the view is consumed through its accessible text structure
- **THEN** the same ordered nodes, relationships, states, and source ranges are available without relying on position or color

### Requirement: Incomplete data flow remains explicit
The data-flow view SHALL represent missing, ambiguous, incompatible, and unavailable relationships
as terminal or branched states rather than drawing a successful path. It SHALL preserve all known
provenance and link each stopped edge to the fact or phase-owned diagnostic that explains it.

#### Scenario: Stop at wrong arity
- **WHEN** a resolved call has an arity-mismatch contract
- **THEN** the view shows any available positional pairs, marks unmatched arguments or parameters, and stops before claiming a valid call result

#### Scenario: Branch at an ambiguous reference
- **WHEN** a parameter or function reference has multiple matches
- **THEN** the view exposes all candidates without selecting a successful edge

#### Scenario: Stop at unavailable syntax or type
- **WHEN** parser recovery or an unresolved type makes a required fact unavailable
- **THEN** the path ends at a labeled unavailable state with the available syntax and diagnostic context retained

### Requirement: Data-flow presets remain disposable
The inspector SHALL provide complete, wrong-arity, unknown-reference, ambiguous-reference, and
syntax-damaged data-flow presets. Flow state, selection, and source emphasis SHALL remain in browser
memory and SHALL reset to the canonical preset on reload.

#### Scenario: Compare complete and incomplete paths
- **WHEN** a developer switches among data-flow presets
- **THEN** the diagram and accessible description recompute locally from each preset's semantic facts

#### Scenario: Reload after selecting a flow node
- **WHEN** the inspector is reloaded after source edits or flow navigation
- **THEN** the canonical source and unselected canonical flow are restored without persisted state
