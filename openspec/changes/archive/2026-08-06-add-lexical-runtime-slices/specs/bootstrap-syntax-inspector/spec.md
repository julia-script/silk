## ADDED Requirements

### Requirement: Inspect lexical slices in the unified workbench

The existing `/labs` workbench SHALL expose the canonical multi-length shared-slice program and the
exclusive write-through program without adding a standalone inspector. Coordinated panes SHALL show
slice syntax, semantic source roots, loan identities and access, HIR, instance reuse, target layout
and calling lanes, structured MIR, evaluation, and native and Wasm realization from the same
analysis snapshot.

#### Scenario: Follow one shared slice across representations

- **WHEN** a developer selects the runtime-slice coverage preset and activates `fold(&values)`
- **THEN** the workbench relates its source span to the shared loan, single fold instance, logical slice MIR, target address-and-length layout, and all three execution results

#### Scenario: Inspect exclusive write-through accessibly

- **WHEN** a developer selects the exclusive-slice preset using keyboard or accessible text navigation
- **THEN** the same source root, exclusive loan, checked element replacement, caller-visible value, and backend storage realization are available without relying on color or graphics

### Requirement: Invalid slice state remains inspectable

Unavailable, conflicting, escaping, or malformed slice facts SHALL remain visible in the existing
syntax, semantic, ownership, and flow views with their phase-owned diagnostics and exact source
provenance. The inspector MUST NOT draw an available loan, MIR operation, or backend path after the
phase where validity stops.

#### Scenario: Inspect a conflicting exclusive borrow

- **WHEN** one call attempts two exclusive borrows of the same array root
- **THEN** the workbench shows both source borrow intents, their ownership conflict, and the stopped downstream path without claiming successful execution
