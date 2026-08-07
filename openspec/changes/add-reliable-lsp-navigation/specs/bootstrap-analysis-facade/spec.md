## ADDED Requirements

### Requirement: Position-oriented semantic targets are facade queries

The analysis facade SHALL answer the smallest reference-bearing semantic target containing a byte
offset in a requested module and SHALL map every available resolved target identity to its exact
declaration source span. The answer SHALL preserve explicit missing, inaccessible, ambiguous,
conflicting, and unavailable states from compiler analysis. Tooling MUST NOT recreate lexical
scope, name lookup, member lookup, callable resolution, or declaration selection from source
spelling or syntax.

#### Scenario: Query a local reference target

- **WHEN** a consumer queries a byte offset inside a resolved local or parameter reference
- **THEN** the facade returns that reference's semantic identity and exact declaration-name span

#### Scenario: Query an imported declaration target

- **WHEN** a consumer queries a byte offset inside a resolved qualified or selected import reference
- **THEN** the facade returns the canonical imported declaration identity and its source module and declaration-name span

#### Scenario: Query a struct field target

- **WHEN** a consumer queries a byte offset inside an available field projection
- **THEN** the facade returns the resolved field identity and field declaration-name span without reconstructing the nominal type from syntax

#### Scenario: Query an unavailable target

- **WHEN** a reference is missing, inaccessible, ambiguous, conflicting, or unavailable because of recovered analysis
- **THEN** the facade returns the corresponding explicit state without inventing a declaration location

#### Scenario: Query outside a reference

- **WHEN** a consumer queries trivia, punctuation, or a source offset outside all reference-bearing facts
- **THEN** the facade returns no semantic target

### Requirement: Position query answers are immutable and deterministic

Position-oriented semantic targets and declaration locations SHALL be immutable snapshot values.
Repeated queries of the same snapshot and byte offset SHALL return equal answers, and snapshots of
identical inputs SHALL select the same target through smallest-span and deterministic source-order
tie breaking.

#### Scenario: Repeat a nested target query

- **WHEN** identical source produces nested semantic facts containing one queried byte offset
- **THEN** repeated fresh snapshots select the same smallest reference-bearing target and declaration location

#### Scenario: Query a half-open boundary

- **WHEN** a byte offset equals the exclusive end of one reference span
- **THEN** that reference is not selected unless another containing reference-bearing fact applies

### Requirement: Editor queries preserve recovery isolation

A damaged or unavailable fact SHALL NOT prevent position queries from answering unrelated available
targets in the same module or another module. The facade SHALL derive editor queries from the same
recovered semantic facts and diagnostic causes exposed by its existing query surface.

#### Scenario: Query beside recovered syntax

- **WHEN** one declaration contains recovered syntax and another declaration contains an available resolved reference
- **THEN** the available reference still resolves to its exact declaration location

#### Scenario: Query another module after damage

- **WHEN** one module is damaged while an imported module remains analyzable
- **THEN** position and declaration-location queries for the analyzable module remain complete
