## ADDED Requirements

### Requirement: Editor presentations are semantic facade data

The analysis facade SHALL expose source-like presentations for available semantic occurrences,
including declaration kind, name, generic parameters, named value parameters, source-visible type
forms, mutability, and function kind where applicable. Intrinsic actors and operations SHALL use the
same authoritative definitions as semantic analysis. Tooling MUST NOT reconstruct declaration
signatures from syntax, canonical type encodings, or hard-coded intrinsic spelling.

#### Scenario: Present an effect function

- **WHEN** a consumer requests the presentation of an effect-function declaration or reference
- **THEN** the facade returns its named `effect fn` signature with declared parameter names and result type

#### Scenario: Present an intrinsic operation

- **WHEN** a consumer requests the presentation of `Effect.catch`
- **THEN** the facade returns the intrinsic's authoritative generic operation signature without a fabricated source location

### Requirement: Completion candidates are facade queries

The analysis facade SHALL answer completion candidates for a module and byte offset from recovered
syntax context, visible lexical and module scope, available subject types, visibility, imports, and
intrinsic definitions. Each candidate SHALL carry a stable semantic kind, insertion spelling, and
source-like detail. Missing or ambiguous context SHALL remain explicit, and tooling MUST NOT
reconstruct candidate scope or member sets independently.

#### Scenario: Query expression completions

- **WHEN** a consumer queries an expression position inside a nested lexical scope
- **THEN** the facade returns the compiler-visible bindings, parameters, declarations, imports, intrinsics, and applicable syntax candidates with shadowing already resolved

#### Scenario: Query qualified completions

- **WHEN** a consumer queries immediately after an available actor, namespace, or typed subject followed by `.`
- **THEN** the facade returns only the accessible operations, declarations, or fields belonging to that qualifier

#### Scenario: Query recovered completion context

- **WHEN** the source at the completion position contains a partial identifier or incomplete type application
- **THEN** the facade returns available candidates from the recovered context without requiring a resolved occurrence at the cursor

### Requirement: Inferred binding hints are facade queries

The analysis facade SHALL answer immutable inferred-type hint data for local bindings in a requested
module range. Each available hint SHALL identify the exact binding-name span and source-like type
presentation; unavailable inference SHALL remain absent rather than speculative.

#### Scenario: Query a local binding hint

- **WHEN** a consumer requests hint data for an inferred local binding
- **THEN** the facade returns the binding-name span and its available source-like inferred type

## MODIFIED Requirements

### Requirement: Position-oriented semantic targets are facade queries

The analysis facade SHALL answer the smallest token-level semantic occurrence containing a byte
offset in a requested module. Occurrences SHALL cover declaration sites and resolved value, type,
field, actor, operation, parameter, binding, import, and qualified-name roles represented by the
snapshot. Every available source-backed identity SHALL map to its exact declaration span, while
intrinsic identities may remain available without a source location. Answers SHALL preserve
explicit missing, inaccessible, ambiguous, conflicting, and unavailable states from compiler
analysis. Tooling MUST NOT recreate lexical scope, type lookup, name lookup, member lookup,
callable resolution, intrinsic recognition, or declaration selection from source spelling or syntax.

#### Scenario: Query a local reference occurrence

- **WHEN** a consumer queries a byte offset inside a resolved local or parameter reference
- **THEN** the facade returns that token's semantic role, identity, and exact declaration-name span

#### Scenario: Query an imported declaration occurrence

- **WHEN** a consumer queries a byte offset inside a resolved qualified or selected import reference
- **THEN** the facade returns the canonical imported declaration identity and its source module and declaration-name span

#### Scenario: Query a declared type occurrence

- **WHEN** a consumer queries a byte offset inside a resolved parameter, result, field, or generic-argument type
- **THEN** the facade returns the nominal or type-parameter identity and its declaration location

#### Scenario: Query a declaration-site occurrence

- **WHEN** a consumer queries the declared name of a function, type, field, parameter, or binding
- **THEN** the facade returns a declaration occurrence carrying that entity's identity and location

#### Scenario: Query an intrinsic occurrence

- **WHEN** a consumer queries the actor or operation token of a recognized source-less intrinsic
- **THEN** the facade returns its intrinsic identity and presentation with no source declaration location

#### Scenario: Query an unavailable occurrence

- **WHEN** an occurrence is missing, inaccessible, ambiguous, conflicting, or unavailable because of recovered analysis
- **THEN** the facade returns the corresponding explicit state without inventing an identity or declaration location

#### Scenario: Query outside a semantic occurrence

- **WHEN** a consumer queries trivia, punctuation, or a source offset outside all token-level semantic facts
- **THEN** the facade returns no semantic occurrence

### Requirement: Position query answers are immutable and deterministic

Position-oriented semantic occurrences, identities, presentations, and declaration locations SHALL
be immutable snapshot values. Repeated queries of the same snapshot and byte offset SHALL return
equal answers, and snapshots of identical inputs SHALL select the same occurrence through
smallest-span and deterministic source-order tie breaking.

#### Scenario: Repeat a nested occurrence query

- **WHEN** identical source produces nested semantic facts containing one queried byte offset
- **THEN** repeated fresh snapshots select the same smallest token-level occurrence and presentation

#### Scenario: Query a half-open boundary

- **WHEN** a byte offset equals the exclusive end of one semantic token span
- **THEN** that occurrence is not selected unless another containing semantic occurrence applies

### Requirement: Editor queries preserve recovery isolation

A damaged or unavailable fact SHALL NOT prevent occurrence, presentation, completion, or inferred
hint queries from answering unrelated available facts in the same module or another module. The
facade SHALL derive every editor query from the same recovered semantic facts and diagnostic causes
exposed by its existing query surface.

#### Scenario: Query beside recovered syntax

- **WHEN** one declaration contains recovered syntax and another declaration contains an available resolved occurrence
- **THEN** the available occurrence still answers with its identity, presentation, and exact declaration location

#### Scenario: Complete beside recovered syntax

- **WHEN** one source region is damaged while the completion position retains an available recovered scope
- **THEN** completion returns the candidates from that available scope without candidates guessed from the damaged fact

#### Scenario: Query another module after damage

- **WHEN** one module is damaged while an imported module remains analyzable
- **THEN** occurrence, presentation, completion, and hint queries for the analyzable module remain complete
