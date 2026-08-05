## Purpose

The supported, snapshot-backed query surface over compiler analysis: one immutable snapshot per
compilation request answering queries for sources, syntax, imports, declarations, references,
types, contracts, HIR facts, evaluation, and merged diagnostics — the exclusive consumer surface
for tooling, so tools can grow without reimplementing Silk semantics.

## ADDED Requirements

### Requirement: One snapshot answers supported queries

The facade SHALL build one immutable analysis snapshot from a compilation request (with a
single-source convenience for one module) and SHALL answer queries over sources, syntax
artifacts, import facts and cycles, collected declarations and lookups, elaborated function facts
with their types, references, and contracts, and HIR facts. Query results SHALL be immutable
values, and repeated snapshots of identical input SHALL answer every query identically.

#### Scenario: Query a multi-module snapshot

- **WHEN** a snapshot is built from a request whose root imports another module
- **THEN** the facade lists both modules, answers each module's syntax artifact and declarations, and resolves declaration lookups per module

#### Scenario: Repeat snapshot construction

- **WHEN** the same request is snapshotted repeatedly in fresh processes
- **THEN** every supported query answers identically

### Requirement: The facade merges the compilation's diagnostics

The facade SHALL expose the compilation's complete diagnostic sequence, merged across every
module and phase by the single deterministic ordering authority. No consumer SHALL need to
collect or order per-phase diagnostic collections itself.

#### Scenario: Merge diagnostics across modules and phases

- **WHEN** two modules produce lexical, parser, module, and semantic diagnostics
- **THEN** the facade answers one deterministic driver-ordered sequence containing all of them

### Requirement: Recovery states remain queryable through the facade

Facade results SHALL carry the same explicit unavailable, missing, ambiguous, and damaged states
as the underlying fact tables, and a damaged module SHALL leave every unrelated declaration fully
queryable.

#### Scenario: Query around damage

- **WHEN** one module of a snapshot contains recovered syntax and semantic mistakes
- **THEN** the other module's declarations, functions, and HIR answer completely and the damaged module's facts expose their explicit recovery states

### Requirement: The facade is the only supported consumer surface

Tooling SHALL consume compiler phases exclusively through the facade; the immutable data-model
vocabularies (syntax elements, diagnostics, fact types) are part of the facade's answers and
remain importable as types. The compiler package SHALL document this rule, and evaluation SHALL
be reachable as a facade query rather than by invoking the evaluator phase directly.

#### Scenario: Evaluate through the facade

- **WHEN** a tool evaluates a snapshot's root module through the facade
- **THEN** it receives the closed evaluation outcome without invoking the evaluator phase itself

#### Scenario: Keep phase modules out of tooling imports

- **WHEN** the docs labs are checked for compiler imports
- **THEN** no lab or flow model value-imports a phase module (lexer, parser, closure loading, header collection, elaboration, evaluation); only facade queries and data-model types appear
