# bootstrap-analysis-facade Specification

## Purpose
The supported, snapshot-backed query surface over compiler analysis: one immutable snapshot per
compilation request answering queries for sources, syntax, imports, declarations, references,
types, contracts, HIR facts, evaluation, and merged diagnostics — the exclusive consumer surface
for tooling, so tools can grow without reimplementing Silk semantics.
## Requirements
### Requirement: One snapshot answers supported queries

The facade SHALL build one immutable analysis snapshot from a compilation request (with a
single-source convenience for one module) and SHALL answer queries over sources, syntax
artifacts, import facts and cycles, collected declarations and lookups, elaborated function facts
with their types, references, and contracts, HIR facts, and ownership facts with their cleanup
plans. Query results SHALL be immutable values, and repeated snapshots of identical input SHALL
answer every query identically.

#### Scenario: Query a multi-module snapshot

- **WHEN** a snapshot is built from a request whose root imports another module
- **THEN** the facade lists both modules, answers each module's syntax artifact and declarations, and resolves declaration lookups per module

#### Scenario: Repeat snapshot construction

- **WHEN** the same request is snapshotted repeatedly in fresh processes
- **THEN** every supported query answers identically

#### Scenario: Query ownership facts

- **WHEN** a snapshot's module contains checked functions
- **THEN** the facade answers the module's ownership facts and cleanup plans as immutable values

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

### Requirement: Discovery and lowered MIR are facade queries

The facade SHALL answer the snapshot's instance discovery (entry state and ordered instances)
and its lowered MIR program as immutable values, alongside the existing queries.

#### Scenario: Query discovery and lowered MIR

- **WHEN** a snapshot's root module has a valid entry
- **THEN** the facade answers the ordered instances and a lowered MIR program containing one function per instance

#### Scenario: Answer an unavailable entry

- **WHEN** the root module has no valid entry
- **THEN** the facade answers the explicit unavailable entry state and an empty lowered program

### Requirement: Codegen is a facade query

The facade SHALL answer a snapshot's backend emission — bitcode, IR text, and symbols for a
given codegen request — through the nominal backend service, so tooling never invokes the
backend directly.

#### Scenario: Emit through the facade

- **WHEN** a tool requests a snapshot's release emission
- **THEN** the facade answers the artifact with its bitcode bytes, IR text, and symbol table

### Requirement: Target and layout are facade queries

The facade SHALL expose the snapshot's canonical target selection and immutable completed layout
plan as supported queries. The lowered MIR, interpreter, and codegen queries SHALL consume that
same plan by value; no facade query or tooling consumer may construct a replacement target layout.
An unsupported target SHALL remain an explicit queryable outcome and SHALL make lowering,
evaluation, and codegen unavailable without inventing fallback facts.

#### Scenario: Query one shared layout plan

- **WHEN** a supported snapshot discovers instances using `I32` and `Bool`
- **THEN** its target query, layout query, lowered MIR query, evaluation query, and codegen query all identify the same canonical target and scalar layout entries

#### Scenario: Query an unsupported target

- **WHEN** a snapshot request selects an unsupported target
- **THEN** the facade exposes the target failure and marks layout, MIR, evaluation, and codegen unavailable without invoking a backend

#### Scenario: Emit WebAssembly from a WebAssembly snapshot

- **WHEN** a snapshot selects `wasm32-unknown-unknown` and codegen uses the compatible direct WebAssembly backend
- **THEN** codegen consumes the snapshot's existing MIR layout plan without replacing its target or scalar entries

### Requirement: Name resolution is a facade query

The facade SHALL expose each module's immutable scope, import-binding outcomes, collisions, and
unqualified or qualified declaration lookup facts from the same snapshot used for HIR, ownership,
instance discovery, MIR, evaluation, and codegen. Tooling consumers MUST NOT reconstruct module
scopes from syntax or declaration headers. Damaged imports and failed lookups SHALL retain their
explicit unavailable state and originating diagnostic cause.

#### Scenario: Query a hybrid import scope

- **WHEN** a snapshot module imports `compiler.Syntax as Tree { parse }`
- **THEN** the facade reports namespace binding `Tree`, selected binding `parse`, and their canonical target module and declaration identities

#### Scenario: Query a cross-module call reference

- **WHEN** a body calls a public function through a valid namespace alias
- **THEN** the facade's lookup fact and HIR query identify the same canonical imported declaration

#### Scenario: Query a binding collision

- **WHEN** a local declaration and selected import claim the same module-scope spelling
- **THEN** the facade exposes every conflicting binding, the unavailable lookup, and its diagnostic cause without choosing a winner

#### Scenario: Query around a damaged import

- **WHEN** one import clause contains recovered syntax
- **THEN** its unavailable binding facts remain queryable while unrelated module scopes, HIR, and declarations answer completely

### Requirement: Struct declarations and layouts are facade queries

The facade SHALL expose every module's nominal struct headers, ordered field facts, type lookups,
visibility and dependency states, the snapshot's complete nominal layout catalog, and its
reachable runtime layout plan as immutable queries. Struct facts and layout entries SHALL reuse the
same canonical nominal identity; tooling MUST NOT reconstruct fields, dependencies, recursion,
padding, or offsets from syntax.

#### Scenario: Query a complete nominal struct

- **WHEN** a snapshot contains an available struct with scalar fields
- **THEN** the facade returns its canonical header, ordered resolved fields, and selected-target catalog entry under one nominal identity

#### Scenario: Query a cross-module field dependency

- **WHEN** one struct contains a public nominal type imported from another module
- **THEN** the field lookup and both layout entries expose the same canonical imported type identity

#### Scenario: Query an unavailable recursive layout

- **WHEN** structs form an inline recursive dependency cycle
- **THEN** the facade retains their headers and fields while exposing unavailable layout states and the canonical diagnostic cause


### Requirement: Struct-value facts are facade queries

The facade SHALL expose struct literal target and field mappings, projection chains, typed aggregate
HIR, whole-value ownership and cleanup, aggregate runtime reachability, catalog and calling-shape
facts, aggregate MIR, evaluation traces, and codegen outcomes from one immutable snapshot. Every
query SHALL reuse the same canonical nominal and field identities; tooling MUST NOT reconstruct
construction completeness, projection lookup, ownership, lane order, or backend realization.

#### Scenario: Query one complete struct value path

- **WHEN** a snapshot constructs a struct through a factory and projects a nested scalar
- **THEN** facade queries link syntax, semantic mappings, HIR, ownership, layout, MIR, evaluation, and emission through the same canonical nominal and field identities

#### Scenario: Query an unavailable construction

- **WHEN** a literal is externally unauthorized, incomplete, duplicated, or mistyped
- **THEN** the facade retains every supplied field fact and exact cause while aggregate HIR, MIR, evaluation, and codegen remain explicitly unavailable

### Requirement: Aggregate facade answers remain immutable and deterministic

Facade results containing aggregate values or paths SHALL use immutable canonical data rather than
JavaScript object identity or mutable maps. Repeated snapshots of identical inputs SHALL answer
byte-identical encodings and identically ordered aggregate facts.

#### Scenario: Repeat aggregate queries

- **WHEN** identical nested aggregate sources and targets are snapshotted in fresh processes
- **THEN** every construction, projection, ownership, layout, MIR, and trace query answers identically
