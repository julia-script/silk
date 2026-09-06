## Context

See proposal.md. Baseline e0b30a93 contains JUL-120. ModuleClosure currently resolves every root import before Frontend builds headers; Realization.configure later completes package defaults using ProfileBootstrap and Residualization. Selection must move that bootstrap ahead of final closure admission. SyntaxFile remains the lossless, profile-independent parsed file.

## Goals / Non-Goals

**Goals:** one concrete module namespace per profile; ordinary static helper semantics; exact source provenance; no inactive module resolution; immutable snapshots.

**Non-Goals:** module/type values, wildcard publication, local declaration groups, provider catalogs/editor presentation, artifact roots, native requirements or ABI changes.

## Decisions

### Syntax and publication

Use `static if condition { declarations } else { declarations }` at module scope. An arm is a declaration group, not a lexical namespace; nested groups are permitted. `else static if` nests another conditional group. Existing declaration syntax and visibility apply inside each arm. Conditions cannot be prefixed by `pub`.

Use `pub import platform { name, original as local }` for explicit selective publication, extending existing selective imports. Namespace imports remain private; `pub import platform` is rejected. Re-exported identity remains the original declaration identity, with both import and origin provenance. This avoids wrapper functions that change identity or require duplicating generic signatures.

### Demand and availability

Within a module, unconditional declarations/imports are visible to conditions regardless of source order. A reference to a conditional declaration first demands selection of its enclosing conditions, recursively. A selected sibling group's members are therefore available through forward references; an inactive member remains unavailable. A group's own contents never become visible merely to evaluate that group. Track resolving/resolved/failed condition and declaration requests, diagnosing cycles with the requesting condition and dependency origins. Ordinary constant recursion retains the evaluator's cycle diagnostic.

Condition dependency admission is separate from whole-module program admission. Demand a helper's signature, types, constants and actually required static body using ordinary semantic resolution and bounded evaluation. Unrelated runtime declarations and inactive bodies do not enter the condition checking set. Only condition-required imports resolve during this phase. An unconditional import subsequently participates in the selected program closure normally.

### Bootstrap ordering

Normalize immutable initial target/artifact inputs; discover unconditional package schemas from the supplied package graph; bind explicit values; demand and evaluate defaults and validation predicates; publish the final profile; then select module groups. Conditional package schema declarations are invalid, including schemas in modules first reached through selected imports; schema-owning modules must be unconditionally reachable before profile publication. Defaults may use unconditional ordinary helpers and facts admitted by JUL-120; a default requiring conditional availability is a configuration dependency cycle, never a reason to publish or mutate a provisional profile.

Reuse ProfileBootstrap and Residualization rather than introducing a compiler-only predicate interpreter. Extract reusable header/condition checking coordination from the frontend as needed so closure discovery does not invoke the full runtime pipeline. Selected syntax views retain original node identities and spans while the full SyntaxFile remains available for parsing/tooling.

### Semantic identity

Closure modules expose selected declarations and inactive ranges independently of full parsed syntax. All declaration collection, public surface, elaboration, foreign inventory and backend consumers read selected declarations. The active profile and condition-required dependency origins are generic analysis facts.

Parsed syntax can be shared across profiles; selected views, body queries, surfaces and semantic caches require canonical profile identity and selection dependency identity. Changing a demanded helper body invalidates selection even if its public signature is unchanged. Changing an active program dependency follows existing semantic invalidation. Unloaded sources have no cache edge and cannot invalidate this profile. Failed condition dependencies remain observable dependencies so correcting them permits recovery.

### Authorities and deliberate differences

The Native OS Integration Plan revision 2026-09-04T21:17:22.942Z, D-012 and SPEC-01, and the accompanying Silk reference are normative. Local prior-art checkouts match the ticket pins: Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa and Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84. Zig lib/std/posix.zig selects a module-valued `system` and aliases declarations; Rust library/std/src/sys/pal/mod.rs uses cfg_select and re-exports. Silk deliberately has neither module values nor wildcard publication. Rust cfg predicates cannot call arbitrary imported Rust functions; Silk conditions call ordinary static Silk helpers. These are design inputs, not ABI authorities or executed conformance claims. This semantic slice requires no SDK/header/ABI fixtures.

## Risks / Trade-offs

- Availability recursion can accidentally admit an inactive declaration → distinguish dependency requests from selected publication, track cycle origins, test forward and inactive-only references.
- A second evaluator can diverge → reuse bounded StaticEvaluation/Residualization and ordinary type checking.
- Profile-independent body reuse can leak another surface → include profile and condition dependencies before reuse decisions.
- Loaded inactive syntax can be lost by flattening → preserve full syntax separately from selected nodes and test syntax diagnostics in both profiles.

## Migration Plan

Complete the prescriptive reference first, implement syntax and demand/selection, migrate every semantic consumer, add structural/incremental tests and regenerate catalogs. Remove the statement-only exclusion and unconditional indexing assumptions within this change. No compatibility path or deployment migration is retained. Submit as the first gh stack layer, with JUL-123 above it.
