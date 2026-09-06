## Context

See proposal.md. Baseline 375ac19e includes JUL-120/121/123. Workspace already transports raw profile settings and resolves them through ProjectProfile; its inventory still uses unconditional ModuleSummary headers. Docgen analyzes all bundled roots using an implicit profile. Integrity separately constructs Provider components and filters reachable provider inventory against manifest target lists.

## Goals / Non-Goals

Use one compiler selector and normalized profile for every consumer. Preserve incremental source identity and integrity without retaining the duplicate policy. Keep operation implementations, default service injection, runtime roots and physical supply discovery in their assigned downstream slices.

## Decisions

### Source boundary and hierarchy

Wrap each existing os_* module's implementation declarations and imports in ordinary static conditions inspecting architecture/OS/ABI facts. Keep module documentation outside the group. The selected native surface remains the existing API; unsupported profiles get an empty provider surface. Do not introduce placeholder namespaces. The future hierarchy distinguishes libc/SDK declarations, kernel UAPI declarations, raw operations, shared POSIX policy, service façades and runtime roots; these are source responsibilities, not manifest kinds. Existing os_* modules remain operation-migration targets of their service tickets.

Delete layer/providerTargets and the portable-to-provider import check. Remove Provider graph components and target selection; retain source content, catalog facts, intrinsic inventory and actual reached runtime implementation checks. No backend spelling supplies source availability.

### Selected catalogs

Provide a compiler-owned catalog projection from ProjectAnalysis's selected closure/public resolution. Extract the existing Frontend selection/header phase shared by ProjectAnalysis and discovered-root catalog analysis. Catalog construction checks selected headers without body analysis; never evaluate predicates in editor/generator code. Preserve source spans and original identities for re-exports. Bind catalog identity to the completed profile and selected dependency/content inputs. Header-only syntax summaries remain useful discovery inputs, but cannot assert conditional public availability.

The editor can discover all project module files without importing them into an application's runtime closure. Catalog analysis uses those files as explicit tooling roots under the same configuration; its result is separate from the application's semantic/reachability snapshot. Reuse immutable analyses for unchanged roots/profile. Failed selection cannot publish speculative candidates. Documentation uses explicitly named supported native and Wasm profiles and records availability per profile, not an anonymous union. Doctests run only where their owning declaration is selected.

### Editor transactions

Reuse canonical ProjectProfile selection at the host edge and pass configuration through document/worker messages. Profile configuration changes cause rediscovery and a new accepted generation even with identical source versions. Existing generation/epoch checks must reject old diagnostics and queries; add distinguishing tests rather than inventing a second scheduler. Equivalent normalized profiles can reuse compiler results; differing parameter bindings cannot share catalogs. Derive inactive presentation from compiler inactiveRanges, never reparsed predicates.

### Descriptive catalog records

Add a typed validated record actor with explicit production (hand-authored/generated/mixed), authority identities and versions, logical target/deployment scope, declaration records, claim-specific evidence, fixture/tool versions and review/update provenance. Evidence explicitly distinguishes planned from verified; verified evidence requires result identity. Validate nonempty identities, duplicate declarations/claims, scope containment, deployment bounds and required generation inputs. No physical SDK discovery or availability decision consumes this record. Representative fixtures are record-shape tests, not claims of platform conformance.

### Research and authorities

The governing authority is Native OS Integration Plan updated 2026-09-04T21:17:22.942Z and the merged Silk module static selection/profile reference. This slice introduces no ABI signature and needs no new native supply execution. Pinned prior art: Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa, posix.zig system selection and behavior/import.zig namespace identity; Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84, sys/pal/mod.rs cfg_select and cfg_attr_path.rs inactive nonexistent paths. Zig selects module values lazily; Silk selects declaration groups and preserves ordinary import identity. Rust cfg uses a predicate system, not imported arbitrary static helpers. The cited Rust static-cycle-error.rs is actually a passing self-referential static test, not an availability-cycle oracle; JUL-121's Silk analysis tests own that distinction. The catalog shape records exact versions supplied by each future consuming subset rather than inventing a global header authority.

## Risks / Trade-offs

- Catalog roots cost analysis → share project snapshots and cache completed profile/content results; avoid repeated per-candidate compilation.
- Conditional wrapping affects documentation extraction → project selected declarations must drive extraction and doctest ownership.
- Generation checks previously imply universal APIs → make profile/availability explicit and test deliberate drift.

## Migration Plan

Implement catalog projection, source/integrity removal, profile-aware editor/docgen consumers and provenance validation in one branch. Regenerate embeddings, inventory and docs; migrate affected tests and examples. Run focused analysis/editor/generator tests followed by all repository gates. No compatibility adapters or provider-policy fallback remain.

Documentation projects record the selected profile and publish resolved re-exports as explicit alias items with their source import signatures. Profiles selecting no declarations or publications omit module API examples, so an unavailable provider page cannot execute native examples on WebAssembly.
