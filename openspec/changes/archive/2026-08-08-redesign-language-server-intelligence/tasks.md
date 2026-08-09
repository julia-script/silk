## 1. Acceptance Corpus and Public Shapes

- [x] 1.1 Add shared Silk fixtures for `SystemAllocator.make`, effect-function handlers, piped `Effect.catch`, declaration names, nominal type references, nested bindings, partial identifiers, and recovered member access
- [x] 1.2 Add failing compiler-facade tests that enumerate the required occurrence role, identity, declaration location, and presentation at every identifier token in the acceptance fixtures
- [x] 1.3 Add failing LSP document tests for the distinct hover, expanded definition, completion, and inferred-type hint outcomes specified by the five capability deltas
- [x] 1.4 Define the breaking public data shapes for semantic occurrences, structured presentations, completion results, and type hints, with immutable collections and precise recovery variants

## 2. Intrinsic Catalog

- [x] 2.1 Add the `Intrinsic` actor with stable actor and operation identities, deterministic catalog order, lookup operations, and source-like signature metadata
- [x] 2.2 Register the existing arithmetic, layout, allocator, raw-buffer, slot, unit, and other builtin operations without changing their accepted or rejected programs
- [x] 2.3 Register Effect operations including catch, retry, map, flatMap, tap, provide, and provide-with using the spellings and generic/value parameter forms accepted by the language
- [x] 2.4 Migrate ordinary builtin recognition and elaboration dispatch from independent spelling tables to intrinsic identities, retaining analysis, HIR, ownership, MIR, evaluation, and backend parity
- [x] 2.5 Migrate special Effect-operation recognition and elaboration dispatch from per-operation spelling predicates to intrinsic identities, retaining exact diagnostics and effect-row behavior
- [x] 2.6 Add catalog tests that pair every intrinsic presentation with successful and rejected semantic-analysis examples and prove stable fresh-snapshot ordering

## 3. Source-Like Presentation

- [x] 3.1 Add the `Presentation` actor and structured variants for functions, structs/types, parameters, fields, bindings, imports, intrinsic actors, intrinsic operations, and anonymous expression types
- [x] 3.2 Render function declarations from declaration facts with the declared name, type parameters, named parameters, result/failure/requirement forms, and faithful `fn` versus `effect fn` syntax
- [x] 3.3 Render structs, fields, value parameters, type parameters, immutable and mutable bindings, imports, and intrinsic actors/operations without importing protocol Markdown types
- [x] 3.4 Add scope-aware inferred type rendering that selects an unambiguous local or selected spelling, then a namespace alias, then a canonical module-qualified fallback
- [x] 3.5 Add presentation tests for local, imported, generic, union, reference, effect, and ambiguous inferred types plus the exact `recover` and allocator acceptance signatures

## 4. Semantic Token Provenance

- [x] 4.1 Introduce a resolved reference-path fact that retains exact qualifier, member, and type-argument tokens together with compiler-selected identities and recovery states
- [x] 4.2 Deepen declared-type facts so every nested nominal or type-parameter path exposes the identity and exact tokens required for editor queries
- [x] 4.3 Deepen call, callable-value, struct-construction, field-initializer, and field-projection facts so occurrence collection never infers the semantic token by selecting an identifier from broad syntax
- [x] 4.4 Deepen special Effect expression facts to retain intrinsic actor and operation references plus recursively resolved type-argument facts
- [x] 4.5 Add declaration-index, name-resolution, and elaboration tests proving exact token provenance and explicit unavailable states under recovered qualified and generic syntax

## 5. Semantic Occurrence Index

- [x] 5.1 Add the `SemanticOccurrence` actor with role, identity, resolution, exact token span, optional declaration location, and deterministic source ordinal
- [x] 5.2 Collect top-level function and struct declarations, type parameters, value parameters, fields, local bindings, and pattern-binding declaration sites
- [x] 5.3 Recursively collect every declared type and type argument, including qualified nominal segments and type-parameter references
- [x] 5.4 Collect imports, module qualifiers, ordinary value references, calls, callable values, constructors, field initializers, field projections, and intrinsic actor/operation references
- [x] 5.5 Resolve available occurrence identities to exact module, declaration span, and declaration-name selection span while leaving source-less intrinsics location-free
- [x] 5.6 Build the per-module sorted occurrence arrays and prefix maximum-end lookup index with smallest-span, half-open, and stable-ordinal selection
- [x] 5.7 Add exhaustive occurrence tests for declaration and reference parity, distinct qualified tokens, cross-module locations, ambiguity, missing facts, recovery isolation, Unicode-adjacent bytes, and repeated determinism
- [x] 5.8 Measure occurrence-index memory and lookup cost on representative bootstrap and multi-module fixtures and keep compact identities/spans within the documented acceptance budget

## 6. Analysis Facade Editor Queries

- [x] 6.1 Store the occurrence index in `Analysis.Snapshot` and expose occurrence-at-position and occurrence-range queries through the public facade
- [x] 6.2 Expose lazy occurrence presentation queries that combine occurrence identity, declaration facts, module scope, and the intrinsic catalog
- [x] 6.3 Build and expose a per-module anonymous-expression fallback index for the smallest available typed expression without per-request fact scanning
- [x] 6.4 Add an occurrence-first hover-subject facade query that does not fall back through an unavailable identifier occurrence to a misleading enclosing expression
- [x] 6.5 Export the new editor actors and explicit package subpaths and add public API/type tests for immutable data, precise error-free query channels, and deterministic answers

## 7. Navigation and Hover Migration

- [x] 7.1 Switch `Document.definition` to semantic occurrences and preserve exact synchronized local and cross-file `LocationLink` ranges
- [x] 7.2 Expand definition unit tests to declaration sites, nominal and generic type references, type parameters, imports, qualified actors, fields, bindings, parameters, and source-less intrinsic no-location behavior
- [x] 7.3 Switch `Document.hover` to the facade hover-subject query and map only structured Silk presentation and exact source ranges into protocol Markdown
- [x] 7.4 Delete `Document.typeAt`, `factType`, and the spike's per-request expression/binding scan after literal fallback and all prior hover cases pass
- [x] 7.5 Add document and stdio tests proving distinct hovers for `allocator`, `SystemAllocator`, `make`, `Effect`, `catch`, and `Problem`, plus identical declaration/reference hover for `recover`
- [x] 7.6 Verify hover and definition through exact-version acquisition during rapid edits, non-ASCII positions, open unsaved imports, closed imports, damaged declarations, and shutdown

## 8. Inferred Type Hints

- [x] 8.1 Add the `TypeHint` actor that projects available local binding facts into deduplicated source-ordered name spans and shared inferred type presentations for a byte range
- [x] 8.2 Expose module-range type-hint queries through `Analysis` and test unavailable inference, unrelated damage, range clipping, repeated determinism, and duplicate prevention
- [x] 8.3 Add `Document.inlayHints` protocol conversion with `: ` type labels placed after binding names and no compiler dependency on LSP types
- [x] 8.4 Advertise and handle `textDocument/inlayHint` through exact-version project sessions, returning only hints inside the requested range
- [x] 8.5 Add document and stdio tests for mutable allocator hints, multiple ranges, unavailable bindings, Unicode positions, rapid edits, and capability negotiation

## 9. Completion Context and Scope

- [x] 9.1 Add compact parent-linked lexical scope facts during elaboration, covering parameters, locals, pattern bindings, nested blocks, shadowing, and declaration-before-use boundaries
- [x] 9.2 Add the `Completion` actor with explicit context state, exact replacement span, semantic or syntax candidate identity, kind, insertion spelling, structured detail, and stable sort group
- [x] 9.3 Classify recovered cursor contexts as expression, declared type, type argument, actor/namespace member, typed-value member, or unavailable without requiring a resolved token at the cursor
- [x] 9.4 Compute exact replacement spans for empty positions, partial identifiers, qualified partial members, and recovered generic arguments using source byte offsets
- [x] 9.5 Add scope and context tests for nested shadowing, use before declaration, partial tokens, missing delimiters, incomplete calls, and unrelated damaged declarations

## 10. Completion Candidates and Protocol

- [x] 10.1 Return visible expression candidates for locals, pattern bindings, parameters, callable declarations, constructors, imports, intrinsics, and context-valid expression keywords
- [x] 10.2 Return type-position candidates for built-in and nominal types, type parameters, selected and qualified imports, and context-valid type syntax while excluding value-only candidates
- [x] 10.3 Return actor, intrinsic, and imported-module member candidates after qualified access using the same identities and presentations consumed by hover
- [x] 10.4 Return accessible struct fields after typed values, including references and recovered projections, while preserving unavailable or ambiguous subject state
- [x] 10.5 Deduplicate candidates by semantic identity, apply stable locality/kind/label ordering, and leave prefix filtering and fuzzy ranking to the client
- [x] 10.6 Expose completion through `Analysis` and add deterministic facade tests for every context, visibility boundary, ambiguity state, partial spelling, and recovered-source scenario
- [x] 10.7 Add `Document.completion` conversion to protocol item labels, kinds, insertion text, replacement edits, sort text, and shared signature detail
- [x] 10.8 Advertise completion with `.` triggering and handle `textDocument/completion` through exact-version sessions without advertising before all required contexts pass
- [x] 10.9 Add document and stdio tests for initial typing, parameter types, generic arguments, `Effect.`, `SystemAllocator.`, typed fields, Unicode, rapid edits, and capability negotiation

## 11. Spike Removal, Documentation, and Verification

- [x] 11.1 Remove `SemanticTarget`, `Analysis.semanticTargetAt`, obsolete collector helpers, old exports, and tests superseded by semantic occurrences without compatibility aliases
- [x] 11.2 Update compiler and LSP package documentation to describe semantic occurrences, source-like hover, completion contexts, inferred type hints, and the distinction between inlay hints and code lenses
- [x] 11.3 Run `pnpm typecheck` and fix all public API, strict typing, and package-subpath failures
- [ ] 11.4 Run `pnpm exec biome check .` and repair formatting or lint findings without suppressions, casts for convenience, or non-null assertions
- [x] 11.5 Run `pnpm test` and confirm compiler, LSP document, stdio, recovery, cross-module, Unicode, intrinsic-parity, determinism, and performance coverage passes
- [ ] 11.6 Run `pnpm check` as the repository handoff gate and report any pre-existing failure separately from this change
- [x] 11.7 Run `pnpm release:candidate` because compiler contents, exports, and public analysis APIs change, then record the packed-package and export verification result
