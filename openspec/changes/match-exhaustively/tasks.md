## 1. Match source surface

- [x] 1.1 Add `MatchKeyword`, ampersand, fat-arrow, and dot-dot token kinds with deterministic longest-match lexing, keyword-boundary behavior, exact spans, and lexer coverage tests.
- [x] 1.2 Add lossless syntax actors for match expressions, access modes, arms, nominal and universal patterns, ordered field patterns, renamed bindings, nested patterns, and explicit rest markers.
- [x] 1.3 Parse match as a primary expression with one scrutinee, optional `move`/`&`/`&mut` mode, braced source-ordered arms, optional guards, and expression results in every expression position.
- [x] 1.4 Add arm-local parser recovery for missing pattern names, fields, colons, commas, guards, fat arrows, and braces without consuming later arms or enclosing declarations.
- [x] 1.5 Add focused lexer, lossless-tree, expression-position, nested-pattern, trivia, and damaged-arm parser tests.
- [x] 1.6 Format complete match expressions as canonical indented arm blocks and format nested field patterns, guards, comments, and recovered syntax idempotently.
- [x] 1.7 Extend CodeMirror and TextMate token classification, regenerate the VS Code grammar, and add keyword-parity and highlighting tests for the complete match surface.

## 2. Pattern and coverage semantics

- [x] 2.1 Add stable match, arm, pattern, and pattern-binding identities plus immutable source-ordered facts for scrutinee type, access mode, guards, results, and exact provenance.
- [x] 2.2 Resolve nominal patterns against precise nominal scrutinees and normalized structural-union member sets while retaining both source spelling and canonical member identity.
- [x] 2.3 Resolve shorthand, renamed, nested, and rest field patterns to canonical field paths; diagnose unknown, duplicate, and omitted-without-rest fields while retaining independent facts.
- [x] 2.4 Introduce flat arm-local pattern-binding scopes with precise narrowed types and deterministic conflicts against parameters, body bindings, and earlier pattern bindings.
- [x] 2.5 Implement the source-ordered canonical remaining-member fold, including non-subtracting guards, terminal universal coverage, duplicate-member rejection, unreachable arms, and exact incomplete-member diagnostics.
- [x] 2.6 Type-check guards as `Bool`, make candidate bindings visible during their own guard, and exclude damaged or unreachable arms from executable facts without discarding their source facts.
- [x] 2.7 Publish arm-local nominal narrowing without changing the scrutinee binding outside an arm or exposing physical member tags.
- [x] 2.8 Add semantic tests for nominal one-member coverage, multi-member exhaustiveness, guarded fallthrough, universal coverage, invalid members and fields, pattern scoping, and deterministic diagnostics.

## 3. Match result typing and HIR

- [x] 3.1 Implement one canonical result-join operation that ignores unreachable and `Never` arms, preserves equal types, normalizes nominal/union results, and rejects incompatible scalar or aggregate mixtures.
- [x] 3.2 Add `MatchArm` widening provenance and insert explicit HIR `UnionConvert` nodes where an arm result widens to the joined structural union.
- [x] 3.3 Add HIR actors for match access, canonical pattern paths, ordered arm regions, optional guard regions, narrowed payload bindings, cleanup boundaries, and one typed join result.
- [x] 3.4 Extend HIR elaboration, traversal, verification, equality, and deterministic text encoding for ancestor-only acyclic match-region relationships.
- [x] 3.5 Add HIR tests for nested expression matches, guarded same-member arms, precise and union result joins, damaged matches, deterministic encodings, and rejection of cyclic or backend-owned data.

## 4. Match-local ownership

- [x] 4.1 Represent pattern access separately from semantic value types and classify bare matching as a Copy read, `move` as a whole-value transfer, `&` as a shared local view, and `&mut` as an exclusive local view of a mutable live place.
- [x] 4.2 Make guard bindings provisional and non-consuming so a false guard performs no move, cleanup, mutation, or borrow completion before later arms are considered.
- [x] 4.3 Commit consuming destructuring only on the selected path, transfer bound non-Copy fields, and plan omitted active-field cleanup exactly once in canonical order.
- [x] 4.4 Enforce shared-view read-only behavior, exclusive-view mutation rules, selected-arm lexical endings, and continued owner availability after borrowed matches.
- [x] 4.5 Reject borrowed pattern bindings that move, escape as a result, enter owned storage, cross a call contract, or survive through closure capture or another arm.
- [x] 4.6 Add ownership tests for Copy reuse, whole-value consumption, guard fallthrough, nested field moves, omitted-field cleanup, early return and trap cleanup, mutable-place requirements, and borrow escape.

## 5. Instance discovery and structured MIR

- [x] 5.1 Extend runtime instance discovery through executable member patterns, nested field types, guards, joined results, conversions, and cleanup while excluding unreachable arms and preserving canonical worklist order.
- [x] 5.2 Add MIR actors for structured match regions, canonical member decisions, candidate bindings, guard regions, selection-time ownership operations, per-arm results and cleanup, and one join destination local.
- [x] 5.3 Lower each HIR match scrutinee exactly once and lower nested matches to nested acyclic regions whose joined result is an ordinary expression local.
- [x] 5.4 Lower guarded same-member decisions in source order without duplicating payloads, and emit moves, views, borrow endings, and cleanup only on their valid selected paths.
- [x] 5.5 Reuse the existing compiler-owned union sum shape, member ordinals, payload offsets, and calling shapes without adding a public tag ABI or artificial tagged allocation for precise nominal values.
- [x] 5.6 Verify scrutinee and result layouts, exhaustive canonical cases, decision order, pattern paths and binding types, Boolean guards, ownership metadata, cleanup, arm joins, arm-local lifetimes, and region acyclicity before execution or emission.
- [x] 5.7 Extend MIR traversal and deterministic encoding with topological region order, source decision order, canonical member and field identities, join relationships, cleanup, and provenance.
- [x] 5.8 Add instance and MIR tests for nested patterns, unreachable-type omission, guarded fallthrough, exact cleanup, hand-built invalid regions, nested expression matches, and fresh-process encoding stability.

## 6. Logical evaluation

- [x] 6.1 Evaluate the scrutinee once and dispatch by logical active nominal member, never by inspecting physical storage or reconstructing backend tags.
- [x] 6.2 Evaluate matching guards in source order, create verified pattern bindings, select exactly one arm, and preserve the payload unchanged across rejected guards.
- [x] 6.3 Execute and trace ownership transfer, match-local borrow ending, active omitted-field cleanup, arm result conversion, and the joined result with exact provenance.
- [x] 6.4 Add evaluator tests for nominal and union matches, universal fallback, guarded fallthrough, Copy/move/shared/exclusive access, traps, nested matches, cleanup, and deterministic traces.

## 7. Native and WebAssembly realization

- [x] 7.1 Lower verified match regions in the native LLVM backend using target-private blocks, comparisons, or switches over the compiler-owned layout while preserving source decision and cleanup semantics.
- [x] 7.2 Lower verified match regions in the direct WebAssembly backend using target-private structured blocks, `if`, or `br_table` without feeding branch depths or reconstructed control edges back into MIR.
- [x] 7.3 Implement payload projection, provisional guard bindings, selected-path ownership operations, arm result widening, and one join value consistently in both backends.
- [x] 7.4 Reject invalid match or cleanup metadata before partial artifact construction and retain stable canonical symbols and source provenance.
- [x] 7.5 Add native and WebAssembly tests for guarded multi-member dispatch, nominal fast paths, nested matches, branch cleanup, deterministic IR/WAT/binaries, and parity with logical evaluation.

## 8. Analysis facade and unified labs

- [x] 8.1 Extend immutable analysis snapshots and facade queries with match syntax identities, access, canonical coverage transitions, patterns, bindings, guards, narrowing, result joins, ownership, cleanup, and cross-phase provenance.
- [x] 8.2 Add facade tests proving stable ordered match answers across repeated and fresh snapshots without tooling-side coverage, payload, or tag reconstruction.
- [x] 8.3 Add match rows, details, and coordinated source/semantic/HIR/MIR/evaluation/layout/backend selections to the existing unified `/labs` panes with accessible textual equivalents.
- [x] 8.4 Add browser-local presets for valid access modes, nested and renamed patterns, guards, universal coverage, joins, loops and aggregates, plus incomplete, unreachable, typing, binding, borrow, and cleanup failures.
- [x] 8.5 Add workbench tests for coordinated selection, unavailable downstream phases after errors, accessibility, stale-analysis handling, and deterministic preset output without creating a standalone inspector.

## 9. Differential corpus and release gates

- [x] 9.1 Add valid three-engine corpus programs covering precise and union scrutinees, all access modes, nested destructuring, guarded fallthrough, universal arms, result joins, loops, aggregates, mutation, traps, and cleanup.
- [x] 9.2 Add invalid phase-owned corpus programs for incomplete or unreachable coverage, unknown members and fields, malformed patterns, non-Boolean guards, incompatible results, illegal moves, borrow escape, exclusive access, and cleanup verification.
- [x] 9.3 Add fresh-process determinism fixtures comparing syntax, semantic facts, coverage, HIR, ownership, instances, layouts, MIR, traces, native artifacts, and WebAssembly artifacts.
- [x] 9.4 Run `pnpm typecheck` and fix every match-related type error.
- [x] 9.5 Run `pnpm exec biome check .` and fix every match-related formatting or lint failure.
- [x] 9.6 Run `pnpm test` and confirm parser, semantics, ownership, evaluator, backend, tooling, and differential suites pass.
- [x] 9.7 Run `pnpm check` and resolve every in-scope repository gate failure.
- [x] 9.8 Run `pnpm release:candidate` and verify the changed public compiler representations, exports, and packaged language integrations.
- [x] 9.9 Run strict OpenSpec validation, inspect the final diff for unrelated or generated changes, and record any pre-existing failure exactly before handoff.
