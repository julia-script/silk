## 1. Canonical union types and lossless syntax

- [x] 1.1 Extend the semantic `Type` actor with `Never` and privately constructed normalized unions, including flattening, empty/singleton collapse, nominal-member validation, canonical deduplication, locale-independent ordering, immutable member storage, keys, equality, encoding, and nominal traversal.
- [x] 1.2 Add typed normalization outcomes for invalid non-nominal leaves and unit tests covering order, nesting, duplicates, `Never`, singleton collapse, qualification, and stable fresh-process encodings.
- [x] 1.3 Add the standalone type-union token while preserving longest-token recognition of `|>` and update token/SyntaxTree stable encoders plus language highlighter token classification.
- [x] 1.4 Parse low-precedence union types and parenthesized declared types in parameters, returns, struct fields, fixed-array elements, and other supported type positions without treating `|` as an expression operator.
- [x] 1.5 Add bounded recovery for missing left/right members, damaged parentheses, repeated separators, and following declaration/parameter boundaries while preserving every token, trivia item, and exact span.
- [x] 1.6 Extend declared-type facts with source-ordered union members, separators, nested provenance, `Never`, and complete-or-unavailable normalized outcomes that retain independent resolved members after damage.
- [x] 1.7 Add stable diagnostics for unresolved members, invalid non-nominal members, and unavailable normalized unions without changing existing nominal/array resolution behavior.
- [x] 1.8 Add lexer, parser, declaration-analysis, highlighting, round-trip, malformed-input, and fresh-process tests for the complete union type syntax slice.

## 2. Contextual conversion, semantic facts, and HIR

- [x] 2.1 Implement one immutable type-compatibility result covering exact identity, nominal injection, union widening, and incompatibility with canonical total source-to-target member mappings.
- [x] 2.2 Route declared return and call-parameter checking through the compatibility operation so conversions are immediate-contextual and source expression/binding types remain precise.
- [x] 2.3 Route struct-field construction, contextual array elements, and assignment destinations through the same compatibility operation without adding general subtyping or retroactive inference.
- [x] 2.4 Publish source type, expected-context owner, target union, mapping, access mode, provenance, and complete-or-unavailable conversion outcome as immutable semantic facts.
- [x] 2.5 Add deterministic diagnostics for non-containing targets, unavailable sources, attempted narrowing, and contexts with no executable conversion while retaining all independent facts.
- [x] 2.6 Extend HIR with normalized union types and one explicit `UnionConvert` expression carrying exact source/target types, conversion kind, total member mapping, access mode, expected-context provenance, and span.
- [x] 2.7 Extend HIR construction and stable encoding across returns, arguments, aggregates, arrays, writes, and loops while keeping source/child/continuation relationships acyclic.
- [x] 2.8 Add semantic/HIR tests for exact use, precise unannotated bindings, every expected-context boundary, nominal injection, widening, nested/duplicate normalization, invalid targets, unavailable members, and deterministic encodings.

## 3. Affine ownership and active-member cleanup

- [x] 3.1 Extend type-property analysis so union Copy/cleanup classification derives recursively from every canonical nominal member and `Never` remains uninhabited and cleanup-free.
- [x] 3.2 Make owned nominal injection and union widening transfer one complete payload and consume a non-Copy source without exposing partial payload moves or stored borrows.
- [x] 3.3 Add canonical member-indexed union cleanup cases to ownership facts and plans, with each active case reusing that member's ordinary recursive cleanup and inactive cases performing no action.
- [x] 3.4 Preserve one union obligation through bindings, calls, returns, structs, arrays, moves, assignments, loop fixed points, `continue`, `break`, traps, and cleanup-region sharing.
- [x] 3.5 Integrate owned union replacement with the transactional write model so the old active payload is released exactly once before one complete replacement commits.
- [x] 3.6 Add ownership tests for injection, widening, repeated moves, aggregate/array containment, mutable replacement, loop transfers, returns, invalid borrow storage, inactive cases, and exact cleanup order.

## 4. Runtime discovery and compiler-owned layout

- [x] 4.1 Extend instance keys and deterministic runtime reachability to normalized unions appearing in contracts, locals, aggregates, arrays, conversions, and cleanup, following every canonical member dependency exactly once.
- [x] 4.2 Add union layout entries with a private 32-bit discriminant, canonical member ordinals, target-aware max-member payload size/alignment, payload offset, total alignment/size, and deterministic padding.
- [x] 4.3 Propagate invalid or unavailable member layouts into one union-layout failure with exact dependency provenance before MIR construction.
- [x] 4.4 Extend layout plans with a backend-neutral `SumShape` containing the tag lane, fixed payload slots, member logical shapes, zero-fill policy, and complete per-member lane mappings.
- [x] 4.5 Extend layout lookup, verification, text encoding, repeated-layout queries, and canonical ordering rules to structural unions without duplicating target or representation facts.
- [x] 4.6 Add target-layout tests for zero-sized and differently sized/aligned members, embedded unions, equivalent spellings, unavailable recursive members, tag order, padding, calling mappings, and fresh-process determinism.

## 5. MIR conversion and DAG-preserving lowering

- [x] 5.1 Extend MIR logical types, locals, contracts, fields, arrays, calls, returns, writes, and drops with canonical union types while keeping target/layout facts in the shared compiler plan.
- [x] 5.2 Add verified `ConvertUnion` with source/destination locals, exact logical types, injection/widening kind, total canonical member/tag mapping, layout/calling-shape references, access mode, and provenance.
- [x] 5.3 Carry ownership-selected union cleanup cases through MIR drop and replacement metadata without translating them into compiler-owned cyclic or general branch graphs.
- [x] 5.4 Lower HIR conversions in source order, emitting no operation for exact identity and preserving one move/commit boundary for owned injection, widening, and replacement.
- [x] 5.5 Extend MIR verification for canonical member order, duplicate/invalid members, subset direction, total mappings, local/type/layout/calling-shape disagreement, ownership mode, cleanup cases, and DAG invariants.
- [x] 5.6 Extend MIR text encoding and facade projections with normalized unions, mappings, layouts, sum shapes, cleanup cases, and exact provenance in canonical topological order.
- [x] 5.7 Add hand-built and lowered MIR fixtures for injection, widening, aggregate/array transport, replacement, loop transport, invalid mappings/layouts, cleanup sharing, and deterministic fresh-process goldens.

## 6. Logical evaluation oracle

- [x] 6.1 Add immutable evaluator union values containing normalized union type, active canonical nominal member, and one complete logical payload without physical tag or byte-storage exposure.
- [x] 6.2 Execute nominal injection and union widening from verified mappings, preserving the active payload and remapping only its enclosing union/member tag identity.
- [x] 6.3 Transport union values through parameters, returns, structs, arrays, bindings, moves, reads, and transactional writes with exact logical types.
- [x] 6.4 Dispatch union cleanup through the ownership-selected active-member case and prove inactive cases and transferred owners perform no cleanup.
- [x] 6.5 Add deterministic injection, widening, transport, replacement, and cleanup trace events using canonical type/member identities and source provenance.
- [x] 6.6 Add evaluator tests for nested conversions, aggregate/array containment, mutation, loops, move-only transfer, replacement cleanup, invalid MIR, traps, and repeated trace equality.

## 7. Native LLVM realization

- [x] 7.1 Extend native type and calling-shape realization with the compiler-planned 32-bit tag, aligned payload storage, payload slots, member lane mappings, padding, and zero-fill behavior.
- [x] 7.2 Emit nominal injection and statically known tag installation without choosing tags or payload placement outside the compiler layout plan.
- [x] 7.3 Emit dynamic union widening with deterministic backend-private tag remapping while keeping every block, branch, and physical type out of MIR and facade control relationships.
- [x] 7.4 Preserve union layouts and calling shapes through calls, returns, structs, arrays, reads, moves, and transactional writes.
- [x] 7.5 Emit active-member drop and replacement cleanup from the verified cleanup cases, releasing exactly one payload and preserving compiler provenance.
- [x] 7.6 Add native structural/execution tests for layouts, injections, widening, transport, mutation, loops, cleanup, traps, stable IR/bitcode, and agreement with evaluation.

## 8. Direct WebAssembly realization

- [x] 8.1 Extend WebAssembly value/calling-shape realization with the planned i32 tag lane, fixed payload lanes, member mappings, padding lanes, and deterministic zero fill.
- [x] 8.2 Emit injection and widening from verified mappings using structured backend-private control where dynamic tag remapping is required, without a dispatch loop or compiler-graph reconstruction.
- [x] 8.3 Preserve union tags and payload slots through calls, returns, structs, arrays, reads, moves, and transactional writes.
- [x] 8.4 Emit active-member drop and replacement cleanup through structured private branches with exact member cleanup and provenance.
- [x] 8.5 Add WebAssembly structural/execution tests for layouts, injections, widening, nested aggregate/array transport, mutation, loops, cleanup, deterministic WAT/bytes, and agreement with evaluation/native.

## 9. Facade, language tooling, and unified labs

- [x] 9.1 Add immutable facade queries for source/canonical members, `Never`, compatibility outcomes, member maps, ownership, cleanup, discovery, layouts, sum shapes, HIR/MIR conversions, evaluator values/events, and backend provenance.
- [x] 9.2 Update public encoders and exports so consumers use canonical union identities and ordered mappings without normalizing syntax, assigning tags, decoding payloads, or inspecting mutable objects.
- [x] 9.3 Update CodeMirror, TextMate, and generated VS Code grammar fixtures so type-level unions and `Never` highlight correctly while `|>` remains the pipeline operator.
- [x] 9.4 Add structural-union material to the unified `/labs` registry and existing syntax, semantic, HIR, ownership, layout, MIR, evaluation, native, and WebAssembly rows without a standalone inspector.
- [x] 9.5 Add coordinated selection and accessible textual equivalents from a source member/conversion through canonical mappings, layout, cleanup, trace, and emitted provenance.
- [x] 9.6 Add browser-local presets for normalization, `Never`, precise inference, injection, widening, boundaries, aggregate/array containment, moves, replacement, invalid members/targets, unresolved facts, and unavailable layouts.
- [x] 9.7 Add facade-boundary, immutability, reload, accessibility, highlighting, coordinated-selection, and fresh-process determinism tests for the union workbench.

## 10. Differential corpus and completion gates

- [x] 10.1 Extend the compiler corpus and release-candidate consumer with supported union programs covering normalization, injection, widening, call/return transport, aggregates, arrays, mutation, loops, and move-only cleanup plus phase-owned invalid cases.
- [x] 10.2 Require evaluator/native/WebAssembly agreement for completion, traps, active payload behavior, replacement, and cleanup, and require byte-identical compiler/backend artifacts across fresh processes.
- [x] 10.3 Run focused syntax, semantic, HIR, ownership, discovery, layout, MIR-DAG, evaluator, native, WebAssembly, facade, language-tooling, and `/labs` suites and resolve every union failure.
- [x] 10.4 Run the full legacy plus structural-union three-engine differential corpus and fresh-process artifact comparisons for every supported target.
- [x] 10.5 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in repository-required order and record any failure with its ownership.
- [x] 10.6 Run `pnpm check` and resolve every repository-wide validation failure attributable to this change.
- [x] 10.7 Run `pnpm release:candidate` because semantic types, compiler representations, package behavior, tooling, and public facade contracts change.
- [x] 10.8 Run `openspec validate normalize-structural-unions --strict`, `openspec validate --all --strict`, and `git diff --check` before implementation handoff.
