## 1. Canonical Control-DAG Foundation

- [x] 1.1 Amend Wayfinder issue 06 and `roadmaps/project.md` to make compiler-published control a structured DAG, repetition an explicit loop-region semantic, and cyclic CFGs backend-private derived artifacts.
- [x] 1.2 Replace MIR's basic-block/successor vocabulary with canonical region identities, ordered operation regions, conditional regions, loop regions, cleanup regions, forward continuations, and terminal return/trap/repeat/exit outcomes.
- [x] 1.3 Define immutable region ownership and traversal queries that distinguish structural child/forward edges from lexical repeat/exit ports and expose one canonical topological order.
- [x] 1.4 Extend MIR verification with missing/duplicate identity, cross-function reference, lexical owner, invalid loop target, unreachable required outcome, and structural cycle checks returned as deterministic data.
- [x] 1.5 Replace the MIR text encoder with deterministic DAG encoding over logical types, target/layout facts, regions, operations, outcomes, shared continuations, cleanup, and provenance.
- [x] 1.6 Add hand-built straight-line, conditional-diamond, shared-cleanup, structured-loop, nested-loop, invalid-target, and cyclic-region verifier/encoder fixtures and fresh-process goldens.
- [x] 1.7 Migrate existing straight-line, call, conditional, aggregate, and array MIR constructors and tests to regions without preserving compatibility adapters for the old block API.

## 2. Existing Pipeline Migration to the DAG

- [x] 2.1 Update lowering of existing statements and conditionals so source-ordered HIR becomes operation/conditional/cleanup regions with explicit forward continuations and no general jump or branch graph.
- [x] 2.2 Update ownership cleanup materialization so returns, conditional arm exits, and shared joins traverse verifier-compatible cleanup regions without duplicating releases.
- [x] 2.3 Update the evaluator to execute existing straight-line and conditional programs directly from regions through an explicit execution stack.
- [x] 2.4 Update native LLVM emission to deterministically linearize existing regions into backend-local blocks while preserving compiler region provenance.
- [x] 2.5 Update direct WebAssembly emission to emit existing conditionals and cleanup from region nesting without CFG reconstruction or a dispatch loop.
- [x] 2.6 Update driver, analysis facade, stable encoders, compiler corpus, release-candidate consumer, and existing `/labs` MIR/evaluation/backend rows for the region API.
- [x] 2.7 Run the complete pre-mutation compiler, evaluator, native, WebAssembly, facade, and `/labs` suites to prove the DAG migration preserves every existing scalar, struct, and array behavior.

## 3. Mutable and Loop Syntax

- [x] 3.1 Add lossless `mut`, `while`, `break`, and `continue` tokens while preserving longest-token distinction between assignment `=` and equality `==`.
- [x] 3.2 Extend binding concrete syntax with the optional `mut` token and stable mutability/span encoding without changing ordinary immutable `let` behavior.
- [x] 3.3 Parse statement-form assignment over binding/field/index place syntax, retaining selector order, assignment punctuation, right-hand expressions, trivia, recovery nodes, and exact spans.
- [x] 3.4 Parse `while` conditions and brace-delimited bodies containing bindings, assignments, conditionals, nested loops, returns, `break`, and `continue` in exact source order.
- [x] 3.5 Add bounded recovery for missing or damaged mutable names, assignment sides, loop conditions/braces, and transfer statements without consuming following statements or declarations.
- [x] 3.6 Extend stable syntax encoding, syntax-file artifacts, lexer fixtures, parser fixtures, and round-trip tests with valid, nested, malformed, and equality-adjacent mutable loops.

## 4. Semantic Facts and Acyclic HIR

- [x] 4.1 Extend binding and local-scope facts with explicit immutable/mutable classification while preserving non-shadowing and existing canonical binding identities.
- [x] 4.2 Analyze assignment destinations through the canonical place-selector pipeline, publishing root mutability, selector facts, bounds mode, exact destination/source types, compatibility, provenance, and complete-or-unavailable write outcomes.
- [x] 4.3 Enforce assignment evaluation and diagnostic rules for immutable, moved, unknown, private, non-place, incompatible, overlapping-consuming, negative-index, upper-bound, and dynamically checked destinations without fabricating later facts.
- [x] 4.4 Publish canonical loop identities, lexical parents, strict `Bool` condition facts, ordered body regions, and complete-or-unavailable loop outcomes.
- [x] 4.5 Resolve each `break` and `continue` to its innermost enclosing loop and add stable diagnostics for transfers outside loops while preserving unrelated facts.
- [x] 4.6 Extend HIR binding contracts with mutability and add typed `WritePlace` carrying one root, ordered selectors, complete replacement value, access mode, and provenance.
- [x] 4.7 Add HIR region identities and structured `While`, `Break`, and `Continue` outcomes whose child/sequencing/continuation relationships remain acyclic.
- [x] 4.8 Extend semantic and HIR encoders and focused tests for scalar, field, indexed, nested, invalid, zero-iteration, early-transfer, and deterministic fresh-process cases.

## 5. Ownership, Replacement, and Loop Fixed Points

- [x] 5.1 Require one live mutable root and exclusive write access for every accepted assignment while leaving immutable reads and Copy behavior unchanged.
- [x] 5.2 Model replacement as check selectors, evaluate one complete right-hand value, clean the old non-Copy value exactly once, and commit the new complete root without introducing partial-initialization states.
- [x] 5.3 Reject overlapping consuming assignment and move-out-then-repair patterns while allowing whole-value and nested-place replacement that preserves complete ownership.
- [x] 5.4 Implement the finite deterministic loop-header ownership lattice and canonical worklist over incoming, fallthrough, and `continue` states.
- [x] 5.5 Reject repeating paths with incompatible liveness or missing initialization and accept paths that completely reinitialize a moved mutable binding before repetition.
- [x] 5.6 Join `break` states at the loop continuation, keep `return` as a function exit, and preserve outer-owner liveness across nested-loop exits.
- [x] 5.7 Extend cleanup plans with replacement cleanup plus exact iteration-fallthrough, `continue`, `break`, and `return` releases in reverse acquisition order without duplication.
- [x] 5.8 Add ownership tests for Copy and move-only replacement, nested struct/array writes, failed right-hand evaluation, repeated moves with replacement, incompatible loop headers, nested exits, and exact cleanup.

## 6. MIR Writes and Structured Loops

- [x] 6.1 Add verified MIR `WritePlace` with root local, canonical selectors, dynamic index locals and lengths, source/destination logical types, layout/calling-shape references, replacement cleanup, commit boundary, and provenance.
- [x] 6.2 Lower assignment so every selector check precedes right-hand evaluation and one write commit, preserving source order and the old root through any pre-commit trap.
- [x] 6.3 Lower `while` to one loop region with acyclic condition and body children, explicit following continuation, and lexical repeat/exit ports.
- [x] 6.4 Lower body fallthrough and `continue` to `Repeat(loopId)`, `break` to `Exit(loopId)`, and `return` to the function outcome through ownership-selected cleanup regions.
- [x] 6.5 Extend MIR verification to reject write mutability/type/selector/layout/cleanup disagreements, invalid repeat/exit targets, incompatible loop-header states, and duplicate cleanup paths.
- [x] 6.6 Extend MIR encoding and facade projections with writes, loop regions, lexical outcomes, topological edges, cleanup sharing, and exact source provenance.
- [x] 6.7 Add lowering/verifier tests for scalar and nested writes, dynamic bounds, transactional traps, zero and multiple iterations, nested loops, conditional transfers, early return, invalid cycles, and deterministic encodings.

## 7. Evaluation and Trace Semantics

- [x] 7.1 Extend logical evaluator storage with immutable root replacement for bindings, structs, nested fields, arrays, and indexed fields without exposing physical lanes or aliases.
- [x] 7.2 Execute `WritePlace` transactionally: resolve/check selectors, evaluate the right-hand value once, apply old cleanup, and commit one updated root.
- [x] 7.3 Execute loop regions iteratively through condition, body, repeat, exit, cleanup, return, and trap outcomes without flattening to a cyclic CFG or consuming host stack per iteration.
- [x] 7.4 Add compact deterministic region-entry, condition, iteration, write-check, replacement, repeat, exit, transfer, cleanup, and trap trace events with canonical IDs and spans.
- [x] 7.5 Add evaluator tests for indexed algorithms, nested replacement, zero/multiple/nested iterations, `continue`, `break`, early return, out-of-bounds pre-RHS trap, cleanup, and repeated trace equality.

## 8. Native and WebAssembly DAG Lowering

- [x] 8.1 Build the native region linearizer with deterministic region-to-block mapping, shared continuation reuse, conditional branches, cleanup blocks, loop headers/exits, and backend-private back-edges.
- [x] 8.2 Emit native checked root/field/index writes using compiler-owned layouts and selector paths while preserving transactional evaluation and replacement cleanup order.
- [x] 8.3 Build the WebAssembly region emitter with a lexical label stack mapping loop regions to nested exit `block` plus repeat `loop`, conditionals to `if`, and repeat/exit outcomes to exact branch depths.
- [x] 8.4 Emit WebAssembly checked root/field/index writes with the same planned logical lanes, pre-RHS bounds behavior, commit order, and replacement cleanup as evaluation/native.
- [x] 8.5 Delete or prove absent every WebAssembly CFG-reconstruction, relooper, dispatch-loop, or source-structure inference path and prevent backend labels/depths from entering MIR.
- [x] 8.6 Add native/WebAssembly structural tests for shared joins, zero and nested loops, inner continue, outer break, early return, cleanup, traps, deterministic IR/WAT/bytes, and canonical provenance.
- [x] 8.7 Extend the differential corpus with mutable scalar/struct/array algorithms and require evaluator/native/WebAssembly agreement for completion, traps, cleanup, and phase-owned rejection.

## 9. Analysis Facade and Unified Workbench

- [x] 9.1 Add immutable facade queries for binding mutability, write places and outcomes, loop identities/nesting, lexical transfers, ownership fixed points, cleanup exits, DAG regions/edges, traces, and backend control provenance.
- [x] 9.2 Update public encoders and exports so tools consume canonical topological order and semantic repeat/exit ports without reconstructing loops from emitted branches or mutable graph objects.
- [x] 9.3 Add a control-DAG view to the unified `/labs` registry and update syntax, HIR, ownership, MIR, evaluation, native, and WebAssembly rows for mutation and loops.
- [x] 9.4 Add accessible textual equivalents and coordinated selection from assignment/transfer spans through semantic facts, HIR regions, cleanup, MIR outcomes, trace events, and backend-local branches.
- [x] 9.5 Add browser-local presets for immutable-write rejection, scalar/field/index mutation, Copy/move-only replacement, zero/multiple/nested loops, conditional break, continue, early return, bounds trap, invalid condition/transfer, and loop-header ownership failure.
- [x] 9.6 Add facade-boundary, immutability, reload, accessibility, coordinated-selection, and fresh-process determinism tests without creating a standalone loop inspector.

## 10. Verification and Release Gates

- [x] 10.1 Run focused syntax, semantic, HIR, ownership, MIR-DAG, evaluator, native, WebAssembly, facade, and `/labs` suites and resolve every mutable-loop failure.
- [x] 10.2 Run the full legacy plus mutable-loop three-engine differential corpus and fresh-process artifact comparisons for every supported target.
- [x] 10.3 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in repository-required order and record any failure with its ownership.
- [x] 10.4 Run `pnpm check` and resolve all repository-wide validation failures attributable to this change.
- [x] 10.5 Run `pnpm release:candidate` because compiler representation contracts, package contents, and public facade exports change.
- [x] 10.6 Run `openspec validate add-mutable-bindings-and-structured-loops --strict`, `openspec validate --all --strict`, and `git diff --check` before implementation handoff.
