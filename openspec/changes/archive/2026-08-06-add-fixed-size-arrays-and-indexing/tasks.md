## 1. Syntax and Canonical Types

- [x] 1.1 Add lossless tokens and concrete syntax nodes for `Array<T, N>` types, array literals, and postfix index expressions, including stable encoders and source spans.
- [x] 1.2 Parse nested fixed-array types with a non-negative decimal length and recovery that remains inside the type constructor.
- [x] 1.3 Parse empty, nested, comma-separated, and trailing-comma array literals while retaining punctuation, trivia, recovery nodes, and exact spans.
- [x] 1.4 Extend the postfix parser so calls, field projections, and repeated index projections compose left-to-right above prefix and infix operators.
- [x] 1.5 Add the recursive canonical `FixedArray` type form, normalized length identity, equality, ordering, and deterministic encoding without creating a nominal `Array` declaration.
- [x] 1.6 Resolve fixed-array element types through ordinary local, selected, and namespace-qualified scopes while retaining causal unavailability for invalid nested elements.
- [x] 1.7 Add focused syntax, recovery, round-trip, canonicalization, and imported-element resolution tests, including nested and zero-length types.

## 2. Contextual Semantic Analysis

- [x] 2.1 Thread an optional expected type through expression elaboration without changing independently discovered scalar facts or established scalar diagnostics.
- [x] 2.2 Analyze non-empty array literals by exact common element type and written length when no array expectation is available.
- [x] 2.3 Propagate contextual array element types and lengths into empty and nested literals in returns, arguments, bindings, and other typed positions.
- [x] 2.4 Publish every literal element fact, contextual expectation, compatibility result, canonical element type, length outcome, and complete-or-unavailable construction result.
- [x] 2.5 Analyze index expressions as typed place projections requiring an available array subject and `I32` index, with exact result type and provenance.
- [x] 2.6 Classify bounds as statically proven, statically invalid, or requiring a runtime check from the canonical length and index expression.
- [x] 2.7 Add stable diagnostics for missing empty-literal context, incompatible elements, contextual length or element mismatch, invalid subject/index types, negative constants, and upper-bound constants.
- [x] 2.8 Add semantic and facade-fact tests for inferred, contextual, empty, nested, incompatible, constant-index, and dynamic-index cases while pinning prior scalar corpora.

## 3. HIR Places and Ownership

- [x] 3.1 Extend HIR contracts, bindings, parameters, calls, and results with recursive canonical fixed-array types.
- [x] 3.2 Lower accepted literals to one complete array construction that preserves exact left-to-right operand evaluation and ascending logical indices.
- [x] 3.3 Add typed `IndexPlace` HIR with subject, array type, index expression, element type, access request, bounds mode, and selector span.
- [x] 3.4 Preserve mixed nested `IndexPlace` and `FieldPlace` chains in source order and classify the final requested access without fabricating intermediate aggregate values.
- [x] 3.5 Make ownership classification recursive so an array is Copy exactly when its element type is Copy, independent of its length.
- [x] 3.6 Allow Copy-leaf reads through mixed place chains while rejecting indexed non-Copy extraction as a partial move and preserving whole-root liveness.
- [x] 3.7 Support whole-array moves, calls, returns, and bindings as one ownership transfer with the existing use-after-move rules.
- [x] 3.8 Add symbolic recursive array cleanup plans that visit live elements exactly once in ascending index order, including explicit zero-action and Copy-only facts.
- [x] 3.9 Add HIR and ownership tests for Copy arrays, move-only arrays, zero-length move-only arrays, indexed fields, rejected partial moves, whole moves, and nested cleanup.

## 4. Reachability, Layout, and Calling Shapes

- [x] 4.1 Extend runtime reachability to follow fixed-array contracts, values, constructions, indexed places, projections, and cleanup recursively without planning unused array types.
- [x] 4.2 Include canonical element identity and every nested length in deterministic instance keys and encodings, including distinct zero-lane types.
- [x] 4.3 Extend the shared memoized target-layout solver with repeated-element layout, checked stride and total-size arithmetic, element alignment, and zero-length alignment retention.
- [x] 4.4 Preserve phase-owned unavailable layout outcomes with canonical type and target provenance for element failure or arithmetic overflow.
- [x] 4.5 Replace field-only layout paths with one canonical selector vocabulary that distinguishes field identities from array element indices.
- [x] 4.6 Represent struct products and array repetitions in one symbolic scalar-leaf calling-shape tree with checked lane counts and lazy deterministic traversal.
- [x] 4.7 Update layout and calling-shape encoders and queries to expose canonical selector paths without backend-derived traversal or eager expansion during analysis.
- [x] 4.8 Add reachability, instance-key, nested-layout, padding, zero-length, overflow, array-of-struct, struct-of-array, selector-order, and fresh-process determinism tests.

## 5. MIR and Evaluation

- [x] 5.1 Extend MIR types, locals, parameters, results, calls, moves, and drops with canonical logical fixed arrays linked to the selected layout and calling shape.
- [x] 5.2 Add complete array construction with ascending canonical operands and preserve source evaluation order during lowering.
- [x] 5.3 Lower each readable Copy place chain to one `ReadPlace` over the root logical aggregate and ordered field/index selectors, without independently owned intermediate locals.
- [x] 5.4 Carry dynamic `I32` index locals, canonical lengths, exact selector spans, and immediate pre-selection bounds checks in `ReadPlace`.
- [x] 5.5 Extend MIR verification to reject count, operand, index, length, selector, layout, calling-shape, destination, and ownership disagreements before execution or emission.
- [x] 5.6 Extend deterministic MIR text encoding with recursive array types, construction, selectors, checks, lengths, and provenance.
- [x] 5.7 Add immutable complete array values to the evaluator and support construction, whole moves, calls, returns, parameter binding, and cleanup without exposing physical lanes.
- [x] 5.8 Evaluate checked place reads selector by selector, trapping before an invalid dynamic access and returning only the final Copy value.
- [x] 5.9 Add compact deterministic array construction, selector, bounds, transfer, and cleanup trace events without dumping large values or ABI details.
- [x] 5.10 Add MIR verifier and evaluator tests for construction completeness, mixed chains, in-bounds reads, negative and upper-bound traps, zero length, calls, moves, cleanup, and trace provenance.

## 6. Native, WebAssembly, and Driver Parity

- [x] 6.1 Teach native signature, local, construction, call, result, move, and cleanup realization to traverse the compiler-owned symbolic calling shape in canonical lane order.
- [x] 6.2 Emit native checked dynamic selection and subsequent mixed field/index projection from `ReadPlace` without recalculating layout, stride, or selector order.
- [x] 6.3 Teach direct WebAssembly signatures, locals, construction, calls, results, moves, and cleanup to traverse the same symbolic shape, including multi-value and zero-lane contracts.
- [x] 6.4 Emit WebAssembly bounds checks and dynamic lane-group selection with the same success and trap behavior as MIR evaluation and native execution.
- [x] 6.5 Reject unavailable or incompatible array plans before artifact construction and preserve deterministic symbols, LLVM IR, WAT, and binaries for equivalent inputs.
- [x] 6.6 Extend driver phase reports and the differential corpus with inferred, contextual, empty, nested, struct-element, moved, indexed, mismatched, unavailable-layout, and bounds-failure arrays.
- [x] 6.7 Add evaluator/native/WebAssembly parity tests for construction, internal parameters and results, array-of-struct indexed fields, zero lanes, negative indices, upper-bound indices, and dynamic success.
- [x] 6.8 Add repeated-compilation and fresh-process checks for diagnostics, HIR, ownership, layouts, MIR, traces, symbols, IR, WAT, binaries, and phase-owned failures.

## 7. Analysis Facade and Unified Workbench

- [x] 7.1 Extend immutable analysis snapshots with direct queries for canonical array types, literal elements and completeness, indexed-place chains, bounds modes, repeated layouts, calling paths, MIR, traces, and codegen outcomes.
- [x] 7.2 Update public facade encoders and exports so tooling consumes authoritative array lengths, selectors, cleanup order, and lane paths without reconstruction or mutable identity.
- [x] 7.3 Add an array-values view to the unified `/labs` registry and update the existing syntax, HIR, ownership, layout, MIR, evaluation, and backend panes for all new variants.
- [x] 7.4 Add coordinated links from array syntax through semantic facts, HIR, MIR checks, evaluation events, and emitted branch provenance, with accessible textual equivalents.
- [x] 7.5 Add browser-local presets for inferred, contextual, empty, nested, struct-element, evaluation-order, whole-moved, Copy-read, indexed-field, constant-out-of-bounds, dynamic-trap, type-mismatch, length-mismatch, partial-move, and unavailable-layout cases.
- [x] 7.6 Add facade boundary, immutability, reload, accessibility, coordinated-selection, and fresh-process determinism tests without introducing a standalone legacy inspector.

## 8. Verification and Release Gates

- [x] 8.1 Run focused syntax, semantic, HIR, ownership, layout, MIR, evaluator, backend, facade, and `/labs` suites and resolve every array-specific failure.
- [x] 8.2 Run the complete three-engine differential and determinism corpus in fresh processes for supported targets.
- [x] 8.3 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in repository-required order and record any failure with its ownership.
- [x] 8.4 Run `pnpm check` and resolve all repository-wide validation failures attributable to this change.
- [x] 8.5 Run `pnpm release:candidate` because compiler package contents and public facade exports change.
- [x] 8.6 Run `openspec validate add-fixed-size-arrays-and-indexing --strict` and confirm every proposal artifact remains coherent before implementation handoff.
