## 1. Slice syntax and canonical types

- [x] 1.1 Add lossless `&[T]` / `&mut [T]` type nodes and `&value` / `&mut value` borrow-expression nodes with bounded recovery and exact source spans.
- [x] 1.2 Extend syntax traversal and the canonical formatter for shared and exclusive slice types and borrow expressions, preserving comments and damaged syntax deterministically.
- [x] 1.3 Add the canonical semantic slice type with access and element identity, plus encoding, ordering, equality, substitution, traversal, and recursive `containsBorrow` support.
- [x] 1.4 Add parser, formatter, and type-unit coverage for nested generic elements, trivia, missing brackets/elements, shared/exclusive distinction, deterministic encoding, and substitution.

## 2. Slice declarations, calls, and semantic places

- [x] 2.1 Resolve slice parameter types over concrete and declaration-owned generic elements while rejecting slices in return types and recursively owned storage positions.
- [x] 2.2 Analyze explicit whole-array call borrows from direct stable bindings, including mutable-root checks, expected slice compatibility, no implicit decay, and explicit unavailable facts.
- [x] 2.3 Analyze compatible call-scoped reborrows from slice parameters, retaining parent access suspension and rejecting shared-to-exclusive strengthening.
- [x] 2.4 Reject standalone slice bindings, temporary or subplace borrows, lazy/capturing boundaries, and every recursive escape path with stable phase-owned diagnostics.
- [x] 2.5 Add semantic `length: I32` projection and runtime-bounded borrowed index places whose field projections preserve shared or exclusive access.
- [x] 2.6 Add semantic tests for different source lengths resolving to one slice type, invalid borrow positions, implicit decay, access mismatch, runtime index facts, non-Copy element projection, and damaged-input availability.

## 3. Root-attached ownership loans

- [x] 3.1 Add deterministic loan identities and root/access/start/end facts to function ownership analysis without adding runtime borrow fields.
- [x] 3.2 Keep each call argument loan live through later argument evaluation and the complete ordinary call; accept shared/shared and reject shared/exclusive or exclusive/exclusive root conflicts.
- [x] 3.3 Implement call-scoped shared and exclusive reborrow trees, suspending parent exclusive permission until the nested call returns.
- [x] 3.4 Reject direct owner read, mutation, replacement, move, or cleanup while a conflicting loan is live, and reject every move of a non-Copy value through a borrowed place.
- [x] 3.5 End loans before cleanup on return, typed failure, early return, `break`, `continue`, and fallthrough while preserving existing trap semantics and exactly-once element cleanup.
- [x] 3.6 Add ownership tests for alias matrices, later call arguments, reborrow suspension/restoration, owner operations, recursive escape, structured exits, zero-sized elements, and displaced-value cleanup.

## 4. HIR and instance discovery

- [x] 4.1 Extend HIR with typed whole-root slice formation/reborrow, loan identity, runtime length, and borrowed place roots while preserving exact provenance and evaluation order.
- [x] 4.2 Reuse the existing complete-replacement HIR protocol for exclusive slice assignment so bounds validation precedes replacement evaluation and cleanup precedes commit.
- [x] 4.3 Extend HIR traversal, verification, text/debug projections, and unavailable-state tests for slice operations without encoding raw addresses.
- [x] 4.4 Extend instance discovery and reachability through slice element types while excluding source fixed-array length from slice-taking instance keys.
- [x] 4.5 Prove that calls from distinct array lengths reuse one function instance and that generic slice functions specialize only by canonical concrete element arguments.

## 5. Target-aware slice layouts and typed lanes

- [x] 5.1 Add an internal target-width address scalar and compiler-owned slice layout entry with exact address/length offsets, size, alignment, padding, and element stride.
- [x] 5.2 Generalize calling-shape lanes to carry heterogeneous typed scalars while preserving byte-identical shapes for every existing non-slice type.
- [x] 5.3 Add one address-plus-`I32` slice calling shape reused by shared and exclusive access and consumed uniformly at call boundaries.
- [x] 5.4 Extend layout catalogs, reachability, structural keys, verification, and deterministic text projection for concrete and generic slice elements.
- [x] 5.5 Add target-layout tests for native pointer width, Wasm address width, aggregate stride/padding, zero-length and zero-sized elements, access-mode representation reuse, and malformed typed lanes.

## 6. Structured MIR slice operations

- [x] 6.1 Add logical slice types, formation/reborrow operations, and ordered loan endings to MIR regions without introducing control back-edges or public pointer types.
- [x] 6.2 Generalize MIR place roots and runtime element selectors so slice length, address, access, and element type derive from the same slice local.
- [x] 6.3 Lower shared reads, Copy field projections, exclusive checked replacement, and slice `length` while retaining existing source evaluation and cleanup order.
- [x] 6.4 Lower loan endings on every structured outcome before owner cleanup, including loop repeat/exit outcomes.
- [x] 6.5 Extend MIR verification for loan/root conflicts, parent reborrow suspension, missing or duplicate endings, mismatched slice bounds, access permissions, and cleanup under a live loan.
- [x] 6.6 Extend MIR traversal, instance substitution, deterministic encoding/text, samples, and verifier fixtures for valid and malformed slice DAGs.

## 7. Logical evaluator addressable storage

- [x] 7.1 Introduce deterministic evaluator frame and cell identities for address-taken array bindings while leaving non-address-taken values on the existing path.
- [x] 7.2 Realize slice values as backing cell, base element, and runtime length and pass that identity across nested ordinary calls without copying array contents.
- [x] 7.3 Evaluate slice length, checked reads/projections, and exclusive write-through replacement against authoritative cells with check-before-RHS and exactly-once cleanup.
- [x] 7.4 Add evaluator tests for multiple source lengths, caller-visible exclusive mutation, nested calls, negative/equal-length traps, zero-sized elements, and replacement trace ordering.

## 8. Native LLVM addressable lowering

- [x] 8.1 Teach native signature and call emission to consume heterogeneous typed lanes, using real pointer-width values for internal slice addresses.
- [x] 8.2 Identify address-taken fixed-array roots and materialize them in aligned entry-block storage using the compiler layout plan while retaining scalarized lowering elsewhere.
- [x] 8.3 Lower slice formation, runtime length, stride-aware checked projection, and exclusive stores through authoritative native storage.
- [x] 8.4 Invalidate or reload cached aggregate lanes after potentially mutating slice calls so same-block caller reads observe callee writes.
- [x] 8.5 Add native IR, execution, ABI, aggregate-stride, stale-value, bounds, cleanup, and determinism tests for shared and exclusive slices.

## 9. Direct Wasm shadow frames and slice lowering

- [x] 9.1 Teach Wasm signature, parameter, result, and local-bundle planning to consume the compiler's typed address-plus-length slice lanes.
- [x] 9.2 Add a deterministic private linear-memory frame plan for reachable address-taken locals, aligned after static data and emitted only when required.
- [x] 9.3 Emit frame reservation, overflow/capacity checks, deterministic memory growth or trap, and exactly-once restoration on every normal structured exit.
- [x] 9.4 Materialize address-taken arrays contiguously and lower stride-aware slice checks, reads, projections, and exclusive write-through stores against linear memory.
- [x] 9.5 Add Wasm tests for multiple source lengths, caller-visible mutation, aggregate stride, early-return restoration, nested and recursive frame isolation, exhaustion, zero-sized elements, and deterministic text/bytes.

## 10. Three-engine and unified-inspector acceptance

- [x] 10.1 Generalize the canonical coverage fold to `&[I32]`, invoke it with the reviewed three- and six-element arrays, retain results `40` and `42`, and assert one instance and one symbol.
- [x] 10.2 Add the canonical exclusive-slice fixture over move-only aggregate elements and require caller-visible replacement plus exactly-once cleanup across evaluator, native, and Wasm execution.
- [x] 10.3 Add deterministic negative fixtures for implicit decay, immutable exclusive borrow, conflicting arguments, recursive storage/return, standalone binding, temporary/subplace borrow, access strengthening, non-Copy extraction, and out-of-bounds access.
- [x] 10.4 Extend fresh-process determinism coverage across syntax, semantic facts, ownership loans, HIR, instances, layout, MIR, evaluation traces, native artifacts, Wasm frames, and Wasm artifacts.
- [x] 10.5 Update the unified `/labs` presets and coordinated projections to expose the exact shared and exclusive fixtures, loan/source relationships, slice layout lanes, runtime results, and stopped invalid paths accessibly.
- [x] 10.6 Update the project roadmap to mark lexical runtime slices complete and promote scoped allocation with typed slots and restricted drop hooks as the next memory change.

## 11. Repository gates

- [x] 11.1 Run focused compiler and workbench tests throughout implementation and resolve every in-scope failure.
- [x] 11.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`; report any failure with its exact command and provenance.
- [x] 11.3 Run `pnpm release:candidate` because compiler package contents and public types change, and resolve every in-scope packaging or export failure.
- [x] 11.4 Run strict OpenSpec validation and inspect the final diff for accidental allocators, raw pointer APIs, local/stored slices, range syntax, iterator behavior, backend-owned ABIs, or unrelated changes.
