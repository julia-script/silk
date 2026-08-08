## 1. Unsafe and conformance syntax

- [x] 1.1 Add lossless lexer and syntax-tree forms for `unsafe { ... }`, `impl Allocator for Name` operation mappings, and restricted `impl Drop for Name` bodies without changing ordinary qualified-call syntax.
- [x] 1.2 Parse unsafe blocks and both conformance forms with bounded recovery for missing braces, capability or provider names, operation mappings, hook bodies, and following declarations.
- [x] 1.3 Extend canonical formatting, syntax traversal, semantic-target lookup, CodeMirror, TextMate, and VS Code highlighting for the accepted forms.
- [x] 1.4 Add parser, formatter, losslessness, malformed-source, and highlighting tests proving no named-scope or allocator-specific syntax is accepted.

## 2. Nominal providers and allocator conformance

- [x] 2.1 Add canonical nominal `SystemAllocator`, generic `RawBuffer<T>`, lexical `Slot<T>`, and private reclaim-entry identities with deterministic equality, substitution, ordering, traversal, and encoding.
- [x] 2.2 Replace the provisional implementation-erased `SystemAllocator.make() -> Allocator` shortcut with nominal `SystemAllocator` construction and an ordinary `Allocator` conformance witness.
- [x] 2.3 Index `impl Allocator for Provider` mappings and validate that the selected qualified actor operation has exclusive provider access, validated `Layout` input, `Allocation` success, and only allocation-free `OutOfMemory` failure.
- [x] 2.4 Extend capability provision to accept a concrete nominal provider plus its conformance witness while keeping the requirement keyed by capability and role and the provider borrow call-scoped.
- [x] 2.5 Add a user-authored deterministic quota allocator fixture through the same conformance path and reject missing, duplicate, foreign-module, or contract-incompatible implementations.
- [x] 2.6 Add semantic and requirement-row tests for default and named roles, two simultaneous allocator providers, nominal provider retention in facts, and zero allocator-kind branches.

## 3. Self-contained Allocation authority

- [x] 3.1 Define the private compiler model for one active Allocation ticket: logical base, validated Layout, noncapturing reclaim entry, optional raw context, and active ownership state.
- [x] 3.2 Add the allocator-author-only unsafe adoption operation and require a statically known noncapturing, synchronous, infallible, requirement-free reclaim function with target-compatible base, context, and Layout inputs.
- [x] 3.3 Ensure successful allocator calls atomically return one active affine Allocation and failed calls return allocation-free `OutOfMemory` with no block, ticket, or cleanup obligation.
- [x] 3.4 End the exclusive provider loan when allocation returns and reject retained provider borrows, provider-dependent result metadata, forged reclaim entries, and allocator-kind tags.
- [x] 3.5 Extend explicit `drop value` and ordinary moves so Allocation authority transfers or releases exactly once, including zero-byte and over-aligned allocations.
- [x] 3.6 Add semantic and ownership tests for adoption, move chains, early drop, lexical drop, provider access ending before release, duplicate consumption, invalid reclaim contracts, and atomic exhaustion.

## 4. RawBuffer, Slot, and restricted Drop

- [x] 4.1 Elaborate compiler-checked repeated layout for concrete `T` and runtime count, including padded stride, checked target-width total bytes, zero-sized elements, and equality with the Allocation's recorded Layout.
- [x] 4.2 Implement unsafe `RawBuffer<T>` construction by consuming one matching Allocation and retaining only the owner plus runtime logical count.
- [x] 4.3 Implement bounds-checked lexical exclusive `Slot<T>` projection and the minimum unsafe write, take/move, and destroy-in-place operations without a runtime initialization bitmap.
- [x] 4.4 Extend ownership loans to reject Slot escape, incompatible aliasing, buffer move or drop under a live Slot, mismatched element provenance, and use after slot value transfer.
- [x] 4.5 Validate exactly one compiler-sealed Drop hook for an affine nominal type and reject Copy targets, duplicate hooks, direct calls, failure, requirements, allocation, self replacement or move, escaping self borrows, and recursive invocation.
- [x] 4.6 Insert accepted hooks before fixed declaration-order field cleanup on fallthrough, return, `break`, `continue`, typed failure, and explicit early drop while retaining no trap-unwind promise.
- [x] 4.7 Add an ordinary affine initialized-prefix guard fixture whose Drop loop destroys exactly the live prefix before RawBuffer field cleanup.
- [x] 4.8 Add the full negative and cleanup-order test matrix for unsafe boundaries, bounds, provenance, initializedness contracts, live loans, hook restrictions, partial rollback, and duplicate cleanup.

## 5. HIR and public analysis

- [x] 5.1 Extend HIR with unsafe boundaries, allocator conformance dispatch, validated allocation outcomes, Allocation adoption and ownership, RawBuffer/Slot operations, restricted Drop declarations and calls, and cleanup provenance.
- [x] 5.2 Keep raw reclaim addresses and backend heap policy private while giving every logical allocation, ticket, buffer, slot, hook, and cleanup operation deterministic canonical identities and source provenance.
- [x] 5.3 Extend HIR verification, traversal, substitution, debug text, deterministic encoding, and unavailable-state handling for every new form.
- [x] 5.4 Extend `Analysis` with immutable source-correlated projections for provider witnesses, allocations, loans, hooks, cleanup plans, layout, MIR, evaluation events, and backend realization without exposing mutable storage or host pointers.
- [x] 5.5 Add HIR and facade tests for successful and unavailable paths, cross-phase identity correlation, provider/result separation, and absence of fabricated downstream artifacts.

## 6. Instance discovery and target layout

- [x] 6.1 Extend instance discovery through concrete allocator witness operations, reclaim entries, `RawBuffer<T>`, Slot operations, restricted hooks, and recursively cleaned field types.
- [x] 6.2 Keep runtime counts, allocation ordinals, provider object identities, logical addresses, and cleanup-event identities out of instance keys and prove one typed helper instance is reused across counts.
- [x] 6.3 Add compiler-owned target layouts and calling shapes for Allocation tickets, optional raw context, RawBuffer values, Slot addresses, Drop hooks, and typed allocation outcomes using the selected address and `Usize` width.
- [x] 6.4 Plan distinct zero-byte logical ownership, padded and over-aligned repeated storage, and native/Wasm-specific physical shapes without changing logical cleanup semantics.
- [x] 6.5 Extend layout reachability, verification, deterministic text and byte encoding, malformed fixtures, and allocation-free stability tests.

## 7. Structured MIR and verification

- [x] 7.1 Add MIR operations and structured regions for checked repeated layout, conformance witness dispatch, fallible allocation, unsafe adoption, RawBuffer and lexical Slot operations, initialization transitions, restricted hooks, explicit drop, and automatic field cleanup.
- [x] 7.2 Lower allocation exhaustion through the existing typed outcome shape, cleaning earlier live owners before unchanged `OutOfMemory` propagation and emitting no release for a rejected request.
- [x] 7.3 Lower partial construction rollback, every structured exit, retry attempt cleanup, and explicit early drop inside the existing acyclic MIR region graph.
- [x] 7.4 Verify witness contracts, target layout and type provenance, bounds, loans, affine ticket activity, hook restrictions, hook-before-field order, and exactly-once release while leaving unsafe initializedness as a caller obligation.
- [x] 7.5 Reject forged tickets, mismatched reclaim identities, duplicate release, provider-retention metadata, named scopes, dynamic cleanup registries, and allocator-kind operations before execution.
- [x] 7.6 Extend MIR traversal, substitution, samples, text and byte encoding, determinism tests, and malformed-MIR fixtures for the complete allocation substrate.

## 8. Evaluator allocation oracle

- [x] 8.1 Add deterministic per-run provider-witness, logical block, reclaim-ticket, RawBuffer, Slot, and cleanup-event state without relying on JavaScript object identity or garbage collection.
- [x] 8.2 Execute nominal SystemAllocator and quota-allocator witnesses through the same capability dispatch, ending provider access before any escaped Allocation is dropped.
- [x] 8.3 Implement atomic acquisition and ordinal-based deterministic exhaustion with allocation-free `OutOfMemory`, no rejected ticket, and same-process reuse after every failure.
- [x] 8.4 Evaluate target-planned slot bounds and typed values, initialized-prefix guard mutation, hook-before-field cleanup, explicit early drop, and exact release of zero-sized and nonzero owners.
- [x] 8.5 Add evaluator tests sweeping every allocation and partial-initialization exit, all structured transfers, provider lifetime, custom allocator dispatch, unsafe invariant failures, trap separation, and deterministic traces.

## 9. Native SystemAllocator realization

- [x] 9.1 Add compiler-versioned private aligned acquire and infallible release shim operations with fixed-width caller-owned status/result storage and no retained Silk pointer.
- [x] 9.2 Implement nominal SystemAllocator's native actor operation over the platform primitive and unsafe adoption, preserving the ordinary conformance witness path in HIR and MIR.
- [x] 9.3 Lower compiler-planned Allocation tickets, optional context, RawBuffer addressing, Slot value operations, Drop hooks, and release without ambient allocator lookup or implicit zeroing.
- [x] 9.4 Convert native acquisition exhaustion to typed `OutOfMemory`, including zero-size and over-aligned layouts, and create no release obligation on failure.
- [x] 9.5 Add native IR, bitcode, execution, custom-provider, exhaustion, padded/over-aligned/zero-size, partial rollback, early-exit cleanup, provider-independence, and fresh-process determinism tests.

## 10. Direct Wasm SystemAllocator realization

- [x] 10.1 Extend the compiler-owned Wasm memory plan with non-overlapping static data, private frames, heap metadata, and allocation regions.
- [x] 10.2 Implement checked aligned heap acquisition, deterministic reusable logical release, and checked `memory.grow`, translating growth failure to typed `OutOfMemory`.
- [x] 10.3 Realize the same nominal conformance dispatch, fixed reclaim-ticket shape, RawBuffer/Slot operations, Drop hooks, and exactly-once logical release from verified MIR.
- [x] 10.4 Preserve distinct affine zero-byte owners and post-failure block reuse without promising that physical linear-memory pages shrink.
- [x] 10.5 Add Wasm text/byte, execution, custom-provider, exhaustion, frame/heap isolation, padded/over-aligned/zero-size, rollback, cleanup-order, and fresh-process determinism tests.

## 11. Differential acceptance and unified Labs

- [x] 11.1 Add one canonical construction-guard program using runtime-counted move-only elements and no Vector or collection-specific intrinsic.
- [x] 11.2 Compare evaluator, native, and Wasm across successful transfer, explicit early drop, fallthrough, return, `break`, `continue`, typed failure, and trap separation.
- [x] 11.3 Sweep every allocation and partial-initialization failure ordinal and assert atomic rejection, exact prefix destruction, release once, unchanged failure, and a successful subsequent run.
- [x] 11.4 Add frontend-negative acceptance for missing unsafe boundaries, invalid allocator conformances, layout/type mismatch, Slot escape, live-buffer consumption, duplicate drop, and every prohibited Drop-hook behavior.
- [x] 11.5 Extend fresh-process determinism across syntax, facts, ownership, HIR, instances, layout, MIR, evaluator events, LLVM artifacts, Wasm memory plans, and Wasm artifacts.
- [x] 11.6 Add coordinated successful, exhausted, rollback, early-drop, invalid-hook, and zero-sized presets to unified `/labs` through `Analysis`, with accessible text explaining the no-Scope/no-Arena-privilege boundary.
- [x] 11.7 Update the roadmap after acceptance to mark the substrate complete and promote the separate Silk-written `Vector<T>` and scanner change.

## 12. Repository gates

- [x] 12.1 Run focused compiler, runtime-shim, native, Wasm, language-highlighting, and Labs tests throughout implementation and resolve every in-scope failure.
- [x] 12.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`; report any failure with its exact command and provenance.
- [x] 12.3 Run `pnpm release:candidate` because compiler/runtime contents, language syntax, public artifacts, and package exports change.
> **12.4 finding:** the diff carries one unrelated change — `packages/llvm/src/FunctionBody.ts`
> enriches the `FunctionBody.call` argument-mismatch message with the actual and expected type
> tags. It is behavior-preserving debug ergonomics inherited from the interrupted session that
> started this change, kept because it is what made the witness-dispatch lane mismatch findable.
> No named scopes, retained provider dependencies, allocator-kind branches, ambient or default
> allocation, public `free`, implicit zeroing, runtime initialization bitmaps, collection
> behavior, or general finalizers appear in the diff.

- [x] 12.4 Run strict OpenSpec validation and inspect the final diff for named scopes, retained provider dependencies, allocator-kind branches, ambient/default allocation, public free, implicit zeroing, runtime initialization bitmaps, collection behavior, general finalizers, or unrelated changes.
