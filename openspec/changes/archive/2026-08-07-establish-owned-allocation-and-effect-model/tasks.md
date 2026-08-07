## 1. Baseline and migration inventory

- [x] 1.1 Run the existing Flow, ownership, layout, MIR, evaluator, native, Wasm, formatter, highlighting, and labs tests and record any pre-existing failures.
  - Baseline 2026-08-07: `pnpm test` passed all 16 Turbo tasks. The docs build retained its existing 28 TypeDoc visibility warnings and reported no errors.
- [x] 1.2 Enumerate every public Flow token, syntax node, semantic/HIR/MIR value, actor operation, fixture, export, preset, and documentation reference that must break to Effect.
- [x] 1.3 Add rejected compatibility fixtures proving `flow fn` and `Flow.*` no longer remain as hidden aliases after migration.
- [x] 1.4 Add an architecture assertion that compiler artifacts contain no named allocation scope, dynamic finalizer record, provider dependency, or allocator-kind tag.

## 2. Effect lexical and concrete syntax

- [x] 2.1 Replace the `flow` keyword token with `effect` across the bootstrap lexer, token encodings, formatter vocabulary, and recovery sets.
- [x] 2.2 Rename Flow function syntax nodes and facts to Effect function equivalents without preserving compatibility aliases.
- [x] 2.3 Parse `effect { ... }` as a primary lazy imperative expression with bounded damaged-block recovery.
- [x] 2.4 Parse and format `effect fn` as whole-body Effect sugar while preserving explicit `run` and `return`.
- [x] 2.5 Rename `Flow.map`, `Flow.flatMap`, `Flow.flatten`, `Flow.catch`, `Flow.tap`, and `Flow.suspend` syntax fixtures to `Effect.*`.
- [x] 2.6 Add `Effect.retry` examples and parsing fixtures, including pipeline insertion.
- [x] 2.7 Accept `fail value` for Copy payloads while retaining `fail move value` for explicit affine transfer.
- [x] 2.8 Update syntax traversal, canonical encoding, formatter goldens, damaged-source fixtures, and keyword parity tests for the new surface.
- [x] 2.9 Resolve explicit `Effect<A ! E ? R>` contract syntax in ordinary function signatures, including empty failure and requirement rows.

## 3. Effect semantic contracts and captures

- [x] 3.1 Rename the canonical Flow success/failure/requirement model and public analysis facts to Effect.
- [x] 3.2 Elaborate `effect {}` with an eager construction boundary and a lazy body contract.
- [x] 3.3 Elaborate `effect fn` through the same Effect expression representation rather than a second execution model.
- [x] 3.4 Infer and validate Copy, shared, exclusive, and moved captures in deterministic canonical order.
- [x] 3.5 Derive shared-repeatable, exclusive-repeatable, and take-once execution access from captures.
- [x] 3.6 Reject second execution after a capture is consumed and report the originating capture span.
- [x] 3.7 Require `Effect.retry` inputs to be repeatable and publish whether locals reset, captures persist, and providers reacquire.
- [x] 3.8 Enforce detached failure payloads: owned self-contained values are legal; lexical/provider borrows are rejected; OutOfMemory construction is allocation-free.
- [x] 3.9 Update exact-member `Effect.catch` subtraction, handler typing, Copy/affine payload transfer, and residual-row diagnostics.
- [x] 3.10 Give every Effect construction site a hidden nominal identity and target-planned capture environment; reject joins that would erase distinct identities.

## 4. Effect provision, HIR, ownership, and execution

- [x] 4.1 Rename Flow HIR, instance-discovery, target-outcome, MIR, evaluator, backend, and facade vocabulary to Effect.
- [x] 4.2 Represent existing-provider capture separately from per-run `provideWith` acquisition in semantic facts and HIR.
- [x] 4.3 Plan moved-provider ownership with the Effect value and borrowed-provider lexical access without inventing a cleanup boundary.
- [ ] 4.4 Plan `provideWith` acquisition and reverse-order Drop for success and typed failure while preserving the original outcome.
- [x] 4.5 Lower repeatable retry into structured attempt regions that reconstruct locals and retain capture state.
- [x] 4.6 Reject retry of consuming Effects before MIR and omit unusable runtime instances.
- [ ] 4.7 Update the evaluator for lazy Effect entry, persistent exclusive captures, take-once rejection, catch, retry, and provider placement.
- [x] 4.8 Update native and Wasm Effect outcome lowering from the compiler-selected target plan without runtime Effect dispatch.
- [ ] 4.9 Add evaluator/native/Wasm parity fixtures for eager setup, delayed body, pipeline catch, mutable retry counters, consuming retry rejection, and per-run provider cleanup.

## 5. Layout and self-contained allocation surface

- [x] 5.1 Add source and canonical semantic `Layout` with target-sized bytes, validated alignment, checked repetition, and ordinary invalid/overflow results.
- [x] 5.2 Add the nominal `OutOfMemory` failure and verify its construction performs no allocation.
- [ ] 5.3 Define the Allocator capability operation with exclusive MVP access, validated Layout input, and self-contained affine Allocation output.
- [x] 5.4 Add canonical `Allocation` ownership facts and a private active reclaim ticket that does not borrow or rediscover the provider.
- [x] 5.5 End the allocator access loan when allocation returns while preserving the Allocation owner's validity.
- [ ] 5.6 Add a deterministic failing allocator fixture that delegates successful allocations to the same self-contained contract.
- [ ] 5.7 Reject any safe or internal representation that records a named destination scope, allocator implementation tag, or retained provider dependency.
- [ ] 5.8 Add syntax, semantic, ownership, and encoding tests for zero-byte, over-aligned, overflowing, exhausted, moved, explicitly dropped, and lexically dropped allocations.

## 6. Restricted Drop and raw typed storage

- [x] 6.1 Add explicit consuming `drop value` through syntax, semantic analysis, ownership, HIR, MIR, evaluator, and formatting.
- [ ] 6.2 Add one restricted Drop declaration for affine nominal types and reject Copy types or duplicate hooks.
- [ ] 6.3 Reject Drop hooks that fail, allocate, require services, move or replace fields, retain self, or invoke themselves manually.
- [ ] 6.4 Insert Drop before fixed declaration-order field cleanup on fallthrough, return, break, continue, typed failure, and explicit early drop.
- [ ] 6.5 Preserve trap semantics by emitting no claim or trace that normal Drop unwinding completed after a trap.
- [ ] 6.6 Add typed `RawBuffer<T>` construction over Allocation and compiler-selected repeated-element layout.
- [ ] 6.7 Add lexical unsafe uninitialized-slot selection with provenance, bounds, type, and backing-owner liveness checks.
- [ ] 6.8 Add the minimum unsafe slot write, move/read, drop-in-place, and property-gated copy/move operations.
- [ ] 6.9 Model initialized-prefix rollback with ordinary affine guards rather than a hidden runtime bitmap.
- [ ] 6.10 Add negative fixtures for out-of-bounds slots, mismatched layouts, aliasing/move conflicts visible to ownership, uninitialized reads, and duplicate cleanup.

## 7. Target layout, structured MIR, and evaluator allocation oracle

- [ ] 7.1 Extend instance discovery for reachable Layout, Allocation, RawBuffer, Drop hooks, allocator witnesses, and concrete element types.
- [ ] 7.2 Extend target planning with byte/stride/total layouts, reclaim-ticket shape, raw-buffer lanes, Drop calling shapes, and Effect outcome transport.
- [ ] 7.3 Keep HIR target-independent and consume exactly one selected target layout when lowering allocation and cleanup to MIR.
- [ ] 7.4 Add MIR operations and verifier rules for validated allocation, typed exhaustion, slot access, initialization, commit/rollback, explicit drop, and automatic Drop.
- [ ] 7.5 Preserve all new control as regions in the acyclic MIR DAG, including retry attempt and cleanup regions.
- [ ] 7.6 Extend deterministic MIR encoding and malformed-MIR fixtures for allocation identity, provenance, initializedness, and cleanup order.
- [ ] 7.7 Model logical heap blocks, reclaim tickets, slots, initialized prefixes, and ordered Drop events in the evaluator without JavaScript object identity or GC.
- [ ] 7.8 Sweep every allocation and initialization failure ordinal and verify a fresh execution can reuse the evaluator afterward.

## 8. Native and Wasm SystemAllocator realization

- [ ] 8.1 Add compiler-versioned aligned allocate/release shim operations with caller-owned status/result storage and no retained Silk pointers.
- [ ] 8.2 Implement the native SystemAllocator witness and self-contained reclaim ticket over the stable shim release entry.
- [ ] 8.3 Lower native Allocation, raw-buffer addressing, initialized element moves, Drop hooks, and release from the target plan.
- [ ] 8.4 Define the Wasm module heap region without colliding with static data, stack frames, or existing runtime slices.
- [ ] 8.5 Implement checked Wasm allocation, reusable logical release, and `memory.grow` exhaustion as typed OutOfMemory.
- [ ] 8.6 Lower Wasm raw-buffer addressing and Drop from compiler-planned strides and lanes without an allocator-kind branch.
- [ ] 8.7 Add native and Wasm zero-size, alignment, exhaustion, explicit-drop, typed-failure cleanup, and post-failure reuse fixtures.
- [ ] 8.8 Verify LLVM IR, bitcode, WAT, Wasm bytes, symbols, and runtime traces are deterministic across fresh processes.

## 9. Silk standard-library Vector

- [ ] 9.1 Implement `Vector<T>` in Silk with RawBuffer, length, capacity, and a restricted Drop hook that destroys initialized elements first.
- [ ] 9.2 Implement empty construction and shared/exclusive slice views without storing an allocator.
- [ ] 9.3 Implement checked capacity growth and reserve with explicit Allocator requirement and transactional commit.
- [ ] 9.4 Implement append for Copy and affine elements with exact OutOfMemory rollback.
- [ ] 9.5 Implement pop and truncate with correct ownership transfer and element Drop.
- [ ] 9.6 Implement insert and remove with overlap-safe element movement and rollback where growth may fail.
- [ ] 9.7 Add generic Vector tests for scalars, move-only structs, nested vectors, zero-sized values, and Drop-bearing elements.
- [ ] 9.8 Add exhaustive failing-allocator tests proving original-state preservation, initialized-prefix cleanup, no leaks, and no double drops.
- [ ] 9.9 Add the compiler milestone fixture that tokenizes borrowed runtime bytes into an owned growable `Vector<Token>` and returns it from an Effect.

## 10. Unified labs, documentation, and migration cleanup

- [x] 10.1 Replace Flow vocabulary with Effect across package exports, diagnostics, examples, compiler docs, and public API documentation without aliases.
- [x] 10.2 Update TextMate, CodeMirror, generated editor grammars, Cursor assets, and docs highlighting for `effect` keyword parity and Flow removal.
- [x] 10.3 Add unified `/labs` presets for eager-versus-lazy Effect, capture reuse, pipeline catch, retry, and provider placement.
- [ ] 10.4 Add unified `/labs` presets for Layout, allocation success/exhaustion, raw initialized storage, Vector growth, rollback, and Drop.
- [ ] 10.5 Link syntax, facts, HIR, ownership, target layout, MIR, evaluator, LLVM, and Wasm panes through the analysis facade with accessible text equivalents.
- [ ] 10.6 Remove or migrate every legacy Scope/Flow allocation preset and assert no lab imports compiler phase internals.
- [ ] 10.7 Update package exports and release documentation for any new public compiler models or standard-library artifacts.

## 11. Vertical acceptance and release checks

- [ ] 11.1 Run focused unit and integration tests after each gate: Effect rename, Effect semantics, allocation/Drop, backends, and Vector.
- [ ] 11.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in repository order and resolve every change-caused failure.
- [ ] 11.3 Run `pnpm check` and report any exact pre-existing failure separately from this change.
- [ ] 11.4 Run `pnpm release:candidate` because compiler/package contents and exports change.
- [ ] 11.5 Run strict OpenSpec validation and confirm every new/modified capability has acceptance coverage.
- [ ] 11.6 Search active Wayfinder, OpenSpec, compiler, docs, fixtures, and labs for stale normative Flow, named Scope, dependency-set, and Arena-special-case language.
- [ ] 11.7 Verify the owned-token milestone across evaluator, native LLVM, and direct Wasm with deterministic artifacts and failure sweeps.
