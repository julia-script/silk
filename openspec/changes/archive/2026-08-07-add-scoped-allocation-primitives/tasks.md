## 1. Prerequisite gate

- [ ] 1.1 Verify `add-usize-scalar` is archived and `Usize` arithmetic, literals, conversions, target layouts, calling lanes, evaluator, native, and Wasm behavior are available.
- [ ] 1.2 Verify `add-flow-functions-and-typed-failures` is archived and `OutOfMemory` can use the ordinary flow failure ABI and cleanup propagation.
- [ ] 1.3 Verify `add-capability-requirements-roles-and-provision` is archived and allocator calls can consume canonical hidden service slots and witnesses without allocator-specific dispatch.
- [ ] 1.4 Verify `add-named-scope-wrappers-and-cleanup` is archived and allocation can target established scope identities and dynamic LIFO cleanup records.
- [ ] 1.5 Re-run strict OpenSpec validation and review the archived prerequisite specs for any discovered contract changes before implementation.

## 2. Layout, allocation, slot, and drop-hook surface

- [ ] 2.1 Add lossless syntax for qualified unsafe allocation and slot operations, restricted drop-hook declarations, and explicit consuming drop on top of the established flow/scope grammar.
- [ ] 2.2 Extend syntax traversal and canonical formatting with bounded recovery for missing type arguments, operands, and delimiters.
- [ ] 2.3 Add canonical semantic `Layout`, `SlotLayout<T>`, `Allocation`, and lexical `Slot<T>` types with deterministic equality, ordering, substitution, traversal, encoding, and borrow/resource properties.
- [ ] 2.4 Add focused parser, formatter, and type tests covering roles, scope wrappers, generic slots, damaged operations, and rejected safe slot construction.

## 3. Layout and allocation semantics

- [ ] 3.1 Elaborate validated `Layout` construction over `Usize`, accepting zero size and valid over-alignment while returning ordinary invalid-alignment data.
- [ ] 3.2 Elaborate `SlotLayout<T>` from the selected concrete element layout and runtime count with aligned stride and checked `Usize` multiplication.
- [ ] 3.3 Resolve unsafe allocation through the established allocator capability-role slot and active destination scope, retaining typed `OutOfMemory` and explicit unavailable facts.
- [ ] 3.4 Enforce provider-outlives-destination-scope from actual provision and scope-wrapper order, including ancestor scopes and rejected inner providers.
- [ ] 3.5 Elaborate unsafe checked slot selection and value operations with exact allocation/layout/type provenance and no safe escape.
- [ ] 3.6 Add semantic tests for invalid alignment, overflow, missing providers, inactive scopes, provider lifetime, mismatched slot layouts, zero-sized counts, and unavailable downstream facts.

## 4. Affine ownership and restricted cleanup hooks

- [ ] 4.1 Classify `Allocation` as an affine byte owner whose moves transfer one stable cleanup obligation and whose explicit drop consumes it.
- [ ] 4.2 Track lexical exclusive slot loans and reject allocation move, drop, release, or escaping slot storage while a loan is live.
- [ ] 4.3 Validate one restricted drop hook per affine struct: infallible, non-allocating, requirement-free, no move or replacement from `self`, no escaping borrow, and no manual invocation.
- [ ] 4.4 Invoke each accepted drop hook once before recursive declaration-ordered field cleanup on return, typed failure, early return, arm exit, loop repeat/exit, and fallthrough paths that actually leave its region.
- [ ] 4.5 Model explicit release as disarming the stable scope record so later scope closure skips it; reject double consumption and cleanup under live loans.
- [ ] 4.6 Add ownership tests for moves, early drop, provider lifetime, slot loans, restricted-hook rejection matrix, hook-before-field order, loop region boundaries, and zero-sized allocations.

## 5. HIR and instance discovery

- [ ] 5.1 Extend HIR with layout formation, fallible allocation, destination scope and origin relationship, slot selection/access, restricted drop-hook declarations and calls, explicit drop, and cleanup provenance.
- [ ] 5.2 Keep reclaim control blocks and witnesses compiler-private while retaining deterministic logical allocation and scope identities in inspection encodings.
- [ ] 5.3 Extend HIR traversal, verification, text/debug projections, unavailable-state handling, and source-provenance tests for every new operation.
- [ ] 5.4 Extend instance discovery through `SlotLayout<T>`, slot operations, drop hooks, and cleanup behavior while excluding runtime counts, scopes, providers, and allocation ordinals from instance keys.
- [ ] 5.5 Prove one generic typed-slot helper instance is reused across different counts, scopes, and provider implementations for the same canonical `T`.

## 6. Target-aware allocation layouts and calling shapes

- [ ] 6.1 Add compiler-owned `Layout`, `SlotLayout<T>`, affine handle, private control-block, and lexical-slot representation facts using the selected target's `Usize` and address width.
- [ ] 6.2 Reuse the prerequisite failure and hidden service-slot calling shapes for allocation success/failure and allocator witness dispatch.
- [ ] 6.3 Plan zero-sized logical allocation identity, padded aggregate stride, bounds operands, cleanup witness lanes, and stable registration identity before MIR lowering.
- [ ] 6.4 Extend layout reachability, structural keys, verification, deterministic text, and byte encodings without eagerly perturbing allocation-free programs.
- [ ] 6.5 Add target-layout tests for all native profiles and Wasm, overflow, over-alignment, padded aggregates, zero-sized elements, malformed shapes, and byte stability outside allocation paths.

## 7. Structured MIR and verification

- [ ] 7.1 Add DAG operations for checked layout results, fallible allocation, stable cleanup registration, unsafe lexical slot places, slot value operations, restricted drop-hook calls, release/disarm, and scope close.
- [ ] 7.2 Lower allocation failure through the prerequisite explicit success/failure outcome shape and emit cleanup before propagation from every exited region.
- [ ] 7.3 Lower explicit and automatic allocation cleanup through one active/disarmed control-block authority without control back-edges.
- [ ] 7.4 Verify layout/type/provenance agreement, checked bounds, slot non-escape, live allocation ownership, hook restrictions, original provider witness, and exactly-once release/disarm.
- [ ] 7.5 Explicitly leave runtime indexed initializedness and aliasing as unsafe contracts rather than verifier claims or runtime bitmap requirements.
- [ ] 7.6 Extend MIR traversal, substitution, deterministic encoding/text, samples, and malformed fixtures for allocation, slot, hook, failure, and cleanup DAGs.

## 8. Evaluator semantic oracle

- [ ] 8.1 Add per-run deterministic provider, allocation, control-block, cleanup-record, and logical slot identities without relying on JavaScript object identity or garbage collection.
- [ ] 8.2 Implement successful acquisition as one atomic allocation-plus-registration transition and implement ordinal-based deterministic failure with no record or live storage.
- [ ] 8.3 Evaluate checked slot addressing and logical value operations at compiler-planned strides, retaining unsafe invariant violations as explicit test-oracle states rather than a promised production bitmap.
- [ ] 8.4 Evaluate restricted drop hooks before field cleanup, explicit release/disarm, reverse-order scope fallback, and region-correct loop transfers.
- [ ] 8.5 Add evaluator tests for success, every allocation and partial-construction failure ordinal, same-process reuse, original-provider reclaim, explicit-drop fallback skipping, padded and zero-sized slots, and hook-before-byte-release order.

## 9. Native system allocation

- [ ] 9.1 Add compiler-versioned native shim operations for aligned system allocation and infallible release with a private fixed-width ABI and typed OOM status.
- [ ] 9.2 Lower allocator service-witness calls through the prerequisite hidden slots and consume compiler-planned `Usize`, address, layout, failure, handle, and control-block shapes.
- [ ] 9.3 Materialize stable cleanup records and original-provider reclaim witnesses, and lower release/disarm and scope close exactly once without ambient allocator lookup.
- [ ] 9.4 Lower aligned typed-slot addressing and move-only value operations without implicit zeroing or backend-selected stride.
- [ ] 9.5 Add native IR, execution, failure-injection, padded/over-aligned/zero-sized slot, hook ordering, early-exit cleanup, provider-origin, and determinism tests.

## 10. Direct Wasm allocation

- [ ] 10.1 Partition linear memory deterministically between static data, private slice frames, allocation heap state, and cleanup records without overlap.
- [ ] 10.2 Implement checked aligned heap acquisition and growth, converting `memory.grow` failure to typed `OutOfMemory` rather than trap and registering cleanup only on success.
- [ ] 10.3 Realize allocator witnesses, stable control blocks, original-provider reclaim, active/disarmed records, and reverse-order scope close using compiler-planned shapes.
- [ ] 10.4 Lower typed-slot addressing and move-only value operations at the compiler-selected stride while preserving zero-sized logical identities.
- [ ] 10.5 Add Wasm text/byte, execution, exhaustion, nested-frame/heap isolation, padded/over-aligned/zero-sized slot, hook ordering, early-exit cleanup, and determinism tests.

## 11. Three-engine and unified-inspector acceptance

- [ ] 11.1 Add a canonical affine construction guard over runtime-counted move-only slots whose restricted hook drops its initialized prefix before byte release.
- [ ] 11.2 Exercise successful completion, typed failure, early return, `break`, and `continue`, proving cleanup only for exited regions and exact reverse acquisition order across evaluator, native, and Wasm.
- [ ] 11.3 Sweep every allocation and partial-initialization failure ordinal, assert no record for rejected allocation, hook rollback before release, no successful artifact, and same-process successful reuse.
- [ ] 11.4 Add negative fixtures for safe slot access, layout mismatch, unchecked bounds, slot escape, allocation consumption under loan, provider-underlives-scope, duplicate drop, and every restricted-hook prohibition.
- [ ] 11.5 Extend fresh-process determinism coverage across syntax, semantic facts, ownership, HIR, instances, layout, MIR, evaluator events, native artifacts, Wasm memory plans, and Wasm artifacts.
- [ ] 11.6 Add successful and exhaustion presets to unified `/labs` and coordinate allocation, scope, provider, hook, layout, MIR, storage, failure, and release identities across projections.
- [ ] 11.7 Update the project roadmap to mark prerequisites and scoped allocation complete and promote Silk-written `Vector<T>` as the next memory change.

## 12. Repository gates

- [ ] 12.1 Run focused compiler, shim, and workbench tests throughout implementation and resolve every in-scope failure.
- [ ] 12.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`; report any failure with its exact command and provenance.
- [ ] 12.3 Run `pnpm release:candidate` because compiler/runtime package contents and public types change, and resolve every in-scope packaging or export failure.
- [ ] 12.4 Run strict OpenSpec validation and inspect the final diff for allocator-specific contract machinery, backend-owned layouts, ambient/default-static allocators, public free, implicit zeroing, collection behavior, runtime initialization bitmap, general finalizers, arena policy, metrics, or unrelated changes.
