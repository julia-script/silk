## 1. Pin the Corrected Contract

- [ ] 1.1 Add elaboration and intrinsic-catalog tests proving `Effect.suspend<A, E, ?R>` preserves exactly `A ! E ? R`, uses ordinary failure type parameter `E`, and contributes neither `OutOfMemory` nor `Allocator`.
- [ ] 1.2 Add source-composition tests proving nested Effect success remains nested and `map`, `flatMap`, recovery, retry, provision, and an equivalent user-defined combinator add no suspension-specific channels.
- [ ] 1.3 Add structural red tests for one reusable frame per concrete invocation, distinct resume states, no continuation allocator argument, and no source allocation operation on a suspension path.
- [ ] 1.4 Add ownership red tests for an affine owner, shared borrow, exclusive borrow, and exclusive provider access retained across suspension and private execution-stack growth.
- [ ] 1.5 Add red evaluator and backend tests for the every-recursive-cycle criterion, valid uncovered and unrelated recursion, logical `CallDepth`, fatal evaluator and compiled execution-storage exhaustion outside the failure channel, and the absence of allocator request/refusal/release traces.

## 2. Remove the Source Allocation Contract

- [ ] 2.1 Change the sealed suspension intrinsic callable contract to preserve the deferred child's channels exactly and regenerate intrinsic inventory expectations.
- [ ] 2.2 Change shipped `Effect.suspend` to `A ! E ? R` and update its source documentation without recognizing the wrapper by name.
- [ ] 2.3 Remove `Instances.continuationAllocatorViolations`, `SEM0102`, its diagnostic reason/rendering/catalog entries, and all allocator-bootstrap selection logic.
- [ ] 2.4 Migrate compiler fixtures and ordinary Silk examples to remove `OutOfMemory` recovery and allocator provision used solely for suspension while retaining both for real source allocations.
- [ ] 2.5 Prove that an ordinary `Allocator` implementation may suspend under its declared contract now that it is never selected for coroutine storage.

## 3. Replace Continuation Records with Coroutine Frame States

- [ ] 3.1 Introduce target-neutral MIR types for one coroutine frame descriptor per specialized suspendable runner, stable state identities, state-local fields, and one maximum logical layout.
- [ ] 3.2 Refactor `SuspensionMir` to stop discovering allocator providers and instead attach origins and relays to deterministic per-invocation resume states after normalization.
- [ ] 3.3 Replace `ContinuationLayout` with a coroutine-frame planning actor that aggregates mutually exclusive state payloads, computes deterministic maximum layouts, and preserves compiler-generated temporaries.
- [ ] 3.4 Refactor `SuspensionOwnership` to assign each affine value to one frame state, preserve stable loan/provider roots, and remove allocator locals and reclaim obligations.
- [ ] 3.5 Rewrite MIR encoding and verification for state completeness, maximum-frame consistency, exact typed outcomes, unique ownership, stable loans, and rejection of storage-channel contamination.
- [ ] 3.6 Delete `ContinuationTransaction`, allocation-prefix publication/rollback plans, captured reclaim authority, and their dedicated tests with no compatibility actor or aliases remaining.

## 4. Implement the Private Execution Stack

- [ ] 4.1 Define one private variable-sized frame-stack protocol shared semantically by evaluation, native, and Wasm: push invocation, transition state, resume top, complete/pop, and fatal growth failure.
- [ ] 4.2 Implement non-moving segmented native execution-stack storage with stable active-frame addresses, private growth/release support, and fatal failure routing.
- [ ] 4.3 Implement non-moving direct-Wasm execution-stack storage using non-overlapping private linear-memory regions and fatal `memory.grow` failure routing.
- [ ] 4.4 Add target-level tests proving active frame addresses and retained borrows remain stable across segment growth and that mixed source allocation cannot overlap private segments.
- [ ] 4.5 Add internal storage tests proving variable frame sizes, alignment, segment boundaries, LIFO release, reuse of one invocation frame across states, and fatal growth failure without typed cleanup.

## 5. Adapt Evaluation

- [ ] 5.1 Replace evaluator continuation allocation and transaction state with reusable coroutine-frame activation state and iterative child push/resume/pop control.
- [ ] 5.2 Remove scripted continuation allocation failure, allocator calls, allocation tickets, reclaim logic, and `ContinuationRequest`/`ContinuationRelease` events.
- [ ] 5.3 Add deterministic frame-push, state-transition, resume, and frame-completion trace events and update every compiler API consumer atomically without old-event aliases.
- [ ] 5.4 Preserve one source-logical `CallDepth` unit per suspended invocation, no depth for driver helpers, deterministic boundary blockage, and raised-limit deep completion.
- [ ] 5.5 Verify evaluator success, typed failure, retained owners, stable borrows/providers, cleanup order, mutual recursion, repeated suspension, and stored Effect behavior.

## 6. Adapt Native and WebAssembly Drivers

- [ ] 6.1 Refactor native suspension lowering to enter one frame per invocation, transition/reuse it across suspension points, push children iteratively, and pop on success or typed failure.
- [ ] 6.2 Refactor direct-Wasm suspension lowering to implement the identical logical frame protocol without recursive host calls or source allocator operations.
- [ ] 6.3 Remove continuation allocation/reclaim ABI fields, refusal branches, rollback paths, source allocator calls, and obsolete runtime symbols from both backends.
- [ ] 6.4 Prove native and Wasm parity for non-tail self recursion, mutual recursion, ordinary combinators, typed failure, retained affine owners, cross-suspension borrows, providers, and exact cleanup.
- [ ] 6.5 Re-run deep native and Wasm stack-watermark acceptance and prove repeated suspension by one invocation reuses its frame structurally.
- [ ] 6.6 Add bounded private execution-stack exhaustion fixtures proving both compiled targets trap without constructing a typed failure or running source unwind cleanup.

## 7. Preserve Storage, Composition, and Pay for Use

- [ ] 7.1 Update stored nominal Effect layout, invalidation, engine-parity, run, and drop tests for the new suspendable runner and frame descriptors.
- [ ] 7.2 Update callable-field, generic specialization, user-service, requirement-provision, and static-composition fixtures to preserve exact channels and frame states.
- [ ] 7.3 Strengthen native and Wasm artifact audits so closed non-suspending graphs contain no frame descriptor, execution-stack helper, suspension driver, pending branch, or concurrency linkage.
- [ ] 7.4 Add a repository-wide ghost-path check proving `SEM0102`, continuation allocator arguments, allocation trace events, transaction actors, and suspension-only row widening are absent.
- [ ] 7.5 Delete or rewrite allocator-refusal, request-order, reclaim-authority, per-relay-allocation, and partial-rollback fixtures rather than weakening their assertions.

## 8. Update User-Facing Truth

- [ ] 8.1 Rewrite Labs suspension presets as simple exact-channel programs with no allocator provision or OOM recovery and update their trace/MIR assertions.
- [ ] 8.2 Update Effect API examples and canonical language reference sections for explicit stack-safe recursion, reusable frames, fatal execution-storage exhaustion, logical `CallDepth`, ownership, diagnostics, and the non-async boundary.
- [ ] 8.3 Update generated diagnostic and intrinsic documentation after obsolete allocation surface is deleted.
- [ ] 8.4 Verify every shipped example and documentation snippet through the compiler or its designated snippet test.

## 9. Validate and Reconcile

- [ ] 9.1 Run focused suspension, ownership, stored-Effect, evaluator, native, Wasm, intrinsic, diagnostics, Labs, and artifact-architecture suites.
- [ ] 9.2 Run `pnpm typecheck`, then `pnpm exec biome check .`, then `pnpm test`, and resolve every change-related failure.
- [ ] 9.3 Run `pnpm check` and `pnpm release:candidate`, reporting any proven unrelated baseline failure exactly.
- [ ] 9.4 Run strict OpenSpec validation and audit the implementation against SLP-0009 and every delta requirement before sync or archive.
- [ ] 9.5 Confirm the final worktree contains no compatibility shim, obsolete continuation allocation path, stale allocator-only documentation, or unchecked required task.
