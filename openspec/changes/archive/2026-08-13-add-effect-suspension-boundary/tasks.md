## 1. Design Gate and Characterization

- [x] 1.1 Add a reproducible scalar non-tail Effect fixture that records native release, direct Wasm, and raised-limit evaluator behavior before suspension is implemented.
- [x] 1.2 Add separate native release and direct Wasm fixtures for deep `Box` chain build, walk, and drop, ensuring each measured process isolates the selected recursive implementation by using iterative construction or teardown for the other required phases and records success, signal, host exception, or trap with engine versions.
- [x] 1.3 Run and publish the complete pinned-host Box phase matrix, including native limiting boundaries, and add separate local-tracker defects for every limiting ordinary walk, recursive Drop, or borrow-heavy Wasm `unreachable`; do not add those failures to `Effect.suspend` acceptance.
- [x] 1.4 Obtain explicit approval of this proposal, delta specs, allocation policy, evaluator accounting, target-neutral protocol, and native-lowering comparison gate before starting task group 2.
- [x] 1.5 Build a hand-authored frozen, spike-local continuation schema with two reached suspension points, one untaken suspension branch, live affine state, normal completion, typed failure, and harness-only orderly private teardown checkpoints; lower it through both a direct iterative state machine and LLVM switched-resume on LLVM 22.1.8 without adding production MIR or a public LLVM API.
- [x] 1.6 Prove the hard allocation and ownership gates for both spike variants: one complete selected-allocator request per modeled frame-producing relay, none for the untaken branch, full size/alignment visibility, refusal at every ordinal with unchanged `OutOfMemory`, no rejected owner or child event, and no hidden allocation; assert the ordered successful trace closes the allocator loan before publish/child-start and performs a real nested allocator reborrow.
- [x] 1.7 Compare ordered cleanup traces against the direct reference for success, unchanged typed failure, and every refusal ordinal, proving source cleanup precedes exactly-once reclaim; separately prove harness-only defect teardown reclaims raw frames without reporting source cleanup.
- [x] 1.8 Run native depth 1, 1,000, and 100,000 stack sweeps with the no-inline volatile-address watermark and recursive-call-cycle audit, plus O0/O2 structural and benchmark capture; retain exact commands and coroutine pass pipeline, tool versions, host triple/CPU, pre-split and post-coroutine-pass IR, verifier result, linked symbols/disassembly, residual-intrinsic audit, allocator traces, frame bytes, optimized code/data size, raw five-warmup/thirty-sample timing data, and the required source-location/symbolized-checkpoint debug evidence.
- [x] 1.9 Apply the decision thresholds in design decision 5: select an LLVM strategy only if it passes every hard gate, shows at least one material advantage, and has no material regression; measure returned-continuation only for the named layout/control or metric failure, and otherwise select direct lowering. Publish the pass/fail table and remove rejected experimental construction from production LLVM/compiler surfaces.

## 2. Intrinsic and Source Surface

- [x] 2.1 Add exactly one safe generic Effect suspension operation to the sealed intrinsic catalog with evaluator, LLVM, and Wasm availability, widened `OutOfMemory ? &mut Allocator` rows, and inventory/hover/completion verification.
- [x] 2.2 Add the ordinary Silk `Effect.suspend` wrapper with the exact widened public signature, regenerate the embedded standard-library artifact, and verify navigation resolves to source rather than compiler-known spelling.
- [x] 2.3 Add analysis tests proving a same-named user function receives no privilege and that the continuation-storage allocator implementation must have a closed non-suspendable call graph.

## 3. Suspendability and Target-Neutral MIR

- [x] 3.1 Compute deterministic suspendability after concrete instance discovery as a fixed point over direct calls, callable applications, and Effect runner edges; cover self cycles, mutual cycles, and generic `map`/`flatMap` specializations.
- [x] 3.2 Add stable source-derived suspension identities, exact per-runner execution classification, and provisional target-neutral `SuspendEffect` origin plus `RunSuspendableEffect` complete-or-relay control forms to monomorphic MIR, carrying specialized runner, type-argument, typed-outcome, capture, and provider facts while keeping finalized resume identities and continuation descriptors out until MIR-local liveness exists.
- [x] 3.3 Replace the global normalization suspension option with concrete runner facts: preserve established direct and `RunStaticEffect` forms only for proven synchronous runners, retain materialization and suspendable control forms for suspendable or unknown runners, and cover outcome reification, entry closure, and provided generated runners.
- [x] 3.4 Compute backward liveness over specialized post-normalization MIR and classify every post-transfer local, including compiler temporaries, as Copy, exact borrowed dependency, or affine transfer; produce deterministic initialization order, prefix rollback, allocation-refusal, resumed-success, and resumed-failure loan/release plans for every frame-producing relay.
- [x] 3.5 Finalize provisional control into target-neutral suspension regions and continuation descriptors with logical layouts, provider arguments, typed outcome mappings, stable resume identities, and the task 3.4 plans; extend MIR encoding, walking, control edges, local-use accounting, and verification with deterministic repeated-lowering encodings and malformed-MIR tests for missing or duplicate live locals, incompatible access/runner/outcomes, incomplete plans, invalid identities, unfinalized control, and suspension machinery without a reachable suspension origin.

## 4. Continuation Allocation and Evaluation

- [x] 4.1 Plan one validated target layout per continuation, including the private header and compiler-planned live payload, with transactional initialization and prefix rollback before publication.
- [x] 4.2 Route continuation acquisition through the selected `&mut Allocator`, end its exclusive provider loan before child execution, retain only self-contained reclaim authority, and test nested suspension that reborrows the allocator.
- [x] 4.3 Add allocation-failure sweeps over every frame-producing relay ordinal, proving rejected requests create no child execution or owner, every earlier unpublished continuation rolls back and reclaims exactly once, every current source owner cleans exactly once, and `OutOfMemory` is unchanged.
- [x] 4.4 Teach the evaluator activation machine to execute suspension, model continuation allocation/release events, and resume success or typed failure without JavaScript recursion or a source-visible pending value.
- [x] 4.5 Preserve logical `CallDepth`: suspended parents remain active, children add one unit, private helpers add none, and blocked data/traces identify the full suspended source call path deterministically.

## 5. Native and WebAssembly Runners

- [x] 5.1 Implement the native strategy selected by task 1.9 behind the target-neutral descriptors, pin its private LLVM-22 transformation and artifact evidence, and preserve existing synchronous runner ABIs.
- [x] 5.2 Implement native transfer, complete, resume, typed-failure propagation, frame payload cleanup, and reclaim paths with no recursive driver calls, tail-call dependency, exception unwinding, or `setjmp`/`longjmp`.
- [x] 5.3 Prototype and pin the Wasm private step-entry, linear-memory frame header, dispatch identity, and iterative driver protocol without changing synchronous runner ABIs.
- [x] 5.4 Implement Wasm transfer, complete, resume, typed-failure propagation, frame payload cleanup, and reclaim paths with no recursive host calls, JavaScript promise, tail-call dependency, host `RangeError`, or `unreachable` on valid suspended execution.
- [x] 5.5 Add three-engine parity tests for scalar non-tail self-recursion, mutual recursion, state retained after `run`, success, typed failure, allocator exhaustion, allocation/release counts, and cleanup order.

## 6. Composition and Ownership Conformance

- [x] 6.1 Add `Effect.map` conformance tests where suspension precedes mapping and verify the mapper and its captures resume and clean exactly once without changing the source combinator.
- [x] 6.2 Add `Effect.flatMap` conformance tests where the callback selects a suspended Effect and verify unioned failure/requirement rows, callback ownership, and cleanup.
- [x] 6.3 Add suspended outcome-reification, recovery, retry, and provision tests, including provider acquisition and exclusive-provider loan lifetimes across resumption.
- [x] 6.4 Add tracked-owner tests for deep success and injected typed failure, asserting inner-to-outer exactly-once source cleanup, followed by exactly-once private continuation reclamation.
- [x] 6.5 Add trap/defect tests that preserve the no-source-unwind contract and distinguish any orderly private frame reclamation from source Drop observations.

## 7. Pay-for-Use and Acceptance

- [x] 7.1 Strengthen the synchronous Effect cost suite with positive controls and concrete suspendability/MIR assertions: no suspendable instances, suspension operations, continuation descriptors, driver entries, or changes to pinned `RunStaticEffect` verdicts and direct-call shape.
- [x] 7.2 Inspect parsed LLVM IR/bitcode and linked symbols for a pinned non-suspending corpus, proving absence of continuation allocation, driver, resume dispatch, complete-versus-transfer branch, and synchronous entry-shape drift.
- [x] 7.3 Inspect decoded Wasm sections, imports, functions, tables, and instructions for the same corpus, proving absence of continuation allocation, driver, resume dispatch, complete-versus-transfer branch, and synchronous entry-shape drift.
- [x] 7.4 Add release-candidate acceptance for the correct scalar non-tail result at depth 1,000,000 on native release and 100,000 on direct Wasm, plus raised-limit evaluator parity and default deterministic `CallDepth` blockage.
- [x] 7.5 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, `pnpm release:candidate`, and `openspec validate add-effect-suspension-boundary --strict`; report any pre-existing failure exactly before handoff.
