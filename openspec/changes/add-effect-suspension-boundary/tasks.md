## 1. Design Gate and Characterization

- [x] 1.1 Add a reproducible scalar non-tail Effect fixture that records native release, direct Wasm, and raised-limit evaluator behavior before suspension is implemented.
- [x] 1.2 Add separate native release and direct Wasm fixtures for deep `Box` chain build, walk, and drop, ensuring each measured process excludes the other two phases and records success, signal, host exception, or trap with engine versions.
- [x] 1.3 Run the Box phase matrix, publish the results with the fixture, and open separate defects for every limiting ordinary walk, recursive Drop, or borrow-heavy Wasm `unreachable`; do not add those failures to `Effect.suspend` acceptance.
- [ ] 1.4 Obtain explicit approval of this proposal, delta specs, allocation policy, evaluator accounting, and private-runner design before starting task group 2.

## 2. Intrinsic and Source Surface

- [ ] 2.1 Add exactly one safe generic Effect suspension operation to the sealed intrinsic catalog with evaluator, LLVM, and Wasm availability, widened `OutOfMemory ? &mut Allocator` rows, and inventory/hover/completion verification.
- [ ] 2.2 Add the ordinary Silk `Effect.suspend` wrapper with the exact widened public signature, regenerate the embedded standard-library artifact, and verify navigation resolves to source rather than compiler-known spelling.
- [ ] 2.3 Add analysis tests proving a same-named user function receives no privilege and that the continuation-storage allocator implementation must have a closed non-suspendable call graph.

## 3. Suspendability and Target-Neutral MIR

- [ ] 3.1 Compute deterministic suspendability after concrete instance discovery as a fixed point over direct calls, callable applications, and Effect runner edges; cover self cycles, mutual cycles, and generic `map`/`flatMap` specializations.
- [ ] 3.2 Extend ownership liveness at explicit suspension points to classify every post-resume local as Copy, borrowed dependency, or affine transfer and to produce initialization rollback plus success/failure cleanup plans.
- [ ] 3.3 Add target-neutral MIR suspendable-run operations, suspension points, stable resume identities, continuation descriptors, logical layouts, typed outcome mappings, provider arguments, loan endings, and releases.
- [ ] 3.4 Update MIR normalization so `RunStaticEffect` remains eligible only for proven non-suspendable runners and unknown/suspendable candidates retain the appropriate run representation without a global suspension mode.
- [ ] 3.5 Extend MIR encoding, walking, local-use accounting, and verification; add deterministic round-trip and malformed-MIR tests for missing live owners, incompatible runners/outcomes, invalid resumes, and suspension machinery without reachable suspension.

## 4. Continuation Allocation and Evaluation

- [ ] 4.1 Plan one validated target layout per continuation, including the private header and compiler-planned live payload, with transactional initialization and prefix rollback before publication.
- [ ] 4.2 Route continuation acquisition through the selected `&mut Allocator`, end its exclusive provider loan before child execution, retain only self-contained reclaim authority, and test nested suspension that reborrows the allocator.
- [ ] 4.3 Add allocation-failure sweep tests proving rejected requests create no child execution or owner and every previously initialized continuation and source owner cleans exactly once without replacing `OutOfMemory`.
- [ ] 4.4 Teach the evaluator activation machine to execute suspension, model continuation allocation/release events, and resume success or typed failure without JavaScript recursion or a source-visible pending value.
- [ ] 4.5 Preserve logical `CallDepth`: suspended parents remain active, children add one unit, private helpers add none, and blocked data/traces identify the full suspended source call path deterministically.

## 5. Native and WebAssembly Runners

- [ ] 5.1 Prototype and pin the LLVM private step-entry, continuation-header, and iterative driver ABI against the target-neutral descriptors without changing synchronous runner ABIs.
- [ ] 5.2 Implement LLVM transfer, complete, resume, typed-failure propagation, frame payload cleanup, and reclaim paths with no recursive driver calls, tail-call dependency, exception unwinding, or `setjmp`/`longjmp`.
- [ ] 5.3 Prototype and pin the Wasm private step-entry, linear-memory frame header, dispatch identity, and iterative driver protocol without changing synchronous runner ABIs.
- [ ] 5.4 Implement Wasm transfer, complete, resume, typed-failure propagation, frame payload cleanup, and reclaim paths with no recursive host calls, JavaScript promise, tail-call dependency, host `RangeError`, or `unreachable` on valid suspended execution.
- [ ] 5.5 Add three-engine parity tests for scalar non-tail self-recursion, mutual recursion, state retained after `run`, success, typed failure, allocator exhaustion, allocation/release counts, and cleanup order.

## 6. Composition and Ownership Conformance

- [ ] 6.1 Add `Effect.map` conformance tests where suspension precedes mapping and verify the mapper and its captures resume and clean exactly once without changing the source combinator.
- [ ] 6.2 Add `Effect.flatMap` conformance tests where the callback selects a suspended Effect and verify unioned failure/requirement rows, callback ownership, and cleanup.
- [ ] 6.3 Add suspended outcome-reification, recovery, retry, and provision tests, including provider acquisition and exclusive-provider loan lifetimes across resumption.
- [ ] 6.4 Add tracked-owner tests for deep success and injected typed failure, asserting inner-to-outer exactly-once source cleanup, followed by exactly-once private continuation reclamation.
- [ ] 6.5 Add trap/defect tests that preserve the no-source-unwind contract and distinguish any orderly private frame reclamation from source Drop observations.

## 7. Pay-for-Use and Acceptance

- [ ] 7.1 Strengthen the synchronous Effect cost suite with positive controls and concrete suspendability/MIR assertions: no suspendable instances, suspension operations, continuation descriptors, driver entries, or changes to pinned `RunStaticEffect` verdicts and direct-call shape.
- [ ] 7.2 Inspect parsed LLVM IR/bitcode and linked symbols for a pinned non-suspending corpus, proving absence of continuation allocation, driver, resume dispatch, complete-versus-transfer branch, and synchronous entry-shape drift.
- [ ] 7.3 Inspect decoded Wasm sections, imports, functions, tables, and instructions for the same corpus, proving absence of continuation allocation, driver, resume dispatch, complete-versus-transfer branch, and synchronous entry-shape drift.
- [ ] 7.4 Add release-candidate acceptance for the correct scalar non-tail result at depth 1,000,000 on native release and 100,000 on direct Wasm, plus raised-limit evaluator parity and default deterministic `CallDepth` blockage.
- [ ] 7.5 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, `pnpm release:candidate`, and `openspec validate add-effect-suspension-boundary --strict`; report any pre-existing failure exactly before handoff.
