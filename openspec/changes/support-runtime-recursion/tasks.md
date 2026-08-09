## 1. Evaluation limits and outcome model

- [ ] 1.1 Add validated `maxSteps` and `maxCallDepth` evaluation options with defaults of 1,000,000 operations and 1,024 active frames
- [ ] 1.2 Replace `RecursiveCycle` with structured `EvaluationLimit` blocked data for step and call-depth exhaustion
- [ ] 1.3 Update deterministic outcome/trace encoders and all direct consumers for the new reason fields
- [ ] 1.4 Add low-limit tests for direct recursion, mutual recursion, and an infinite loop without relying on host stack overflow or timeouts

## 2. Explicit activation machine

- [ ] 2.1 Define activation records with frame id, function instance, continuation, locals, cells, pending call destination, cleanup state, and depth
- [ ] 2.2 Convert entry and ordinary-call evaluation from recursive host calls to stack pushes and resumable continuations
- [ ] 2.3 Resume callers with complete logical return values and preserve left-to-right argument evaluation and call/binding/return order
- [ ] 2.4 Execute base cases and unwind direct and mutual recursive calls with independent locals for repeated function identities

## 3. Ownership, effects, and cleanup

- [ ] 3.1 Preserve shared and exclusive borrow cell identity across recursive frames and write mutations back to the originating caller
- [ ] 3.2 Execute Drop hooks and recursive cleanup exactly once on recursive return, typed failure, and early control transfer
- [ ] 3.3 Add focused recursive tests for mutable slices, aggregate returns, generic instances, typed failures, and hook-bearing values
- [ ] 3.4 Re-run the non-recursive evaluator corpus to prove the activation-machine refactor preserves existing outcomes and traces

## 4. Trace and inspector integration

- [ ] 4.1 Add frame id and activation depth to activation-specific trace events while retaining canonical function identities
- [ ] 4.2 Replace recursive-cycle corpus and README expectations with terminating recursion and deterministic evaluation-limit expectations
- [ ] 4.3 Add Syntax Inspector presets for completed recursion and call-depth exhaustion
- [ ] 4.4 Render active frames, configured limits, stopping provenance, and accessible recursive trace order without relying on color or indentation

## 5. Native and WebAssembly parity

- [ ] 5.1 Add direct, mutual, generic, and mutable-slice recursion fixtures to the differential driver corpus
- [ ] 5.2 Verify LLVM emits and links recursive calls without acyclic-call-graph assumptions and preserves cleanup/writeback
- [ ] 5.3 Verify direct WebAssembly emits recursive calls without acyclic-call-graph assumptions and preserves cleanup/writeback
- [ ] 5.4 Gate recursive compiler artifacts and traces through fresh-process determinism

## 6. Quicksort graduation

- [ ] 6.1 Execute the existing recursive quicksort source unchanged through evaluation, native, and direct WebAssembly
- [ ] 6.2 Change quicksort to executable status, remove its `RecursiveCycle` blocker, and retain the committed sorted fingerprint
- [ ] 6.3 Update algorithm and evaluator documentation to describe supported runtime recursion and evaluator-only resource limits

## 7. Verification

- [ ] 7.1 Run focused evaluation, ownership, effect, backend, compiler-driver, inspector, and algorithm tests
- [ ] 7.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
