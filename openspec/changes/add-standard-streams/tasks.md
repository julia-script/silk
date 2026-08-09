## 1. Service Contract

- [ ] 1.1 Define `StandardStreams`, destinations, typed write failure, and `writeAll` over immutable bytes.
- [ ] 1.2 Propagate the explicit requirement through analysis, HIR, instances, and missing-provider diagnostics.

## 2. MIR and Evaluation

- [ ] 2.1 Add target-neutral ordered byte-write MIR, typed outcome, verification, and encoding.
- [ ] 2.2 Add evaluator provider and in-memory implementation with deterministic write/failure events.

## 3. Hosts

- [ ] 3.1 Add the native process-stream adapter and LLVM/runtime lowering with all-or-failure semantics.
- [ ] 3.2 Add a private versioned Wasm import, lowering, inspection data, and deterministic host.
- [ ] 3.3 Add cross-engine tests for ordering, destinations, failures, missing providers, and replacement.

## 4. Verification

- [ ] 4.1 Document the Logger/default-provider/Stream-Sink boundary.
- [ ] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and applicable release-candidate verification.

