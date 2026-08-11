## 0. Intrinsic and Service Prerequisite

- [x] 0.1 Archive `establish-minimal-intrinsic-boundary` before starting logging implementation
- [x] 0.2 Reconcile every logging artifact with the archived service, Intrinsic, text, and StandardStreams contracts

## 1. Canonical Logging Surface

- [x] 1.1 Add canonical LogLevel, LogError, and a Logger service operation accepting separate level and complete borrowed-message parameters in a new Silk standard-library module
- [x] 1.2 Add the Logger service declaration and borrowed complete-invocation contracts to declaration, type, requirement-row, and presentation facts
- [x] 1.3 Add the logging module to the canonical manifest and generated embedded-source verification

## 2. Source-Defined Effect API

- [x] 2.1 Implement ordinary `Effect.log` Info dispatch and the explicit-level sibling in canonical Silk source
- [x] 2.2 Prove Logger requirement and LogError propagation across direct, piped, stored, tap, flatMap, catch, and provision forms
- [x] 2.3 Add negative coverage for missing providers, invalid level or message inputs, and any accidental logging-specific HIR or MIR operation

## 3. In-Memory Semantic Provider

- [x] 3.1 Implement `impl Logger for InMemoryLogger` with provider-owned ordered recorded observations and inspection operations
- [x] 3.2 Add deterministic failure-ordinal behavior and verify that failed events and later dependent events are not recorded
- [x] 3.3 Prove borrowed message lifetime, provider cleanup, and deterministic capacity failure without ambient allocator requirements

## 4. Stdout Provider

- [x] 4.1 Implement `impl Logger for StdoutLogger` by forwarding the complete borrowed message through StandardStreams without mandatory allocation or canonical formatting
- [x] 4.2 Translate StreamWriteFailure to LogError while keeping destinations, decoration, newlines, buffering, and physical write strategy provider-owned
- [x] 4.3 Verify that direct StandardStreams writes remain free of Logger, severity, and telemetry semantics

## 5. Execution and Tooling Parity

- [x] 5.1 Add evaluator, native LLVM, and direct-Wasm acceptance for success, failure, ordering, one invocation per log call, multiline messages, and provider replacement
- [x] 5.2 Add fresh-process determinism coverage and confirm no Logger-specific host import or runtime scheduler is introduced
- [x] 5.3 Add completion, hover, occurrences, go-to-definition, documentation, and standard-library source tests for every logging declaration
- [x] 5.4 Add a facade-only Labs preset showing requirement rows, provider conformance, HIR, MIR, events, and backend artifacts

## 6. Pressure Evidence and Verification

- [x] 6.1 Migrate one representative pressure-program observation to Effect.log without replacing raw program output that is not semantic logging
- [x] 6.2 Update public standard-library and service documentation with the accepted event/provider boundary
- [x] 6.3 Run `pnpm typecheck`, `pnpm exec biome check .`, focused logging tests, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
