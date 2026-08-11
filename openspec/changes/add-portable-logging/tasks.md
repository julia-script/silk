## 0. Intrinsic and Service Prerequisite

- [ ] 0.1 Archive `establish-minimal-intrinsic-boundary` before starting logging implementation
- [ ] 0.2 Reconcile every logging artifact with the archived service, Intrinsic, text, and StandardStreams contracts

## 1. Canonical Logging Surface

- [ ] 1.1 Add canonical LogLevel, LogEvent, LogError, and Logger declarations in a new Silk standard-library module
- [ ] 1.2 Add Logger service conformance and borrowed complete-event contracts to declaration, type, and presentation facts
- [ ] 1.3 Add the logging module to the canonical manifest and generated embedded-source verification

## 2. Source-Defined Effect API

- [ ] 2.1 Implement ordinary `Effect.log` Info dispatch and the explicit-level sibling in canonical Silk source
- [ ] 2.2 Prove Logger requirement and LogError propagation across direct, piped, stored, tap, flatMap, catch, and provision forms
- [ ] 2.3 Add negative coverage for missing providers, invalid event types, and any accidental logging-specific HIR or MIR operation

## 3. In-Memory Semantic Provider

- [ ] 3.1 Implement InMemoryLogger with provider-owned ordered event storage and inspection operations
- [ ] 3.2 Add deterministic failure-ordinal behavior and verify that failed events and later dependent events are not recorded
- [ ] 3.3 Prove borrowed message lifetime, provider cleanup, and allocation failure behavior without ambient allocator requirements

## 4. Stdout Provider

- [ ] 4.1 Implement deterministic severity/message/LF rendering in StdoutLogger
- [ ] 4.2 Route each event through exactly one complete StandardStreams stdout write and translate StreamWriteFailure to LogError
- [ ] 4.3 Verify that direct StandardStreams writes remain free of Logger, severity, and telemetry semantics

## 5. Execution and Tooling Parity

- [ ] 5.1 Add evaluator, native LLVM, and direct-Wasm acceptance for success, failure, ordering, multiline messages, and provider replacement
- [ ] 5.2 Add fresh-process determinism coverage and confirm no Logger-specific host import or runtime scheduler is introduced
- [ ] 5.3 Add completion, hover, occurrences, go-to-definition, documentation, and standard-library source tests for every logging declaration
- [ ] 5.4 Add a facade-only Labs preset showing requirement rows, provider conformance, HIR, MIR, events, and backend artifacts

## 6. Pressure Evidence and Verification

- [ ] 6.1 Migrate one representative pressure-program observation to Effect.log without replacing raw program output that is not semantic logging
- [ ] 6.2 Update public standard-library and service documentation with the accepted event/provider boundary
- [ ] 6.3 Run `pnpm typecheck`, `pnpm exec biome check .`, focused logging tests, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
