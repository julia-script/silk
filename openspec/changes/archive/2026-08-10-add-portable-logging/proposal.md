## Why

Silk can write bytes to process standard streams but cannot express semantic logging without
coupling the call site to stdout. A portable Logger is the first application-level service that
should run unchanged under native, test, telemetry, and browser-hosted WebAssembly providers.

## What Changes

- Add a closed `LogLevel` and an explicit `Logger` service contract whose operation receives a
  severity and one complete borrowed immutable UTF-8 message per invocation.
- Add ordinary source-defined `Effect.log` that dispatches one complete message and honestly retains the
  Logger requirement through composition until a provider is supplied.
- Add a stdout-backed Logger implementation over `StandardStreams`; the initial provider may
  forward the borrowed bytes directly, while formatting and physical writes remain provider policy.
- Add a deterministic in-memory Logger implementation for tests and host-independent acceptance.
- Preserve invocation order and complete-message boundaries across evaluator, native LLVM, and direct
  WebAssembly execution.
- Integrate Logger values and operations with standard-library source packaging, hover,
  completion, occurrences, and go-to-definition.
- Keep browser console, OpenTelemetry, filtering, fan-out, tracing spans, and asynchronous export
  compatible with the contract without implementing them in this slice.

## Capabilities

### New Capabilities

- `bootstrap-logging`: Complete semantic log invocations, the Logger service contract, provider-owned
  formatting and output strategy, stdout and in-memory providers, ordering, failures, and cross-engine behavior.

### Modified Capabilities

- `bootstrap-flow-functions`: Add source-defined `Effect.log` with honest Logger requirement
  propagation through ordinary Effect composition.
- `bootstrap-silk-stdlib`: Ship canonical navigable logging source and its initial providers without
  compiler-known library names.
- `bootstrap-standard-streams`: Define the stdout-backed Logger as a consumer of complete stream
  writes while preserving the semantic boundary between logging and raw process output.

## Impact

The change affects the canonical Silk standard library and embedded manifest, service and type
elaboration, evaluator provider plumbing, native and direct-Wasm host boundaries, standard-stream
integration, editor presentation/navigation, labs, and differential acceptance tests. It adds no
ambient logger, byte-at-a-time logging API, scheduler, tracing runtime, or mandatory telemetry
dependency. These artifacts are reconciled with the implemented and archived source-defined
service, static interface, and sealed Intrinsic contracts. The logging implementation is unblocked.
