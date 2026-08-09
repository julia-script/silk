## Why

Real programs need a minimal way to expose results, but raw process output, structured logging, and general streams are different concepts. An explicit byte-writing service enables observation without making Logger ambient or designing the full Stream/Sink model prematurely.

## What Changes

- Add an explicit `StandardStreams` service requirement with stdout and stderr destinations.
- Add an all-or-typed-failure `writeAll` operation over immutable bytes.
- Model writes deterministically in evaluation.
- Add a native process adapter and a private versioned WebAssembly host import.
- Keep `Effect.log`, structured Logger routing, OpenTelemetry, Stream/Sink, and default providers out of this change.

## Capabilities

### New Capabilities

- `bootstrap-standard-streams`: Explicit standard-output/error byte writes with typed failures and host boundaries.

### Modified Capabilities

- `bootstrap-evaluation`: Record ordered standard-stream byte events and failures.
- `bootstrap-mir`: Represent target-neutral ordered byte writes with typed outcomes.
- `bootstrap-backend`: Realize writes through native and hosted-WebAssembly providers.
- `bootstrap-native-toolchain`: Connect final artifacts to explicit process or host stream providers.

## Impact

The change touches service requirement rows, stdlib/runtime actors, HIR/MIR, evaluation, LLVM/runtime lowering, direct WebAssembly imports, native finalization, hosting tests, and inspection artifacts. Defaults remain a future general service feature that applies uniformly to all services.
