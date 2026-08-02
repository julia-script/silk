## Why

The separately tested phases must be integrated into a compiler that learners can actually use. This lesson supplies the completion evidence: compiling the confirmed program to valid IR and running the native result.

## What Changes

- Compose lexing, parsing, validation, two-pass lowering, and rendering in `Compiler.compile`.
- Add the consumer-facing CLI boundary for source-file input, LLVM IR on stdout, and diagnostics on stderr.
- Compile the confirmed `score.tiny` program and inspect `abs`, `score`, and `main`.
- Compile the emitted IR with Clang and verify exit code `20`.
- Add end-to-end tests that keep the tutorial independent of the browser playground.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Completes the tutorial example and introduces end-to-end/native validation. No library runtime behavior changes.

