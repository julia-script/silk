## Why

Silk can analyze, specialize, lower, and emit recursive functions, but bootstrap evaluation rejects
every re-entry of an active function before a terminating recursive program can run. Quicksort is
therefore complete source that cannot satisfy the corpus's evaluator/native/WebAssembly parity gate.

## What Changes

- Execute ordinary direct and mutual recursion with distinct runtime activation frames in bootstrap
  evaluation.
- Replace active-function cycle rejection with deterministic evaluator step and call-depth limits so
  non-terminating programs remain bounded without misclassifying all recursion as a cycle.
- Report limit exhaustion as structured blocked data with the configured limit, active function,
  and source provenance; do not rely on JavaScript stack overflow.
- Preserve deterministic nested call/binding/return traces for recursive activations and expose the
  new blocked outcome in the Syntax Inspector.
- Run terminating recursion across evaluation, native LLVM, and direct WebAssembly, and graduate
  the existing in-place quicksort example to executable status.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-evaluation`: Execute recursive activations under deterministic resource limits instead
  of rejecting active-function re-entry.
- `bootstrap-syntax-inspector`: Render recursive completion and evaluation-limit blockage.
- `bootstrap-compiler-driver`: Gate terminating recursion through evaluator/native/WebAssembly
  differential execution and deterministic artifacts.
- `bootstrap-algorithm-examples`: Graduate quicksort when recursive execution agrees across engines.

## Impact

- Changes the bootstrap evaluator's call machine, blocked-reason model, trace encoding, inspector
  presets/rendering, recursion corpus expectations, and quicksort manifest.
- The evaluator limits are tooling policy, not a language-level termination proof or a mandatory
  native/WebAssembly runtime quota.
