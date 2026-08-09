## Why

Silk programs can model intentional, typed failures inside lazy Effects, but the executable entry
currently accepts only an ordinary `main() -> I32`. Programs must therefore catch every failure by
hand before the boundary, preventing the runtime from giving an unhandled application error the
same deliberate reporting and termination semantics that Rust gives a `main` returning `Result`.

## What Changes

- Accept `pub effect fn main() -> Unit ! E` as an executable entry alongside the existing
  `pub fn main() -> I32` form.
- Introduce the compiler-sealed `Report` marker capability. Every nominal member of an effectful
  entry's failure row must explicitly conform to `Report`.
- Generate a closed host adapter that constructs and runs the entry Effect exactly once, maps
  `Unit` success to status `0`, reports an unhandled failure's canonical identity to standard
  error, releases the owned failure payload, and maps it to status `1`.
- Keep traps outside typed termination: they remain abnormal termination and bypass reporting and
  cleanup guarantees.
- Make MIR and backend entry identity explicit instead of assigning `silk_main` to whichever
  function happens to be first.
- Reject effectful entries with unresolved service requirements until the native host-provider
  adapter exists; no requirement row may escape the executable boundary.
- Preserve the existing ordinary `main() -> I32` behavior for programs that intentionally choose
  their process status.

## Capabilities

### New Capabilities

- `bootstrap-entry-termination`: Rust-like effectful entry execution, reportability, cleanup, and
  process termination semantics.

### Modified Capabilities

- `bootstrap-instances`: Recognize the effectful `Unit` entry form and retain its closed failure
  contract during reachability discovery.
- `bootstrap-evaluation`: Evaluate an effectful entry directly and expose its success or unhandled
  typed failure as deterministic data.
- `bootstrap-mir`: Represent the selected entry and generated closing adapter explicitly in MIR.
- `bootstrap-backend`: Emit `silk_main` from explicit MIR entry metadata for both ordinary and
  effectful entries rather than function order.
- `bootstrap-native-toolchain`: Generate the native shim report table and convert effect-failure
  termination to standard error plus status `1`.

## Impact

The compiler's intrinsic type catalog, conformance validation, instance discovery, evaluator, MIR,
LLVM backend, WebAssembly backend, native shim planning, driver finalization, and their tests are
affected. The source language gains a new accepted entry signature and a new compiler-sealed marker
capability. Existing ordinary entries retain their behavior, but internal MIR and backend APIs may
break because this project is pre-release and entry ordering will no longer be semantic.
