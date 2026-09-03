## Why

Reachable `extern "C"` calls currently make the evaluator and direct WebAssembly backend unusable,
even though MIR already preserves each symbol and classified C signature. This prevents foreign-backed
standard-library providers from using the fast evaluator and Wasm paths without compiler-specific
fallbacks.

## What Changes

- Add an explicit, per-evaluation host-function table keyed by foreign symbol.
- Validate every reachable evaluator binding against the declaration's exact classified C signature
  before execution begins, and report missing or mismatched bindings without starting evaluation.
- Execute admitted foreign calls through their host binding while preserving Silk scalar and pointer
  value representations.
- Emit reachable foreign calls from the direct WebAssembly backend as deterministic symbol-named
  function imports with matching Wasm signatures.
- Replace the blanket evaluator/Wasm availability rejection with target-specific availability and
  binding diagnostics, and expose symbol-specific blocked reasons to labs and inspector surfaces.
- Update the language reference and diagnostic documentation for the expanded execution surfaces.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-foreign-functions`: Foreign calls become available through explicit evaluator host
  bindings and direct WebAssembly imports while remaining reachability-based and target-safe.
- `bootstrap-evaluation`: Evaluation admits, validates, and invokes per-run foreign host bindings.
- `bootstrap-backend`: Direct WebAssembly emission lowers foreign calls to deterministic imports and
  records their ABI inventory on the artifact.

## Impact

The change affects the compiler's foreign-availability planning, evaluator options and execution,
blocked-outcome presentation, direct Wasm lowering, backend artifact metadata, compiler tests, and
foreign-function documentation. It uses the existing `@silklang/wasm` import API and adds no runtime
dependency or compiler-owned libc surface.
