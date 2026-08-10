## Why

The synchronous Effect cost spike shows that Clang removes source-defined composition from every
measured native entry, while the direct WebAssembly entry still calls trivial Effect constructors
before invoking a statically selected runner. Silk needs one backend-independent normalization for
that proven representation-only cost before attempting the much larger problem of cloning complete
runner control-flow graphs.

## What Changes

- Add a shared MIR normalization that folds direct, single-region Effect constructor calls.
- Replace a take-once `MakeEffect` followed by a statically selected `RunEffectValue` with one direct
  runner operation whose arguments are the original captures and providers.
- Record deterministic accepted/rejected verdicts without recognizing pipe syntax, Effect API
  names, standard-library declarations, or source locations.
- Preserve evaluation order, typed failure propagation, provider arguments, traps, semantic runtime
  observations, and ownership;
  conservatively retain ordinary MIR for escaping, reused, mutable, affine, dynamic, or structurally
  complex candidates.
- Apply the same normalized MIR to evaluation, LLVM, and direct Wasm, and extend the cost corpus with
  entry-local structural evidence.

The proposal deliberately does **not** inline runner or callback bodies. Cross-function CFG cloning,
completed-outcome scalarization, and affine exit remapping remain a later proposal once this first
representation seam is proven.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-mir`: Add guarded constructor and static-dispatch normalization before MIR consumers.
- `bootstrap-compiler-driver`: Gate behavioral parity and direct-Wasm entry structure for the
  normalized subset.

## Impact

The change affects shared MIR representation, transformation and verification, the evaluator, both
backends, analysis projections, and the synchronous cost harness. It does not change Silk syntax,
Effect library declarations, public failure/requirement contracts, suspension semantics, or
backend-specific public APIs.
