## Why

Silk can call scalar C functions but cannot express the two remaining symbol shapes needed by
ordinary native APIs: callbacks and global data. This blocks APIs such as `qsort` and prevents the
OS-provider migration from reading `environ` without retaining compiler intrinsics.

## What Changes

- Add the noncapturing C function-pointer type `extern "C" fn(P...) -> R` and contextual conversion
  from an exact, synchronous, nongeneric `export "C" fn` item.
- Diagnose conversion of ordinary, suspending, generic, or capturing callables to a C function
  pointer.
- Add immutable `unsafe extern "C" static` imports and initialized `export "C" static` exports,
  including optional external symbol spelling for imports.
- Carry reachable callback and data-symbol facts through semantic analysis, MIR, native LLVM
  declaration/lowering, artifacts, and deterministic symbol inventories.
- Keep reachable callback conversion and data-symbol access unavailable to the evaluator and direct
  WebAssembly until those surfaces have explicit pointer/global binding models.
- Document and verify native `qsort`, `environ`, and separately linked exported-data programs.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Define C function-pointer types and foreign/static declaration syntax and
  recovery.
- `bootstrap-foreign-functions`: Define callback conversion, foreign data symbols, native-only
  reachability, and diagnostics.
- `bootstrap-backend`: Define native declarations, addresses, loads, and exports for callbacks and
  data symbols.
- `bootstrap-evaluation`: Define the explicit evaluator boundary for unsupported reachable callback
  and data-symbol operations.

## Impact

This changes the language reference and compiler syntax/type/fact models, executable discovery,
MIR, native LLVM lowering, artifact metadata, diagnostic catalog, and native acceptance corpus.
There is no compatibility surface to preserve: the repository is green-field, and superseded
native-only rejection paths are replaced rather than adapted.
