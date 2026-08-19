## Why

Current text handling relies on special semantic exceptions and incomplete scalar conversions. The confirmed language treats `string`, views, bytes, Unicode scalars, and integer indices through ordinary value, ownership, and checked-conversion rules.

## What Changes

- Remove text-specific type compatibility exceptions and route string references and byte views through ordinary borrow provenance.
- Preserve `string` as immutable UTF-8 text and `&[u8]` as binary data even when their bytes coincide.
- Add the checked scalar conversion needed for UTF-8 traversal to produce `char` without silent truncation.
- Align literal inference, indexing, length, equality, debug presentation, evaluation, LLVM, and Wasm.
- Replace `SEM0094` exceptions with ordinary type or conversion diagnostics.

## Capabilities

### Modified Capabilities

- `bootstrap-string`: use ordinary value and borrow semantics for text and its views.
- `bootstrap-runtime-slices`: preserve provenance and binary identity for text-derived byte slices.
- `bootstrap-integer-scalars`: expose the required checked conversion to `char`.
- `bootstrap-evaluation`: evaluate UTF-8 traversal and checked scalar conversion exactly.
- `bootstrap-backend`: preserve identical text/value behavior and presentation across targets.

## Impact

Depends on `generalize-borrows-and-callable-lifetimes`. It changes semantic typing, ownership facts, scalar conversion, evaluator/backends, standard-library traversal, diagnostics, and tests. It introduces no second text type, locale semantics, or implicit lossy conversion.
