## Why

Entrypoint discovery, typed failure eligibility, exit statuses, fatal traps, and reporting still encode several superseded rules, including an `i32`-only ordinary entry and the `Report` gate. A programmer cannot reliably predict whether a valid `main` runs, which status it returns, or what diagnostic context survives optimization and suspension.

## What Changes

- Accept public ordinary `main() -> ()` and `main() -> i32`, plus public effectful `main() -> () ! E ? never` after all dependencies are resolved.
- Remove `Report`; every concrete detached owned failure value is entry-eligible.
- Map ordinary/effect success to status zero, unhandled typed failure to one, and preserve custom status only for ordinary `i32` entry.
- Produce one target-neutral structured termination outcome for success, typed failure, and fatal trap with identity, reason, provenance, logical call path, and causal recovery history.
- Preserve logical traces through optimization and suspension, expose structured data to embeddings, and render it only in standalone adapters.
- Keep adapters pay-for-use and diagnose private `main`, invalid shapes, and unresolved dependencies accurately.

## Capabilities

### Modified Capabilities

- `bootstrap-entry-termination`: align entry discovery, eligibility, statuses, reports, and traps.
- `bootstrap-evaluation`: return the structured termination outcome without ambient console behavior.
- `bootstrap-backend`: preserve the same outcome and logical trace across LLVM and Wasm.
- `bootstrap-compiler-driver`: expose structured outcomes to CLI and embedding adapters.
- `bootstrap-instances`: remove `Report` and its conformance exception.

## Impact

Depends on `normalize-effect-failure-types` and `normalize-effect-requirement-provision`. It replaces entry and runtime reporting machinery atomically across compiler, evaluator, backends, native adapter, CLI, embeddings, diagnostics, standard library, and tests. It does not add user-defined error formatting; that remains future library work.
