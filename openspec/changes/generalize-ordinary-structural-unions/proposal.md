## Why

Structural unions are normalized as ordinary types in the stabilized language, but current storage, matching, and ownership paths still privilege nominal members. This blocks detached values, exact non-nominal patterns, and consistent failure unions from sharing one type model.

## What Changes

- Normalize finite structural unions over every detached ordinary value type, flattening and deduplicating members deterministically.
- Compute compatibility, layout, tags, Copy, ownership, cleanup, and specialization from the normalized members.
- Carry exact member identity through HIR/MIR, evaluation, LLVM, and Wasm without nominal-only tests.
- Support injection, narrowing, return and branch joins, and diagnostics for ambiguous or non-detached members.
- Provide the exact-type membership evidence consumed by shared patterns and failure recovery.

## Capabilities

### Modified Capabilities

- `bootstrap-structural-unions`: admit and normalize ordinary detached member types.
- `bootstrap-type-generics`: preserve concrete finite unions through inference and specialization.
- `bootstrap-ownership`: derive union ownership and cleanup from all normalized members.
- `bootstrap-hir`: carry exact member mappings without nominal restriction.
- `bootstrap-mir`: encode deterministic ordinary-member tags and payload plans.
- `bootstrap-evaluation`: evaluate injection and projection for every admitted member.
- `bootstrap-backend`: realize the same union plan in LLVM and Wasm.

## Impact

Depends on `normalize-effect-failure-types` and `define-copy-and-executable-ownership`. It replaces nominal-only union assumptions across the compiler and tests without adding open unions, runtime type reflection, or specialization-dependent membership.
