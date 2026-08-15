## Why

Generic wrappers cannot currently preserve the compile-time capabilities of the values they wrap.
Static schema and command composition needs a conformance such as `MappedSchema<S, F>: Decoder`
whenever `S: Decoder`, without runtime dictionaries or compiler-known actor names.

## What Changes

- Add bounded `impl<...>` declarations whose requirements are proved at concrete specialization.
- Index generic conformance heads and reject possible overlap at declaration time using conservative,
  kind-aware unification that ignores bounds.
- Require strict structural descent of the provider, non-increasing variable occurrences, and
  unchanged-only ground non-provider arguments so proof search terminates.
- Emit finite requirement and active-cycle traces for failed proofs.
- Preserve one statically selected witness per concrete provider/interface specialization with no
  runtime dictionary or interface dispatch.
- **BREAKING**: reject overlapping conditional heads even when their bounds appear mutually
  exclusive.

## Capabilities

### New Capabilities

- `bootstrap-conditional-interface-conformance`: Bounded conformance syntax, coherence,
  termination, proof search, specialization, and diagnostics.

### Modified Capabilities

- `bootstrap-declaration-index`: Index conditional generic heads, requirements, overlap state, and
  termination facts.
- `bootstrap-instances`: Discover and key concrete conditional witnesses and their transitive proof
  requirements.

## Impact

Builds on existing user-interface witnesses and multi-operation bounds; representation-bounded
impls additionally depend on `introduce-representation-parameters`. Affects parsing, declaration
indexing, semantic proof search, HIR witness questions, instances, diagnostics, and determinism.
