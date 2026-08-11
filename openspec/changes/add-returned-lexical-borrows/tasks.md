## 1. Returned-View Contracts

- [ ] 1.1 Add ordinary-function result analysis that records exactly one borrowed parameter origin and rejects multiple, absent, or unsupported boundary origins.
- [ ] 1.2 Admit shared results from shared or exclusive inputs and exclusive results only from exclusive inputs, with focused signature and return-path diagnostics.
- [ ] 1.3 Admit lexical slice locals while retaining rejection for aggregates, arrays, unions, Effects, errors, captures, lazy functions, and service operations.

## 2. Ownership and Lowering

- [ ] 2.1 Extend ownership facts with returned-view source provenance, access mode, formation span, and last-use live range.
- [ ] 2.2 Reject conflicting owner reads, writes, moves, and cleanup while returned views are live, and restore access after the last use.
- [ ] 2.3 Preserve returned-view provenance through HIR and MIR calls, assignments, returns, and compatible reborrows without emitting runtime owner tokens.
- [ ] 2.4 Add deterministic diagnostics and fact encodings for owner escape, conflicting access, invalid origin, and unsupported storage.

## 3. RawBuffer and Vector Source

- [ ] 3.1 Add shared and exclusive initialized-range RawBuffer view operations to the sealed intrinsic catalog with unsafe contracts and all-target availability.
- [ ] 3.2 Implement intrinsic analysis, HIR/MIR representation, verification, evaluation, LLVM lowering, and direct-Wasm lowering without recognizing `Vector`.
- [ ] 3.3 Add canonical ordinary-source `Vector.asSlice` and `Vector.asMutSlice` wrappers and update the standard-library manifest.

## 4. Acceptance and Tooling

- [ ] 4.1 Add accepted fixtures for shared, exclusive, nested, subview, and last-use returned borrows across evaluation, native LLVM, and direct Wasm.
- [ ] 4.2 Add rejected fixtures for multiple origins, exclusive strengthening, owner mutation/move/drop, lifetime escape, effects/services, captures, and stored views.
- [ ] 4.3 Add tooling tests for hover, definition, occurrences, navigation, ownership facts, and canonical `Vector` source.
- [ ] 4.4 Regenerate committed manifests and goldens, run `pnpm check`, and run `pnpm release:candidate` if package contents or exports changed.

