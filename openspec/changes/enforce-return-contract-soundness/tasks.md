## 1. Semantic return proof

- [x] 1.1 Inventory every ordinary, effectful, generic, and conformance body path that publishes executable availability.
- [x] 1.2 Add one resolved return-contract proof covering every reachable explicit return and fallthrough path.
- [x] 1.3 Route mapped conformance targets through the declaration proof and make that seam mandatory for inline bodies introduced by the dependent conformance change.
- [x] 1.4 Add precise diagnostics for incompatible values, missing returns, and nested Effect mismatches.

## 2. Lowering boundary

- [x] 2.1 Prevent invalid or unavailable bodies from entering target-dependent reachability while retaining explicitly unavailable HIR for inspection.
- [x] 2.2 Require the semantic proof at MIR construction and retain verifier checks for compiler-generated invalid input.
- [x] 2.3 Confirm no backend-specific invalid-return path remains reachable; retain only generic invalid-MIR rejection for hand-built or compiler-bug input.

## 3. Verification and reconciliation

- [x] 3.1 Add analysis regressions for issue 226, ordinary functions, effect functions, generics, and interface dispatch.
- [x] 3.2 Confirm valid explicit nested Effects remain one layer and run only when source says `run`.
- [x] 3.3 Update diagnostics, canonical specs, language evidence links, and close the reconciliation handoff.
- [ ] 3.4 Run typecheck, Biome, focused compiler tests, full tests, and `pnpm check`.
