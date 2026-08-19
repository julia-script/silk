## 1. Semantic return proof

- [ ] 1.1 Inventory every ordinary, effectful, generic, and conformance body path that publishes executable availability.
- [ ] 1.2 Add one resolved return-contract proof covering explicit returns, final expressions, and reachable fallthrough.
- [ ] 1.3 Apply the proof after generic and `Self` substitution for inline and mapped conformance operations.
- [ ] 1.4 Add precise diagnostics for incompatible values, missing returns, and nested Effect mismatches.

## 2. Lowering boundary

- [ ] 2.1 Prevent invalid or unavailable bodies from entering reachable HIR.
- [ ] 2.2 Require the semantic proof at MIR construction and retain verifier checks for compiler-generated invalid input.
- [ ] 2.3 Remove backend paths and tests that treat invalid-return MIR as a supported input.

## 3. Verification and reconciliation

- [ ] 3.1 Add analysis regressions for issue 226, ordinary functions, effect functions, generics, and interface dispatch.
- [ ] 3.2 Confirm valid explicit nested Effects remain one layer and run only when source says `run`.
- [ ] 3.3 Update diagnostics, canonical specs, language evidence links, and close the reconciliation handoff.
- [ ] 3.4 Run typecheck, Biome, focused compiler tests, full tests, and `pnpm check`.
