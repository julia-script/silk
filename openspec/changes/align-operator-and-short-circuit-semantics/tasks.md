## 1. Explicit operator contracts

- [ ] 1.1 Add the operator marker to interface operation syntax, semantic facts, formatter, and inspectors.
- [ ] 1.2 Validate the closed token set, arity, operand, and result contracts.
- [ ] 1.3 Migrate builtin and standard-library operator interfaces to explicit markers.
- [ ] 1.4 Select concrete and generic operations through ordinary conformance evidence and delete name privilege.

## 2. Ordinary short-circuit analysis

- [ ] 2.1 Remove the right-operand impurity pass and model `&&`/`||` as conditional HIR regions.
- [ ] 2.2 Apply path-local ownership, loans, Effects, cleanup, and boolean join analysis.
- [ ] 2.3 Align MIR, evaluation, LLVM, and Wasm while preserving left-to-right conditional execution.

## 3. Verification

- [ ] 3.1 Add heterogeneous vector-scalar and matrix-shaped signature tests plus unmarked-name rejection.
- [ ] 3.2 Add effectful, affine, borrowed, cleanup, trap, and skipped-right short-circuit cases.
- [ ] 3.3 Update diagnostics, canonical specs, docs, and remove obsolete purity/name fixtures.
- [ ] 3.4 Run typecheck, Biome, evaluator/Wasm tests, native corpus where dispatch is target-specific, full tests, and `pnpm check`.
