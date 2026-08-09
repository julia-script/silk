## 1. Characterize Context Propagation

- [ ] 1.1 Add focused regression cases for concrete direct-call literals, explicit generic contexts, pipeline insertion, out-of-range literals, and already-typed mismatches
- [ ] 1.2 Assert the selected semantic/HIR/MIR integer widths and pre-MIR diagnostic behavior

## 2. Repair Call Elaboration

- [ ] 2.1 Align argument nodes with effective parameter types before exact integer literal analysis
- [ ] 2.2 Preserve unresolved generic inference, partial-call alignment, and the no-implicit-conversion rule

## 3. Remove the Pressure-Program Workaround

- [ ] 3.1 Change the lexer byte helpers and comparisons to consume `u8` literals directly
- [ ] 3.2 Update the lexer findings to record the resolved compiler defect and any new evidence

## 4. Verify and Finalize

- [ ] 4.1 Prove focused evaluator, native LLVM, direct WebAssembly, and lexer corpus parity
- [ ] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
- [ ] 4.3 Strictly validate the OpenSpec change and reconcile all planning artifacts with the implementation
