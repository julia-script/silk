## 1. Characterize Context Propagation

- [x] 1.1 Add focused regression cases for concrete direct-call literals, explicit generic contexts, pipeline insertion, enclosing Boolean result contexts, out-of-range literals, and already-typed mismatches
- [x] 1.2 Assert the selected semantic/HIR/MIR integer widths and pre-MIR diagnostic behavior

## 2. Repair Contextual Elaboration

- [x] 2.1 Refine homogeneous operator arguments from the resolved first operand even when the enclosing expression supplies a result expectation
- [x] 2.2 Preserve call and pipeline parameter contexts, unresolved generic inference, and the no-implicit-conversion rule

## 3. Remove the Pressure-Program Workaround

- [x] 3.1 Change the lexer byte helpers and comparisons to consume `u8` literals directly
- [x] 3.2 Update the lexer findings to record the resolved compiler defect and any new evidence

## 4. Verify and Finalize

- [x] 4.1 Prove focused evaluator, native LLVM, direct WebAssembly, and lexer corpus parity
- [x] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
- [x] 4.3 Strictly validate the OpenSpec change and reconcile all planning artifacts with the implementation
