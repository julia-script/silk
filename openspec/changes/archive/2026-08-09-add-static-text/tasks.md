## 1. Syntax and Semantics

- [x] 1.1 Add lossless text/byte literal syntax and local escape recovery.
- [x] 1.2 Decode UTF-8 and exact bytes with deterministic malformed/non-byte diagnostics.
- [x] 1.3 Add semantic/HIR static identities, immutable views, provenance, and `usize` lengths.

## 2. MIR and Execution

- [x] 2.1 Add the canonical MIR static-data table, view operations, verification, and encoding.
- [x] 2.2 Add target layout facts and allocation-free evaluator behavior.
- [x] 2.3 Emit native and Wasm static data/views with reuse and non-ASCII parity tests.

## 3. Verification

- [x] 3.1 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`.
