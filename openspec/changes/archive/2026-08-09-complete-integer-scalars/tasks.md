## 1. Catalog and Source Migration

- [x] 1.1 Add the integer scalar catalog and refactor current built-in consumers to query it.
- [x] 1.2 Add lowercase integer identities, `()`, and `never`; remove uppercase aliases and migrate parser/semantic tests.
- [x] 1.3 Implement unit fallthrough/bare return and payload-free unit/bottom through HIR, MIR, layout, evaluation, and encoders.
- [x] 1.4 Migrate fixtures, examples, docs, editor expectations, and goldens to lowercase spellings.

## 2. Exact Integer Semantics

- [x] 2.1 Replace literal magnitude handling with exact values and implement contextual ranges plus default `i32`.
- [x] 2.2 Add every fixed- and target-width integer contract, explicit conversions, and homogeneous operators/callable sections.
- [x] 2.3 Add checked trapping arithmetic, comparisons, bitwise operations, shifts, and rotates for all widths.
- [x] 2.4 Add wrapping and saturating variants with deterministic primitive identities.
- [x] 2.5 Add canonical `Option<T>` stdlib source and recoverable checked operations/conversions.

## 3. Usize Migration

- [x] 3.1 Change array indices and HIR places to `usize` with static range diagnostics.
- [x] 3.2 Change slice lengths/indices and allocation/capacity size paths to `usize`.
- [x] 3.3 Update MIR/evaluator/backend bounds and addressing logic plus native-64/Wasm-32 tests.

## 4. Pipeline and Backends

- [x] 4.1 Add canonical layouts/calling lanes and complete integer HIR/MIR operations, verification, and encoding.
- [x] 4.2 Implement exact width-aware evaluator behavior and Option outcomes.
- [x] 4.3 Implement LLVM lowering for all widths, signedness, modes, conversions, and traps.
- [x] 4.4 Implement direct Wasm lowering including subword masking/sign extension and 32/64-bit lanes.
- [x] 4.5 Derive hover/completion from the catalog and add the complete differential parity matrix.

## 5. Verification

- [x] 5.1 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`.
- [x] 5.2 Run `pnpm release:candidate` if stdlib/package artifacts change and record any pre-existing failure precisely.
