## 1. Harness

- [x] 1.1 Create `examples/algorithms` and the per-example source/fixture/outcome/capability/status format.
- [x] 1.2 Add harness checks for executable parity, normalized frontier evidence, and executable-to-frontier regression.

## 2. Executable Baseline

- [x] 2.1 Add and graduate fixed-board Game of Life with final-board output parity.
- [x] 2.2 Add and graduate Sieve with deterministic prime results.
- [x] 2.3 Add and graduate fixed-size matrix multiplication with expected matrix results.
- [x] 2.4 Add and graduate CRC-32 with committed bytes and expected checksum.

## 3. Frontier Programs

- [x] 3.1 Add complete in-place quicksort and either execution parity or precise frontier evidence.
- [x] 3.2 Add complete FFT and precise math/allocation/collection frontier evidence until executable.

## 4. Verification

- [x] 4.1 Run all algorithm analysis in CI and require explicit review for evidence changes.
- [x] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`.
