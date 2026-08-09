## 1. Static-view read contract

- [ ] 1.1 Add semantic/HIR regression coverage showing a byte literal remains a shared `u8` slice with index and length operations
- [ ] 1.2 Extend MIR place verification tests to accept a checked static-view selector and reject scalar, aggregate, wrong-index, and mismatched-static-data roots
- [ ] 1.3 Pin deterministic MIR encoding for a loop that indexes committed static bytes

## 2. Evaluator support

- [ ] 2.1 Refactor slice-element evaluation to select from either runtime `SliceValue` storage or immutable `StaticViewValue` bytes
- [ ] 2.2 Preserve ordinary `usize` bounds checks, `u8` results, and indexing provenance for static-view reads
- [ ] 2.3 Add evaluator tests for every valid byte, zero-length input, out-of-bounds blockage, repeated reads, and absence of allocation events

## 3. Backend parity

- [ ] 3.1 Route LLVM static-view indexing through the existing checked slice address/length path and test valid and trapping reads
- [ ] 3.2 Route WebAssembly static-view indexing through the existing checked slice address/length path and test valid and trapping reads
- [ ] 3.3 Verify both emitted artifacts read immutable static storage without copying or runtime allocation and match evaluator results

## 4. CRC-32 graduation

- [ ] 4.1 Change CRC-32 to accept a shared byte view and call it with `b"\x99\x13\x1d\x00"`
- [ ] 4.2 Update its manifest capability inventory and README to remove the indexed-static-view frontier note
- [ ] 4.3 Run CRC-32 through evaluation, native, and direct WebAssembly and retain the committed checksum

## 5. Verification

- [ ] 5.1 Run focused static-text, MIR, evaluator, backend, StandardStreams, and algorithm tests
- [ ] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
