## 1. MIR data model

- [x] 1.1 Create `packages/compiler/src/Mir.ts`: typed locals, normal and cleanup blocks,
  operations (literal, move, canonical-target call, drop), terminators (return, jump, branch,
  trap), provenance with generated markers
- [x] 1.2 `TargetLayout` as a separate emission-time input (triple, pointer width, endianness,
  logical size/alignment)
- [x] 1.3 Hand-built sample modules exported for tests and the lab (straight-line call sample;
  branch + cleanup + generated drops + trap sample)

## 2. Verifier

- [x] 2.1 Structural verifier returning ordered violations as data: entry block present,
  terminator targets exist, referenced locals declared
- [x] 2.2 Verifier tests: samples verify clean; broken module reports deterministic violations

## 3. Encoder

- [x] 3.1 Deterministic textual encoder (functions, blocks, ops, terminators, types, provenance,
  generated markers)
- [x] 3.2 Committed goldens for both samples; byte-identical and repeat-determinism tests

## 4. Package surface

- [x] 4.1 Export `Mir` from the index and exports map; update release-candidate surface

## 5. CFG lab

- [x] 5.1 Create the direct-link `/docs/labs/mir-cfg` lab: sample picker, blocks with kinds, ops,
  terminators, and edges; provenance on hover/focus; encoded text view
- [x] 5.2 Lab tests: blocks and edges, generated-drop provenance reveal, encoded text presence

## 6. Verification

- [x] 6.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 6.2 `openspec validate define-mir-and-encoder --type change --strict` passes
