## 1. Backend service

- [x] 1.1 Add `@silk-effect/llvm` as a compiler dependency; create
  `packages/compiler/src/Backend.ts`: the `Backend` interface (one `emit(program, layout,
  request)` operation) and the artifact shape (bitcode, IR text, canonical-id-to-symbol table)
- [x] 1.2 `LlvmBackend`: declare-then-define lowering over the builder — blocks, literals, moves,
  direct calls, drops, returns, branches, traps; deterministic symbols with `silk_main` for the
  entry
- [x] 1.3 Debug requests: compile unit, file, subprograms, instruction locations from source
  bytes; release requests omit metadata

## 2. Determinism gates

- [x] 2.1 Committed IR-text golden and bitcode SHA-256 digest golden for the nested-call program;
  repeat-determinism tests
- [x] 2.2 Backend tests: one artifact per program, symbol table, trap lowering, debug/release
  metadata difference

## 3. Facade and surfaces

- [x] 3.1 `Analysis.codegen(snapshot, request)` facade query; add `Backend` to the docs
  import-boundary list; facade test
- [x] 3.2 Package exports and release-candidate surface (packed llvm override, dependency
  assertion, `./Backend` deep import)

## 4. Inspector lab

- [x] 4.1 Create the direct-link `/docs/labs/llvm-ir` lab: emitted IR text, symbol table, lowered
  MIR blocks, debug/release toggle
- [x] 4.2 Lab tests: IR presence with `silk_main`, symbol table, debug toggle metadata

## 5. Verification

- [x] 5.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 5.2 `openspec validate establish-backend-service --type change --strict` passes
