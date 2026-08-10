## 1. Characterize the composition boundary

- [x] 1.1 Add a public-source regression where effectful `main` directly runs a provided Effect mapped to unit, and prove the current lowering failure before implementation.
- [x] 1.2 Add equivalent grouped, reverse-order, and stored source forms with independent expected results and execution-order observations.

## 2. Compose Effect recipes in lowering

- [x] 2.1 Introduce one recursive execution path for runtime Effect values and nested construction, transformation, and provision recipes.
- [x] 2.2 Route direct and stored `map`, `flatMap`, and `tap` execution through the recursive path while preserving callable construction and access.
- [x] 2.3 Route nested `provide`, `provideWith`, `catch`, and `retry` protected execution through the same path while preserving providers, failures, loans, and cleanup.
- [x] 2.4 Remove unavailable-transform fallback behavior for semantically valid stored compositions and keep invalid source unavailable before MIR.

## 3. Pressure pipelines through public programs

- [x] 3.1 Add a bounded pairwise pipeline matrix covering pure values, Effect operators, data-first/piped/grouped/stored forms, and ordinary/effectful entries.
- [x] 3.2 Add Copy and affine callback/result cases with exact ownership, cleanup, failure, and evaluation-order assertions.
- [x] 3.3 Run representative compositions through evaluator, native LLVM, and direct Wasm, plus fresh-process determinism.
- [x] 3.4 Rewrite the lexer and other clear pressure-program entrypoint flows to use the accepted pipeline form and retain all existing allocation/fingerprint evidence.

## 4. Validate and record the result

- [x] 4.1 Run focused tests after each red-green slice, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`.
- [x] 4.2 Run `pnpm release:candidate` if package contents or exports change and strict OpenSpec validation for the completed change.
- [x] 4.3 Update the real-program and project roadmaps with the confirmed pipeline boundary and completed evidence.
