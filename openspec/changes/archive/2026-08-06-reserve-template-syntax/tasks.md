## 1. Bracketed Fixed-Array Syntax

- [x] 1.1 Add `Semicolon` to the token vocabulary, lexer punctuation recognition, canonical token spelling, and focused lexer coverage.
- [x] 1.2 Replace the `Array<T, N>` type-constructor lookahead and parser branch with lossless `[T; N]` parsing in every explicit type position, including recursive element types and local missing-token recovery.
- [x] 1.3 Update fixed-array concrete-syntax consumers such as declaration indexing and syntax inspection to read bracket, semicolon, and length children without changing the structural semantic type.
- [x] 1.4 Add parser tests for simple, nested, zero-length, missing-length, missing-separator, and missing-bracket forms, plus rejection of the former `Array<T, N>` spelling.

## 2. Template-Start Reservation

- [x] 2.1 Add a stable parser-owned diagnostic for template syntax that is reserved but not implemented.
- [x] 2.2 Detect `<Identifier` and `<>` only when a primary expression is required, retain the reserved input with existing error-syntax machinery, and recover at the enclosing expression boundary without adding template syntax kinds.
- [x] 2.3 Add parser recovery tests for element and fragment starts before following statements and declarations.
- [x] 2.4 Add regression tests proving `<`, `<=`, `>`, and `>=` still parse as relational operators after an existing left operand.

## 3. Formatting and Language Tooling

- [x] 3.1 Format complete fixed-array syntax canonically as `[T; N]`, recursively applying the same layout to nested element types.
- [x] 3.2 Add formatter tests for irregular trivia, nested arrays, idempotence, and rejection of missing fixed-array punctuation.
- [x] 3.3 Update syntax-inspector presets, syntax encodings, and editor token handling affected by the new semicolon token and bracketed type children.

## 4. Source Migration

- [x] 4.1 Replace `Array<T, N>` in Silk source fixtures and corpus programs with `[T; N]` without changing expected semantic, ownership, layout, HIR, MIR, or runtime outcomes.
- [x] 4.2 Update user-facing source examples and syntax-focused documentation to use bracketed fixed-array types while leaving explicitly internal IR/debug notation unchanged.
- [x] 4.3 Audit remaining `Array<` occurrences and classify each as TypeScript syntax, intentional semantic/IR notation, or a missed Silk source migration.

## 5. Verification

- [x] 5.1 Run `openspec validate reserve-template-syntax --strict` and resolve every proposal or delta-spec error.
- [x] 5.2 Run `pnpm typecheck`, then `pnpm exec biome check .`, then `pnpm test`, preserving the repository-required verification order.
- [x] 5.3 Run `pnpm check` and `pnpm release:candidate`, and record any failure with whether it predates this change.
