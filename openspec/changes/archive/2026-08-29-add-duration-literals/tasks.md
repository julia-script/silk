## 1. Literal Recognition and Diagnostics

- [x] 1.1 Add the internal `DurationLiteral` actor with the closed unit catalog, maximal candidate scanning, exact component parsing, canonical order/bound validation, and unbounded `bigint` nanosecond scaling; verify the compiler and test TypeScript projects pass `pnpm --filter @silklang/compiler typecheck`.
- [x] 1.2 Add stable lexical and semantic diagnostic reasons for unknown units, non-whole or non-decimal components, unit order/repetition, subordinate bounds, and total `u64` overflow; verify diagnostic construction and exact spans in `packages/compiler/test/Diagnostic.test.ts`.
- [x] 1.3 Add `DurationLiteral` and `InvalidDurationLiteral` token kinds and integrate committed duration recognition into the numeric lexer path without changing standalone integer or float tokenization; verify every valid unit, compact compounds, malformed candidates, expression boundaries, and lossless recovery with `pnpm --filter @silklang/compiler exec vitest run test/Lexer.test.ts`.
- [x] 1.4 Add boundary coverage for digit separators, leading zeros, zero-valued fields, skipped units, first-field freedom, longest unit matching, and exact `u64` edges; verify the targeted lexer and duration-actor cases pass without a second diagnostic for one malformed candidate.

## 2. Syntax, Parsing, Formatting, and Inspection

- [x] 2.1 Add `DurationLiteralExpression` to syntax and primary-expression parsing, admit the invalid-duration recovery token without parser cascades, and update every exhaustive syntax catalog; verify lossless valid and invalid trees with `pnpm --filter @silklang/compiler exec vitest run test/Parser.test.ts`.
- [x] 2.2 Print valid duration expressions as atomic token content and retain the existing refusal to format lexical damage; verify `01h05m00s`, explicit zero components, digit separators, compact compounds, and additive forms are preserved idempotently with `pnpm --filter @silklang/compiler exec vitest run test/SyntaxFormatter.test.ts`.
- [x] 2.3 Extend token, syntax, and semantic inspection views for the new token and node kinds while reporting valid values as `u64`; verify the relevant compiler inspector and LSP inspection tests expose stable duration spans without inventing component syntax.

## 3. Semantic Facts, Constants, and Lowering

- [x] 3.1 Analyze valid duration expressions to a fixed `u64` fact, ignore contextual integer type selection, preserve exact totals, reject totals above `u64.MAX`, and suppress duplicate semantic diagnostics for invalid-duration tokens; verify fixed typing, mismatch behavior, representative scales, and overflow in the existing integer-scalar analysis tests.
- [x] 3.2 Add a distinct duration constant fact and deterministic module-surface encoding/decoding so exported duration constants retain fixed `u64` meaning and exact value; verify equal surfaces remain equal across spelling-only changes while value changes invalidate importers in the existing module-surface tests.
- [x] 3.3 Normalize valid duration facts to the existing HIR integer-literal form with type `u64` and keep MIR/backend catalogs duration-free; verify lowered MIR contains only the general scalar `Literal` operation for representative duration values.
- [x] 3.4 Add one shared semantic program covering all units, canonical compounds, aligned zero fields, `1h + 30m + 30s`, ordinary `u64` calls/comparisons, and trapping arithmetic overflow; verify it with `Analysis.evaluate` and add a Wasm leg only for the structural claim that existing `u64` lowering is reused.
- [x] 3.5 Exercise `MonotonicClock.waitFor` with duration literals through its existing `u64` contract and verify no intrinsic, runtime ABI, native backend, standard-library provider, or compiler-known actor change is required.

## 4. Reference Documentation and Generated Artifacts

- [x] 4.1 Add the duration token grammar, suffix table, committed malformed-token behavior, and canonical compound rules to the prescriptive lexical reference; verify every accepted and rejected documentation example agrees with the compiler diagnostics.
- [x] 4.2 Document fixed `u64` nanosecond typing, exact unit scales, fixed day/week meaning, range limit, ordinary arithmetic behavior, and the lack of dimensional identity in the values and expression references; verify `MonotonicClock.waitFor` examples use valid duration literals without claiming calendar semantics.
- [x] 4.3 Regenerate diagnostic and standard-library documentation artifacts with `pnpm --filter @silklang/compiler documentation:generate`, then verify them with `pnpm --filter @silklang/compiler documentation:check` and the documentation example checks.

## 5. Repository Verification

- [x] 5.1 Run `pnpm typecheck` and resolve every exhaustive token, syntax, fact, encoding, and inspector switch without casts, non-null assertions, suppressions, or compatibility fallbacks.
- [x] 5.2 Run `pnpm exec biome check .` after type checking and resolve all formatting and lint findings.
- [x] 5.3 Run `pnpm test` after Biome and confirm the language, formatter, documentation, evaluator, Wasm, and designated native acceptance suites pass.
- [x] 5.4 Run `pnpm check` as the required repository handoff gate and report any failure exactly, including whether it predates this change.
- [x] 5.5 Run `pnpm release:candidate` because compiler package contents change, and verify package assembly and release-candidate validation remain clean.
