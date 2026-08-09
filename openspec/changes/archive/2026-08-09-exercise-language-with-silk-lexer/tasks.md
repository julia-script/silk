## 1. Visible pressure program

- [x] 1.1 Add the visible lexer example, README, manifest, and initial categorized findings report
- [x] 1.2 Implement local token and diagnostic records plus complete byte classification in ordinary Silk
- [x] 1.3 Return owned token and diagnostic vectors through the public allocator service and verify deterministic cleanup in the example entry point

## 2. Differential evidence

- [x] 2.1 Add a centralized complete mapping between the example's local token codes and canonical TypeScript token kinds
- [x] 2.2 Add table-driven valid corpus coverage for trivia, keywords, numeric and static-text literals, punctuation, and EOF
- [x] 2.3 Add invalid corpus coverage comparing invalid-token and lexical-diagnostic spans while proving recovery to later tokens

## 3. Engine and ownership evidence

- [x] 3.1 Add representative valid and invalid evaluator, native LLVM, and direct WebAssembly fingerprint parity
- [x] 3.2 Sweep every exercised allocation-failure ordinal and assert typed failure plus acquire/release equality and repeat-run determinism
- [x] 3.3 Add fresh-process artifact determinism and assert published MIR/backend forms contain no lexer- or token-specific primitive

## 4. Findings and verification

- [x] 4.1 Complete the findings report with evidence and dispositions in every required category
- [x] 4.2 Run focused acceptance tests, `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
