## 1. Visible pressure program

- [ ] 1.1 Add the visible lexer example, README, manifest, and initial categorized findings report
- [ ] 1.2 Implement local token and diagnostic records plus complete byte classification in ordinary Silk
- [ ] 1.3 Return owned token and diagnostic vectors through the public allocator service and verify deterministic cleanup in the example entry point

## 2. Differential evidence

- [ ] 2.1 Add a centralized complete mapping between the example's local token codes and canonical TypeScript token kinds
- [ ] 2.2 Add table-driven valid corpus coverage for trivia, keywords, numeric and static-text literals, punctuation, and EOF
- [ ] 2.3 Add invalid corpus coverage comparing invalid-token and lexical-diagnostic spans while proving recovery to later tokens

## 3. Engine and ownership evidence

- [ ] 3.1 Add representative valid and invalid evaluator, native LLVM, and direct WebAssembly fingerprint parity
- [ ] 3.2 Sweep every exercised allocation-failure ordinal and assert typed failure plus acquire/release equality and repeat-run determinism
- [ ] 3.3 Add fresh-process artifact determinism and assert published MIR/backend forms contain no lexer- or token-specific primitive

## 4. Findings and verification

- [ ] 4.1 Complete the findings report with evidence and dispositions in every required category
- [ ] 4.2 Run focused acceptance tests, `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
