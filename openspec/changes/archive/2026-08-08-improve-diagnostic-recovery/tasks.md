## 1. Diagnostic Vocabulary

- [x] 1.1 Add exhaustive source-language token descriptions and use them in missing-token messages
- [x] 1.2 Add the construct-level missing-return diagnostic and rename `SEM0006` APIs and reason data to unknown-value terminology

## 2. Parser Recovery

- [x] 2.1 Parse empty input as a source file containing only end-of-file
- [x] 2.2 Gate identifier-led assignment parsing on a complete place followed by `=` and recover a final identifier as a missing-keyword return
- [x] 2.3 Retain a wholly missing return structure while emitting only its construct-level diagnostic
- [x] 2.4 Track parser recovery episodes so dependent diagnostics are suppressed until a concrete expected token synchronizes parsing

## 3. Semantic Cascades and Consumers

- [x] 3.1 Suppress invalid-place diagnostics for destinations already unavailable from parser or name-resolution causes in assignments and `Place.replace`
- [x] 3.2 Update compiler documentation and Syntax Inspector presets for empty modules, recovery behavior, and unknown-value terminology

## 4. Tests and Verification

- [x] 4.1 Add focused parser, diagnostic, elaboration, and facade regression coverage for both reported examples and preserved valid assignments
- [x] 4.2 Validate the OpenSpec change and run typecheck, Biome, tests, `pnpm check`, and the release-candidate check
- [x] 4.3 Add incomplete-prefix, post-synchronization, and exact LSP range regression coverage, then rerun focused and repository verification
