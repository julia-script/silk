## 1. Shared pattern representation

- [ ] 1.1 Extract one syntax pattern tree shared by match arms, let, and if-let.
- [ ] 1.2 Build typed pattern facts for structure, exact member selection, bindings, access, coverage, and irrefutability.
- [ ] 1.3 Migrate existing nominal match behavior and tests onto the shared path before adding syntax.

## 2. Let and if-let

- [ ] 2.1 Parse, format, analyze, and lower recursive irrefutable let patterns.
- [ ] 2.2 Route standalone wildcard result discard through the explicit-drop diagnostic.
- [ ] 2.3 Parse, format, analyze, and lower statement-form if-let with optional mismatch body.
- [ ] 2.4 Implement move-on-both-outcomes, scoped borrow modes, cleanup, and post-statement ownership joins.

## 3. Ordinary union members and tooling

- [ ] 3.1 Generalize exact selectors and coverage to normalized nominal and non-nominal union members.
- [ ] 3.2 Require generic member distinctness before specialization and delete specialization-dependent behavior.
- [ ] 3.3 Add completion, hover, navigation, semantic highlighting, structure, and optional irrefutability warnings.
- [ ] 3.4 Add exhaustive, refutable, ownership, cleanup, union, and engine tests; update specs/docs/diagnostics.
- [ ] 3.5 Run typecheck, Biome, full tests, native acceptance only for target-specific cleanup, and `pnpm check`.
