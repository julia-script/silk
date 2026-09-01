## 1. Contextual Import Paths

- [x] 1.1 Add a syntax-owned contextual import-path segment query and parser acceptance for reserved-word segments; verify parser tests cover `silk.effect`, interior reserved segments, lossless tokens, and ordinary reserved-word rejection outside paths
- [x] 1.2 Diagnose a reserved final segment without an alias or selected-member list; verify recovery retains the complete import and following declaration with one stable diagnostic
- [x] 1.3 Migrate module closure, summaries, name resolution, import planning, and LSP import inspection to the shared segment query; verify `silk.effect` resolves to canonical `silk/effect` through each affected artifact

## 2. Explicit Namespace Semantics and Tooling

- [x] 2.1 Remove distribution-catalog namespace seeding from semantic scope and migrate affected tests; verify an unimported operation namespace is unresolved while an explicit namespace import resolves normally
- [x] 2.2 Add generic catalog namespace inventory to applicable non-type completion from complete and partial spellings; verify `Eff` and non-type `Effect` offer `Effect` from `silk/effect` while declared-type and type-argument uses remain import-free
- [x] 2.3 Extend shared import planning with namespace requests, equivalent-import reuse, and deterministic collision aliases; verify completion produces `import silk.effect as Effect`, never a selected-member import, and does not duplicate an existing binding
- [x] 2.4 Verify qualified completion after the explicit `Effect` import exposes the public source operations of `silk/effect` without a spelling-specific semantic actor

## 3. Standard-Library Migration

- [x] 3.1 Rename the canonical Effect source and manifest entry from `silk/effects` to `silk/effect` with no compatibility entry; verify distribution resolution accepts only the singular identity
- [x] 3.2 Update all current source, tests, fixtures, proposal references, and documentation to the singular module identity and explicit required imports; verify no current distribution or executable fixture references `silk.effects`, `silk/effects`, or `effects.silk`
- [x] 3.3 Regenerate checked-in standard-library embedding, catalog, documentation, and package artifacts; verify generated-content and package-content checks match canonical `silk/effect`

## 4. Validation and Reconciliation

- [x] 4.1 Run focused parser, module, name-resolution, completion, auto-import, and standard-library tests and verify every new scenario passes
- [x] 4.2 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, and `pnpm test`; record and resolve every change-related failure
- [x] 4.3 Run `pnpm check` and `pnpm release:candidate`; verify the repository and package contents pass their required handoff gates
- [x] 4.4 Audit implementation, OpenSpec artifacts, and generated outputs against the canonical language rules; resolve any divergence
