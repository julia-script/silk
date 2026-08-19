## 1. Layered catalog and scope

- [ ] 1.1 Extend catalog generation with stable source identity, digest, docs, layer, provider, and runtime metadata.
- [ ] 1.2 Enforce portable-to-provider dependency direction and deterministic catalog validation.
- [ ] 1.3 Build module closure and scope only from explicit imports and delete implicit prelude injection.
- [ ] 1.4 Make exact duplicate, unchanged-alias, and combinable imports compiler-valid and remove semantic redundancy diagnostics.

## 2. Tooling and source migration

- [ ] 2.1 Index catalog declarations for completion without injecting them into scope.
- [ ] 2.2 Insert module-qualified imports with deterministic collision-aware aliases.
- [ ] 2.3 Add LSP-only redundancy warnings and consolidation fixes.
- [ ] 2.4 Add Effect failure/requirement propagation, recovery, provision, and missing-import actions.
- [ ] 2.5 Migrate every compiler, stdlib, example, fixture, and documentation source to explicit imports.

## 3. Runtime inventory and verification

- [ ] 3.1 Derive executable support from reachable intrinsics and structured reporting inventory.
- [ ] 3.2 Add layer violations, no-prelude, redundancy-validity, collision completion, code-action, and pay-for-use tests.
- [ ] 3.3 Update canonical specs, language docs, generated catalogs/docs, and diagnostics.
- [ ] 3.4 Run typecheck, Biome, compiler/LSP tests, full tests, release-candidate checks, and `pnpm check`.
