## 1. Layered catalog and scope

- [x] 1.1 Extend catalog generation with stable source identity, digest, docs, layer, provider, and runtime metadata.
- [x] 1.2 Enforce portable-to-provider dependency direction and deterministic catalog validation.
- [x] 1.3 Build module closure and scope only from explicit imports and delete implicit prelude injection.
- [x] 1.4 Make exact duplicate, unchanged-alias, and combinable imports compiler-valid and remove semantic redundancy diagnostics.

## 2. Tooling and source migration

- [x] 2.1 Index catalog declarations for completion without injecting them into scope.
- [x] 2.2 Insert module-qualified imports with deterministic collision-aware aliases.
- [x] 2.3 Add LSP-only redundancy warnings and consolidation fixes.
- [x] 2.4 Add Effect failure/requirement propagation, recovery, provision, and missing-import actions.
- [x] 2.5 Migrate every compiler, stdlib, example, fixture, and documentation source to explicit imports.

## 3. Runtime inventory and verification

- [x] 3.1 Derive executable support from reachable intrinsics and structured reporting inventory.
- [x] 3.2 Add layer violations, no-prelude, redundancy-validity, collision completion, code-action, and pay-for-use tests.
- [x] 3.3 Update canonical specs, language docs, generated catalogs/docs, and diagnostics.
- [ ] 3.4 Run typecheck, Biome, compiler/LSP tests, full tests, release-candidate checks, and `pnpm check`.
