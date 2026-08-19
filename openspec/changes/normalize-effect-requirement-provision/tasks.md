## 1. Requirement keys and algebra

- [ ] 1.1 Define canonical service-plus-role keys and separate access demands from identity.
- [ ] 1.2 Implement deterministic key normalization, union, repeated-demand merging, and subtraction.
- [ ] 1.3 Parse, resolve, format, and inspect `at` role selectors with collision diagnostics.
- [ ] 1.4 Migrate generic requirement facts and remove access-bearing legacy selector identities.

## 2. Provision and flattening

- [ ] 2.1 Select providers by key and validate shared, exclusive, and acquisition access afterward.
- [ ] 2.2 Align `provide`, `provideMut`, acquisition provision, and exact-key discharge.
- [ ] 2.3 Add `provideEffect`, migrate every caller, and delete `provideWith` without an alias.
- [ ] 2.4 Make `Effect.flatten` union both layers' requirements before normalization.

## 3. Verification

- [ ] 3.1 Add repeated service, role collision, insufficient access, acquisition, effectful provider, and nested-flatten tests.
- [ ] 3.2 Verify ownership loans and cleanup for every provider access mode.
- [ ] 3.3 Update stdlib source/generated artifacts, diagnostics, canonical specs, docs, and language-service facts.
- [ ] 3.4 Run typecheck, Biome, evaluator/Wasm tests, native corpus where provider ABI changes, full tests, and `pnpm check`.
