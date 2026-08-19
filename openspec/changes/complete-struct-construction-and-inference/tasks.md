## 1. Field-based construction

- [ ] 1.1 Resolve every initializer name to a canonical field and validate that field's visibility.
- [ ] 1.2 Replace module-wide construction authorization with required-field completeness plus per-field access.
- [ ] 1.3 Preserve private-field factory boundaries and deterministic missing/duplicate/invisible diagnostics.

## 2. Generic inference

- [ ] 2.1 Collect ordinary parameter constraints from every supplied field after explicit type-argument prefixes.
- [ ] 2.2 Solve constraints together with forward-only call-compatible rules.
- [ ] 2.3 Publish inferred parameters and field resolutions for HIR, completion, hover, and signature help.
- [ ] 2.4 Remove incomplete struct-only inference and source-order selection paths.

## 3. Verification

- [ ] 3.1 Add external public construction, private field rejection, factories, reordered fields, explicit prefixes, conflicts, and ambiguities.
- [ ] 3.2 Update canonical specs, diagnostics, language docs, and existing construction fixtures.
- [ ] 3.3 Run typecheck, Biome, focused compiler/LSP tests, full tests, and `pnpm check`.
