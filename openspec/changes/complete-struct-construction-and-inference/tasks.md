## 1. Field-based construction

- [x] 1.1 Resolve every initializer name to a canonical field and validate that field's visibility.
- [x] 1.2 Replace module-wide construction authorization with required-field completeness plus per-field access.
- [x] 1.3 Preserve private-field factory boundaries and deterministic missing/duplicate/invisible diagnostics.

## 2. Generic inference

- [x] 2.1 Collect ordinary parameter constraints from every supplied field after explicit type-argument prefixes.
- [x] 2.2 Solve constraints together with forward-only call-compatible rules.
- [x] 2.3 Publish inferred parameters and field resolutions for HIR, completion, hover, and signature help.
- [x] 2.4 Remove incomplete struct-only inference and source-order selection paths.

## 3. Verification

- [x] 3.1 Add external public construction, private field rejection, factories, reordered fields, explicit prefixes, conflicts, and ambiguities.
- [x] 3.2 Update canonical specs, diagnostics, language docs, and existing construction fixtures.
- [x] 3.3 Run typecheck, Biome, focused compiler/LSP tests, full tests, and `pnpm check`.
