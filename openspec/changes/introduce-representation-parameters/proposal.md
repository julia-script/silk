## Why

Silk preserves callable and Effect construction identities through direct higher-order calls, but
ordinary nominal applications can carry only type arguments. The compiler needs one explicit,
kinded representation-argument model before any nominal field may retain executable values safely.

## What Changes

- Add callable- and Effect-bounded representation parameters to generic declarations.
- Generalize complete nominal applications from type-only arguments to ordered, kind-checked type,
  row, and representation arguments.
- Separate concrete representation identity and intrinsic contract from the parameter's required
  bound and per-use admissibility proof.
- Preserve open and concrete representation arguments through analysis, HIR, instance keys,
  diagnostics, deterministic encoding, and tooling.
- Keep callable and Effect storage fences active; this change introduces the source and
  specialization substrate but does not yet admit runtime nominal storage.

## Capabilities

### New Capabilities

- `bootstrap-representation-parameters`: Representation kinds, arguments, substitution, equality,
  inference, joins, presentation, and specialization boundaries.

### Modified Capabilities

- `bootstrap-type-generics`: Generic declarations and nominal applications are no longer limited to
  ordinary types and contract rows.

## Impact

Touches syntax, declaration facts, semantic types, generic inference/substitution, module semantic
surfaces, HIR, instance keys, diagnostics, inspectors, and deterministic encoders. This is the
prerequisite for every other static-composition proposal and deliberately retires no runtime fence.
