## Why

Public struct construction is currently gated by module-wide authorization rather than the visibility of each initialized field, and ordinary generic struct arguments are not inferred as completely as call arguments. Both behaviors contradict the confirmed field-based, forward-only struct model.

## What Changes

- Permit construction from any module when every explicitly initialized field is visible.
- Preserve private-field construction fences and factory functions without granting access merely because another field is public.
- Infer omitted ordinary generic arguments from all supplied fields using the same forward-only conflict rules as calls and represented fields.
- Preserve explicit generic prefixes and produce deterministic missing, conflict, ambiguity, and invisible-field diagnostics.
- Update construction facts, specialization identities, tooling, and tests atomically.

## Capabilities

### Modified Capabilities

- `bootstrap-struct-values`: make construction authorization field-based and visibility-sensitive.
- `bootstrap-struct-types`: preserve private representation boundaries while admitting public field construction.
- `bootstrap-type-generics`: infer omitted ordinary struct parameters from supplied fields.
- `bootstrap-semantic-facts`: publish resolved field and type-argument evidence for tooling and lowering.

## Impact

This batch has no semantic prerequisite beyond the stabilized reference and may run after return soundness. It changes name resolution, type inference, construction validation, diagnostics, language-service facts, and tests; it adds no positional constructor or reflection.
