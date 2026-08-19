## Why

Silk currently mixes structural Copy inference with blanket move-only rules for nominal and executable values. The confirmed model has one sealed, non-customizable Copy property, and stored callable or Effect values follow ordinary aggregate ownership derived from their realized fields.

## What Changes

- Admit `impl Copy` only when every stored field is Copy and neither the type nor any field requires cleanup.
- Make Copy a sealed compiler-validated property with no operation body and reject conflicts with `Drop`.
- Derive array, union, generic, layout, move, and diagnostic behavior from that one property.
- Derive represented callable and Effect aggregate ownership from realized fields, allow ordinary partial moves, and retain access-specific capture restrictions.
- Remove structural-copy and always-move-only fallback paths.

## Capabilities

### Modified Capabilities

- `bootstrap-ownership`: define one authoritative Copy category and ordinary executable-field ownership.
- `bootstrap-complete-interface-contracts`: validate sealed `Copy` conformances and reject operation bodies.
- `bootstrap-type-generics`: solve Copy bounds from the authoritative property.
- `bootstrap-fixed-arrays`: derive array Copy from elements.
- `bootstrap-structural-unions`: derive union Copy and cleanup from members.
- `bootstrap-nominal-callable-storage`: use ordinary aggregate ownership for represented callables.
- `bootstrap-nominal-effect-storage`: use ordinary aggregate ownership for represented Effects.

## Impact

Depends on `enforce-return-contract-soundness`. It changes conformance validation, ownership classification, layout facts, generic evidence, cleanup, HIR/MIR verification, diagnostics, and tests. Users cannot customize duplication and allocated or droppable values remain non-Copy.
