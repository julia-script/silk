## Why

Silk can now resolve behavior across modules and express scalar algorithms, but it cannot name the
compiler-shaped aggregate data those algorithms need. Nominal struct declarations must establish
canonical type identity, visibility, field contracts, and target-aware physical layout before value
construction adds ownership and runtime pressure.

## What Changes

- Add lossless, recoverable `pub struct Name { ... }` declarations whose fields are private by
  default and may be explicitly public.
- Add canonical nominal struct identities and ordered field facts to the closure-wide declaration
  index, sharing the existing top-level namespace with functions and imports.
- Resolve struct types in field and function contracts through local, selected, and qualified
  module bindings without introducing structural conformance or implicit imports.
- Reject duplicate fields, private-type exposure, and direct or mutual inline recursive layout with
  stable semantic diagnostics and explicit unavailable facts.
- Extend the backend-neutral target-layout phase with deterministic struct size, alignment, padding,
  and physical field offsets in declaration order.
- Expose struct declarations, type resolution, dependency failures, and target layouts through the
  analysis facade and facade-only inspector labs.
- Keep struct literals, field projection, partial moves, aggregate ABI lowering, and runtime struct
  values for the following `construct-and-project-struct-values` change.

## Capabilities

### New Capabilities

- `bootstrap-struct-types`: Nominal struct identity, visibility, ordered typed fields, dependency
  validation, inline-recursion refusal, and deterministic target-aware layout.

### Modified Capabilities

- `bootstrap-syntax`: Parse and recover struct declarations and field lists losslessly.
- `bootstrap-declaration-index`: Collect function and struct headers into one canonical,
  kind-aware declaration table before body resolution.
- `bootstrap-name-resolution`: Resolve local and imported nominal types in declaration contracts
  through the existing explicit module scopes and visibility rules.
- `bootstrap-target-layout`: Plan reachable nominal struct layouts recursively from their field
  types while retaining one backend-neutral target-owned layout authority.
- `bootstrap-analysis-facade`: Publish immutable struct, type-dependency, and layout queries from
  the shared snapshot.
- `bootstrap-syntax-inspector`: Inspect nominal declarations, field facts, dependency diagnostics,
  and physical layouts without recreating compiler semantics.

## Impact

The compiler's token, concrete syntax, parser, declaration index, name-resolution, diagnostic,
target-layout, driver, and analysis actors will gain nominal-type data. Public compiler exports and
facade query shapes will expand, and the unified `/labs` declaration and target-layout panes plus
deterministic goldens will cover the new facts. No external runtime dependency or backend-owned
layout logic is introduced.
