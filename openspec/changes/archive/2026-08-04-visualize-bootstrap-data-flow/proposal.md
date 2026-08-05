## Why

The inspector can show call and reference facts individually, but the first value-carrying program will be easier to understand as one connected path. A focused data-flow view gives immediate visual feedback before any execution machinery exists.

## What Changes

- Add an inspector data-flow view connecting caller arguments to target parameters and parameter references to returned values.
- Keep each node linked to exact declaration, call-site, argument, parameter, and reference spans.
- Distinguish complete, unavailable, missing, and ambiguous edges without inventing relationships.
- Provide a canonical identity-function preset plus malformed and unresolved variants.
- Keep the visualization derived entirely from existing semantic facts in browser memory.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax-inspector`: Add a navigable visual representation of the first argument-to-parameter value path and its incomplete states.

## Impact

This primarily changes the hidden docs inspector UI, styles, presets, accessibility behavior, and browser tests. Compiler semantics and public compiler APIs do not change; the view must consume facts created by the preceding changes and must not become a second analyzer.
