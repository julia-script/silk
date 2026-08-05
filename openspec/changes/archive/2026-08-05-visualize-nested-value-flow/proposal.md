## Why

Nested calls can now produce correct semantic facts, results, and traces, but the inspector's first
flat value-flow path cannot clearly show one call result becoming another call's argument. A small
nested projection gives immediate visual feedback for composition without adding another compiler
representation or jumping ahead to control flow.

## What Changes

- Extend the Syntax Inspector's value-flow projection to group expressions by nested call site and
  connect inner results through outer arguments, positional parameters, reads, and returns.
- Distinguish static semantic relationships from the reachable order and exact values supplied by
  an explicit evaluation outcome.
- End incomplete or blocked branches at their exact known state instead of drawing a successful
  enclosing result that never existed.
- Keep every group, node, edge, state, value, and source range available through an ordered
  accessible text representation and synchronized source selection.
- Add completed, sibling, unavailable, wrong-arity, and cyclic nested-flow presets that remain
  browser-local and disposable.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax-inspector`: Extend the existing value-flow view and presets from one flat path
  to nested semantic and evaluated value flow.

## Impact

This changes only the hidden docs inspector's browser-side projection, rendering, accessibility
structure, presets, and tests. It consumes existing syntax, semantic, evaluation, and trace data;
it does not change the language, compiler package API, evaluator, or persistence behavior.
