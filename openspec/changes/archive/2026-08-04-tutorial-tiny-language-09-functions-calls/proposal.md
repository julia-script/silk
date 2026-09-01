## Why

A useful small language needs user-defined functions rather than a single hard-coded entry body. Two-pass function lowering teaches symbol resolution while naturally enabling forward references and recursion.

## What Changes

- Add a first pass that validates and declares all function signatures.
- Add a second pass that maps parameters, builds bodies, and resolves direct calls.
- Validate duplicate functions, unknown names, wrong arity, and the required zero-argument `main`.
- Support forward calls and self-recursive calls through `FunctionBody.callDirect`.
- Add reordered-definition and factorial validation fixtures.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Extends tutorial compiler state, diagnostics, fixtures, and tests. It relies on existing declaration and direct-call APIs without modifying the package.
