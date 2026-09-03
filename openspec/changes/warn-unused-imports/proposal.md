# Warn on unused imports

## Why

Valid but unused import bindings obscure module dependencies and stale scope. Tooling needs source-binding-aware facts so aliases of one declaration remain independent.

## What Changes

- Add compiler-owned authored import-binding use facts and snapshot-current removal plans.
- Publish stable LSP0004 warnings and deterministic quick fixes where trivia ownership is unambiguous.
- Document that the warning is tooling-only and imports have no runtime behavior.

## Impact

Compiler analysis/tooling API, LSP diagnostics and code actions, module reference, and focused tests change. Compiler diagnostic severity and successful compilation do not.
