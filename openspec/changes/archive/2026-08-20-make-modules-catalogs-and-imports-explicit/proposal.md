## Why

The compiler catalog currently doubles as an implicit prelude, redundant imports are semantic errors, and tooling does not consistently insert the imports users need. The stabilized model separates distribution catalog, module scope, provider classification, and reachable runtime inventory; all source names are introduced explicitly.

## What Changes

- Extend the canonical catalog with source identity, digest, documentation, layer, and target-provider metadata without injecting declarations into module scope.
- Enforce portable-to-provider dependency direction and derive runtime support only from reachable intrinsics and report needs.
- Remove the implicit standard-library prelude and build closure and scope from explicit imports.
- Make exact duplicate, unchanged-alias, and combinable repeated imports compiler-valid; retain optional LSP warnings and fixes.
- Make completion discover catalog declarations and insert module-qualified, collision-aware imports.
- Add code actions for explicit Effect failure/requirement propagation, recovery, and provision.

## Capabilities

### Modified Capabilities

- `bootstrap-silk-stdlib`: separate catalog metadata, portable source, providers, and runtime inventory.
- `bootstrap-source-resolution`: resolve reserved distribution modules without making them ambient.
- `bootstrap-module-closure`: close programs from explicit imports only.
- `bootstrap-module-semantics`: keep redundant imports semantic and preserve all valid bindings.
- `language-server-completion`: discover catalog declarations and insert collision-aware imports.
- `language-server-code-actions`: add explicit Effect contract and import repairs.
- `bootstrap-backend`: retain only runtime support justified by reachable inventories.

## Impact

Depends on `normalize-effect-failure-types` and `normalize-effect-requirement-provision`. It migrates all repository source to explicit imports and changes catalog generation, resolution, scope, diagnostics, formatter, completion, code actions, runtime linkage, and tests. Re-exports and package distribution remain future work.
