## Why

Silk standard-library declarations are ordinary language code, but their canonical source currently lives inside a JavaScript template string. Users cannot reliably inspect the shipped source, and go-to-definition invents a project path for definitions that do not exist there.

## What Changes

- Move each canonical standard-library module into a physical `.silk` file under a toolchain-owned source tree.
- Ship those files and a deterministic module manifest with the compiler package.
- Generate any embedded byte table from the canonical files and verify byte-for-byte equality.
- Preserve standard-library source origins through resolution and analysis.
- Resolve language-server definitions to the real shipped file URI.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-silk-stdlib`: Canonical standard-library source is shipped as physical `.silk` files rather than authored in JavaScript strings.
- `bootstrap-source-resolution`: Reserved standard-library modules resolve from a toolchain-owned source root with a real source location.
- `language-server-navigation`: Standard-library definitions navigate to the analyzed shipped source file.

## Impact

This changes compiler package contents, standard-library loading, resolved-source metadata, module closure snapshots, LSP URI selection, release-candidate checks, and tests. It intentionally does not change the contents or semantics of `silk/vector`.
