## Why

`silk format` currently treats declaration and module documentation comments as opaque tokens, so
Silk programs inside fenced documentation examples can drift away from the language's canonical
layout even when the surrounding `.silk` file is formatted. Formatting should make every active
Silk program embedded in a source file canonical while retaining the documentation prose and fence
structure around it.

## What Changes

- Format syntactically complete fenced code blocks whose CommonMark language word identifies active Silk code
  inside attached `///` documentation and leading `//!` module documentation.
- Rewrite the fenced bodies in the original `.silk` source, preserving documentation attachment,
  prose, fence spelling, Markdown container structure, comment markers, and all non-Silk blocks.
- Leave `silk,ignore` blocks unchanged because they deliberately permit non-module fragments, treat
  metadata-bearing `silk` language fences consistently with doctest, and
  reject syntactically damaged active `silk` blocks with source-located formatter failure rather
  than partially rewriting the file.
- Apply the same document-local formatting operation in `silk format` and whole-document LSP
  formatting, including check mode, changed detection, and idempotence.
- Introduce an optional formatter package above the compiler and documentation packages so the
  canonical source formatter can reuse CommonMark interpretation without making ordinary compiler
  analysis depend on Markdown.
- **BREAKING**: Replace the compiler package's public canonical `Formatter` surface with an
  explicitly lower-level syntax-layout actor, and publish the documentation-aware canonical
  formatter from the formatter package. All repository callers move together.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `silk-source-formatting`: Extend canonical whole-source formatting to format active Silk fenced
  bodies inside source-owned documentation comments, define ignored and damaged-block behavior,
  and preserve one result across CLI and LSP consumers.

## Impact

- Affects `packages/compiler` formatter naming and exports, documentation parsing/rewrite support,
  CLI formatting, LSP whole-document formatting, and their tests.
- Adds a workspace formatter package depending on `@silklang/compiler` and
  `@silklang/docgen`; ordinary compiler parsing and analysis retain no Markdown runtime
  dependency.
- Changes canonical bytes for `.silk` files whose active fenced examples are not already formatted,
  including generated standard-library source inventory when those files change.
- Requires package export, changeset, full repository verification, and release-candidate checks.
