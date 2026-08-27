## Why

The language server already exposes compiler diagnostics, type hover, document symbols, and
formatting, but every edit can launch overlapping full analyses for every open document and the
server offers no semantic navigation. Now that the analysis facade carries resolved declarations,
reference facts, and source spans, the LSP can become reliably useful without duplicating compiler
semantics or introducing incremental compilation prematurely.

## What Changes

- Add project-scoped analysis scheduling that coalesces edit bursts, bounds concurrent work, retains
  completed snapshots atomically while newer analysis runs, never combines current positions with
  older document state, and publishes results only for the latest synchronized document revisions.
- React to relevant on-disk Silk source and project-manifest changes so open-document analysis does
  not remain stale when closed dependencies change outside the editor.
- Add a position-oriented semantic target query to the compiler analysis facade, backed by resolved
  compiler facts rather than LSP-specific name resolution.
- Legalize nested lexical shadowing so resolved references and navigation select the nearest local
  declaration while same-block rebinding remains diagnosed.
- Add go-to-definition for supported local, parameter, declaration, imported, qualified, and member
  references, including cross-file locations.
- Establish reusable semantic target and location primitives on which document highlights,
  references, completion, and rename can be built in later changes; those later protocol features
  are not part of this change.
- Add protocol and concurrency tests for rapid edits, stale-result suppression, document closure,
  cross-file navigation, Unicode positions, filesystem invalidation, and multiple projects.

## Capabilities

### New Capabilities

- `language-server-synchronization`: Project-aware scheduling, snapshot consistency, overlay
  handling, and filesystem invalidation for synchronized Silk documents.
- `language-server-navigation`: Position-based semantic target resolution and go-to-definition
  behavior across local and project modules.

### Modified Capabilities

- `bootstrap-analysis-facade`: Expose editor-grade position and declaration-location queries while
  preserving the analysis facade as the sole semantic source of truth for tooling.

## Impact

- `packages/lsp`: server session lifecycle, workspace scheduling, document queries, protocol
  capabilities, filesystem watching, and integration tests.
- `packages/compiler`: analysis snapshot query surface, supporting immutable semantic index, and
  nested lexical value-resolution precedence.
- `apps/vscode`: client file-watching registration only if dynamic server registration is not
  sufficient; language behavior remains in the server.
- Public package subpaths may gain additive query types and functions. Nested lexical rebinding now
  shadows enclosing values; same-block duplicates remain invalid.
- The active first-class pipe-callables work may add new reference forms; navigation must consume
  recovered facts through `Analysis` so those forms can participate without protocol-specific
  semantic logic.
