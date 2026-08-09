## Why

The language server still constructs one complete frontend snapshot per open document, so shared
dependencies are loaded, parsed, indexed, elaborated, and ownership-checked repeatedly within the
same accepted project revision. After removing runtime realization from editor requests, this
open-roots multiplication is the next measured architectural cost boundary to remove.

## What Changes

- Introduce a compiler-owned project frontend analysis that accepts multiple canonical roots,
  loads their union dependency closure once, and computes each module's frontend facts once.
- Derive immutable root views from the project analysis so existing module-qualified Analysis
  queries remain coherent while root identity stays explicit.
- Make `ProjectSession` schedule one whole-project analysis operation per accepted revision instead
  of invoking analysis independently for every synchronized document.
- Commit the analyzed-document map atomically exactly as today; protocol requests continue to use
  document text, line indexes, module identities, and semantic facts from one revision.
- Expose deterministic work/reuse observations that prove shared modules are processed once within
  a revision and that distinct root views reference the same immutable project facts.
- Keep cross-revision memoization, dependency-aware invalidation, public-header/body invalidation,
  incremental parsing, and edit-stable syntax correspondence out of this change.

## Capabilities

### New Capabilities

- `bootstrap-project-analysis`: construct one immutable multi-root frontend analysis and derive
  coherent root views that structurally share compiler-owned module facts.

### Modified Capabilities

- `language-server-synchronization`: analyze and atomically commit each accepted project revision
  through one shared project computation rather than one computation per open document.

## Impact

- Compiler closure and frontend orchestration gain a multi-root project entry point and immutable
  root-view model in `packages/compiler/src`.
- `Workspace` and `ProjectSession` change their internal analysis contract from per-document calls
  to one project-revision call returning the complete analyzed-document map.
- LSP scheduling, overlay precedence, version matching, diagnostic publication, and protocol feature
  adapters retain their current externally observable behavior.
- Tests gain counters and identity assertions for shared-module computation plus existing
  latest-wins and stale-revision coverage.
