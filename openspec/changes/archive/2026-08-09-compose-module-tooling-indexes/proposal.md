## Why

Project revisions now avoid repeated elaboration and ownership work, but they still rebuild every
semantic-occurrence and anonymous-expression index across the whole module closure. These editor
indexes already have a natural module boundary, so composing them from reusable module artifacts is
the next safe step toward edit-local LSP work.

## What Changes

- Introduce one immutable module tooling artifact that owns a module's semantic-occurrence index and
  anonymous-expression index.
- Reuse the exact prior tooling artifact whenever the current project structurally shares that
  module's semantic artifact; recompute tooling only for modules with new semantics or missing prior
  tooling.
- Compose project-wide query indexes from current module artifacts without rescanning reusable
  elaboration results.
- Resolve declaration navigation through a current-project location registry so a reused importer
  never exposes a predecessor dependency span.
- Report deterministic reused/recomputed module counts for both tooling phases.

## Capabilities

### New Capabilities

- `bootstrap-module-tooling`: Defines independently reusable module-local editor indexes and their
  current-project composition contract.

### Modified Capabilities

- `bootstrap-project-analysis`: Project revisions structurally share reusable tooling artifacts and
  expose deterministic tooling reuse observations instead of rebuilding all tooling globally.

## Impact

The compiler frontend tooling, semantic occurrence index, project analysis publication, phase
observations, compiler/LSP tests, public compiler exports, release-candidate manifest, and OpenSpec
contracts are affected. Query semantics stay unchanged; the public tooling data model gains explicit
module artifacts and composition operations.
