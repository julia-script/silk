## Why

An unresolved Silk name currently leaves the author to discover the exporting module and edit an
import declaration by hand, even though the compiler already knows the reference context and the
language server already delivers quick fixes. Auto-import should turn that recovery path into a
deterministic code action without expanding every editor revision into full semantic analysis of
every source-root file.

## What Changes

- Add auto-import quick fixes for unresolved value, type, actor, and other importable top-level
  name references when one or more visible modules export the required spelling.
- Discover candidates from an in-memory, incrementally revised workspace inventory built from
  immutable per-module import/export summaries, separate from the open-root project analysis.
- Validate candidates against the requesting module's semantic context, visibility, existing
  bindings, and importability; return distinct, deterministically ordered actions when several
  modules remain valid.
- Add or merge selected-member imports through syntax-aware import planning that preserves valid
  existing imports and source trivia.
- Generalize compiler-owned source actions from isolated diagnostic replacements to atomic change
  plans, establishing the seam later multi-edit and multi-file refactors can reuse.
- Keep dependency catalogs, workspace-wide reference indexing, disk-persistent indexes, automatic
  import organization, and inferred function-contract refactors outside this change.

## Capabilities

### New Capabilities

- `language-server-auto-import`: Candidate discovery, applicability, ranking, import planning,
  incremental workspace inventory, revision coherence, and LSP delivery for auto-import actions.

### Modified Capabilities

- `language-server-code-actions`: Permit compiler-owned candidate-generating source actions in
  addition to diagnostic-carried edits, and replace the existing requirement that unresolved names
  never produce a quick fix.

## Impact

- Affects compiler analysis/tooling facades and the language-server workspace, project session,
  document, and protocol adapters.
- Introduces compiler-owned source-action and import-planning actors plus a project-scoped
  language-server inventory of source-root module summaries.
- Changes the internal code-action representation; backward compatibility is intentionally not
  preserved during the project's alpha stage.
- Adds no runtime, backend, package, or network dependency and does not widen the accepted semantic
  project revision beyond open roots and their transitive import closure.
