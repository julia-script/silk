# Design: Add VS Code Inspector Panel

## Context

See proposal.md for motivation. Current state that shapes the design:

- `apps/docs/app/labs/registry.tsx` defines 19 views as pure projections
  `(ViewContext) => { rows, meta, facts, unavailable }`; `row/project-backend.tsx`,
  `row/project-syntax.tsx`, and `row/flow-model.ts` build the rows. The only impurity is the
  `onSelectSpan`/`onActivate` closures baked into rows; `RowModel` already carries `span` as data.
- Spans are bare `{start, end}` byte ranges — workable in labs because the cursor lives in one
  scratch program, insufficient for a real workspace where closure/index/resolution rows point
  into other files.
- `packages/lsp` already owns the analysis of the open project: `ProjectSession` commits an
  `AnalyzedDocument` (`ProjectAnalysis.View`, a `FrontendSnapshot`) per revision, debounced,
  latest-wins. Backend phases need the single-root `Analysis.Snapshot` realization, which the
  server does not currently build.
- `apps/vscode` is a thin language-client wrapper installed by local symlink.
- The facade rule (bootstrap-analysis-facade): tooling value-imports only `Analysis` queries and
  data-model types, never phase modules.

## Goals / Non-Goals

**Goals**

- One serialization-safe projection library with a single consumer-facing contract for both the
  docs workbench and the language server.
- The LSP stays the only process that runs the compiler against the workspace.
- The panel is deliberately minimal: one webview, one view, native VS Code arrangement.

**Non-Goals**

- No dockview/multi-pane layout in the extension; users split with VS Code's own editor groups.
- No marketplace packaging; the extension stays local-install.
- No change to labs' saved layouts, URL encoding, or presets.
- No cross-module cursor *navigation* added to the docs workbench (rows gain module identity, but
  the workbench keeps its current single-cursor behavior).

## Decisions

### D1: Compiler-owned inspector projection actors

The registry (`ViewDefinition`, `views`, `viewById`, `siblingsOf`), row model types, and the
`project-*`/`flow-model` row builders move here. Depends only on `@silklang/compiler`.

*Alternative — fold into the compiler's tooling namespace:* rejected; row models and fact strips
are a presentation vocabulary, not compiler analysis, and the compiler package should not grow a
UI-shaped surface.

### D2: Rows become pure data; activation is the consumer's job

`RowModel.onActivate` and the `onSelectSpan` parameter threading disappear from the projections.
Rows carry `span?: { module: string; start: number; end: number }` (D3) and a row is activatable
exactly when it has a span. Consumers wire activation: the docs row list calls its cursor atom,
the webview posts a message. `ViewContext` correspondingly drops its callbacks (`onSelectSpan`,
`onEvaluate`); the evaluation action becomes a request/context input (`evaluate: boolean`), which
also matches the LSP request shape.

*Alternative — keep callbacks and strip them at the wire:* rejected; two contracts for the same
rows, and the labs "activatable = has onActivate" rule would silently diverge from the wire rule.

### D3: Spans are module-qualified

`Span` gains the module name. This is the one behavior-adjacent change to the projections: cursor
tinting compares module + range. The labs workbench passes its active module when setting the
cursor from the editor; rows in other modules stop being tinted by a same-offset cursor, which is
a correctness fix, not a regression. In the extension, module names map to file URIs via the
`moduleUris` map the LSP already keeps on `AnalyzedDocument`.

### D4: LSP protocol — request/notification pair

- `silk/inspectorView` request: `{ uri, viewId, filter?, showTrivia?, evaluate? }` →
  `{ rows, meta?, facts?, unavailable? } | error`. Rows are the serialized package output; the
  response also carries the analysis revision it was projected from.
- `silk/inspectorInvalidated` notification: `{ workspace, revision }`, emitted from the session's
  publish path the same way diagnostics are.

Frontend views project straight off the committed `ProjectAnalysis.View`. Backend views
(`layout`, `mir`, `backend`, `toolchain`, `evaluation`, and the backend half of `pipeline`)
realize `Analysis.Snapshot` rooted at the request's document, cached in a
`(revision, rootModule) → snapshot` map cleared on commit. Evaluation and wasm
execute-and-compare run in the server process (Node has `WebAssembly`) only when
`evaluate: true`.

*Alternative — run the compiler in the extension host or webview:* rejected; duplicates
ProjectSession's discovery/debounce/watch machinery, and webviews cannot read the workspace.

### D5: Webview is a dumb renderer, no framework

The panel bundles no React: a few hundred lines of DOM rendering the row grammar (lead, caret,
dot, label, detail, tone) plus the phase picker, styled with `--vscode-*` theme variables.
`retainContextWhenHidden` keeps picker/filter state; content state is always re-requestable from
the server, so a disposed panel loses nothing. The extension host mediates all messages: webview
→ host (`pickView`, `activateRow`, `setFilter`, `evaluate`) and host → webview (`viewResult`,
`cursor`).

*Alternative — share the labs React components:* rejected; RowList/pane chrome are entangled
with atoms and dockview, and a single static list does not need a framework.

### D6: Follow-active-editor, sticky on blur

The panel tracks `window.onDidChangeActiveTextEditor`, re-rooting on `.silk` editors and holding
the last document otherwise (spec'd). Editor selection → `document.offsetAt` → module-qualified
span → `cursor` message; row activation → host resolves module → URI via a `silk/moduleUris`
lookup carried on the view response → `showTextDocument` + `revealRange`. A pin toggle is
deliberately deferred (see Open Questions).

## Risks / Trade-offs

- [Extraction churn: ~19 views and 3 row-builder modules move and lose callbacks, touching every
  labs pane] → The existing labs tests (`registry.test.ts`, `row/*.test.tsx`, presets/facade
  tests) move with the code; the workbench keeps thin adapter wiring, and the facade-boundary
  test extends to the new package.
- [Backend realization cost on large projects: a keystroke storm could queue realizations] →
  Realize lazily only while a backend view is open, per committed revision (debounce already
  coalesces edits), cache per root.
- [Row volume over JSON-RPC: hex dumps and token streams can be thousands of rows] → Rows are
  small flat objects; if a view proves heavy, cap rows server-side with a "truncated" fact rather
  than paginating the protocol now.
- [Webview drift from labs styling: two renderers of one row grammar] → Both consume the same
  row model; the webview intentionally re-skins with VS Code theme tokens, so drift is cosmetic
  by design.

## Migration Plan

Land in three PR-sized steps, each green on its own: (1) extract the package + labs consumes it
(pure refactor plus module-qualified spans), (2) LSP request/notification with tests against
fixture workspaces, (3) extension panel. No release migration — everything affected is
unreleased.

## Open Questions

- Pin-the-panel-to-a-document toggle: deferable UX addition; follow-active is the spec'd
  baseline.
- Whether the docs workbench later adopts cross-module navigation now that rows carry module
  identity — out of scope here.
