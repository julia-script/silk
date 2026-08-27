# Add VS Code Inspector Panel

## Why

The `/labs` workbench makes every compiler phase inspectable, but only for scratch programs typed
into the docs site. Compiler developers working on real `.silk` files in Cursor/VS Code have no way
to see how the open project moves through the pipeline — the language server already computes the
analysis for the workspace, but nothing surfaces it beyond hovers and diagnostics. The labs view
projections are also trapped inside `apps/docs`, baked into React with callbacks, so no other
consumer can reuse them.

## What Changes

- Extract the labs view projections (`registry` view definitions and the `row/project-*` row
  builders) out of `apps/docs/app/labs` into a new shared workspace package, with rows as pure
  serializable data: activation callbacks are replaced by module-qualified spans
  (`{module, span}`) that consumers turn into navigation themselves.
- The docs workbench keeps its current behavior but consumes the shared package instead of local
  modules; only the React shells (row list, pane chrome, phase picker, editor) stay in `apps/docs`.
- The language server answers a new custom request that projects any inspector view for a document
  from its committed `AnalyzedDocument`, and notifies the client when a newer analysis commits so
  an open view can refresh. Backend views (layout, MIR, backend, toolchain, evaluation) realize a
  single-root snapshot on demand, rooted at the requested document.
- The VS Code extension gains a "Silk Inspector" webview panel: one panel, one view at a time,
  with an in-panel phase picker. It follows the active `.silk` editor, syncs the span cursor in
  both directions (editor selection tints rows; clicking a row reveals the span in the editor),
  and renders with editor theme variables.

## Capabilities

### New Capabilities

- `inspector-views`: a shared package of compiler-phase view projections — the view registry and
  per-phase row projections producing serializable row models with module-qualified spans,
  consuming the compiler exclusively through the `Analysis` facade. Consumed by the docs
  workbench and the language server.
- `language-server-inspection`: the language server's inspector surface — a custom request that
  answers a projected view (rows, meta, facts, unavailability) for a document and view id, an
  invalidation notification when a newer analysis commits, and on-demand single-root realization
  for backend views.
- `extension-inspector-panel`: the single-tab Silk Inspector webview in the VS Code extension —
  open command, phase picker, active-editor following, bidirectional span-cursor sync, and
  editor-theme rendering.

### Modified Capabilities

<!-- none: the docs workbench's observable behavior is unchanged (the extraction is internal),
     and the existing cursor-extension spec (language registration + local install) is untouched;
     the panel is specified as its own capability. -->

## Impact

- New package: `packages/inspector` (`@silk-lang/inspector`), depending only on
  `@silk-lang/compiler`.
- `apps/docs/app/labs`: registry and `row/project-*` modules move out; imports and the
  `onActivate` wiring change, behavior does not.
- `packages/lsp`: new request/notification handlers in `Server.ts`; single-root realization
  cached per committed revision.
- `packages/vscode`: webview panel, commands, cursor sync; extension stays local-install only.
- No published-release impact: all affected packages are private/unreleased.
