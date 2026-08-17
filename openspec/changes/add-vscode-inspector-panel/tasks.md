# Tasks: Add VS Code Inspector Panel

## 1. Extract `@silk-effect/inspector`

- [x] 1.1 Scaffold `packages/inspector` (package.json, tsconfig, vitest config) depending only on `@silk-effect/compiler`, wired into the workspace and turbo
- [x] 1.2 Move row model types (`RowModel`, `Span`, tones, `cursorStateFor`, `spanLabel`) into the package; make `Span` module-qualified and rewrite `cursorStateFor` to compare module + range
- [x] 1.3 Move `row/project-syntax`, `row/project-backend`, and `row/flow-model` into the package, dropping the `onSelectSpan` parameters so rows carry spans only
- [x] 1.4 Move the view registry (`ViewDefinition`, `views`, `viewById`, `siblingsOf`, `panels.tsx` helpers) into the package; `ViewContext` loses callbacks, gains `evaluate` input; drop the React-only `source` view body handling from the shared registry
- [x] 1.5 Move and adapt the projection tests (`registry.test.ts`, `presets.test.ts` row assertions, row builder tests) into the package; add a serialization round-trip test over every view on a representative preset
- [x] 1.6 Extend the facade-boundary test to cover the new package's imports

## 2. Docs workbench consumes the package

- [x] 2.1 Rewire `apps/docs/app/labs` to import registry and rows from `@silk-effect/inspector`; keep React shells (RowList, pane chrome, picker, editor) local
- [x] 2.2 Wire activation in the row list from `span !== undefined` (posting to the cursor atom with the active module) instead of `onActivate`; thread the module-qualified cursor through `cursorAtom`
- [x] 2.3 Restore the evaluation "run" action through the new `evaluate` context input; verify labs behavior (views, meta, facts, cursor tinting, saved layouts/URLs) is unchanged and tests pass

## 3. Language server inspection

- [x] 3.1 Add the `silk/inspectorView` request handler in `Server.ts`: resolve the document's committed `AnalyzedDocument`, project frontend views from `ProjectAnalysis.View`, answer rows/meta/facts/unavailable plus revision and the module→URI map; explicit errors for unknown view ids and undiscovered documents
- [x] 3.2 Add single-root realization for backend views, cached per `(revision, rootModule)` and cleared on commit; evaluation and wasm execute run only when `evaluate: true`
- [x] 3.3 Emit `silk/inspectorInvalidated` from the session publish path with workspace + revision
- [x] 3.4 Server tests: frontend view over a fixture workspace, backend view rooted at a document, unknown-view error, invalidation on edit followed by a re-request answering the new revision, evaluation-only-when-asked

## 4. Extension panel

- [x] 4.1 Contribute the `silk.openInspector` command and a singleton `WebviewPanel` (beside, `retainContextWhenHidden`); reveal on re-run
- [x] 4.2 Build the webview renderer: row grammar + phase picker + filter/trivia controls in plain DOM, styled with `--vscode-*` variables; postMessage protocol (`pickView`, `activateRow`, `setFilter`, `evaluate` / `viewResult`, `cursor`)
- [x] 4.3 Host wiring: follow active `.silk` editor (sticky on blur), request views over `silk/inspectorView`, refresh on `silk/inspectorInvalidated`
- [x] 4.4 Cursor sync: editor selection → module-qualified span → tint; row activation → resolve module URI → `showTextDocument` + `revealRange`
- [ ] 4.5 Manual verification in the extension dev host against a real multi-file Silk workspace: phase switching, cross-module row navigation, theme switch, edit-refresh; update `packages/vscode/README.md`
