# Extract an embeddable Silk snippet element

## Why

Silk code shown outside the docs app is dead text: the static documentation site renders every
`silk` fence as an escaped `<pre><code>` with no highlighting, diagnostics, or hover — while a
fully working compiler-driven editor (highlight, diagnostics, hover, format) already exists in the
docs app's labs route, locked behind React. Extracting it as a framework-free custom element lets
any reporter that emits plain HTML embed live, compiler-accurate snippets with one script tag.

## What Changes

- New framework-free package exposing a `<silk-snippet>` custom element built on the existing
  CodeMirror integration (`@silk-effect/language`) and the synchronous language-server surface
  (`@silk-effect/lsp/Document`). The element compiles its own content in the browser exactly as
  doctest does: one snippet, one standalone module.
- Per-snippet feature flags as boolean attributes: syntax highlighting is always on; `diagnostics`,
  `hover`, `inlay-hints`, and `editable` are opt-in. Without semantic flags the element never
  compiles and stays a highlight-only view.
- Semantic features compile lazily (on visibility or first interaction), on the main thread — no
  worker, matching what labs already proves works.
- Hover markdown rendering rewritten from React (`createRoot` + mdast-to-ReactNode) to direct DOM
  construction, preserving the existing link-scheme sanitization.
- Inlay hints wired up for the first time: `LspDocument.inlayHints` exists but no consumer calls
  it today.
- Shadow-DOM styling driven by CSS custom properties with light and dark defaults, replacing the
  labs-only dark theme.
- The labs editor (`apps/docs/app/labs/editor.tsx`) becomes a thin React wrapper over the
  extracted element, proving the extraction against its existing observable behavior.
- The static documentation site (`packages/documentation-site`) emits `<silk-snippet>` for `silk`
  fences and ships the element bundle with generated pages. Fences marked `silk,ignore` emit
  highlight-only elements. The documentation JSON is unchanged.

## Capabilities

### New Capabilities

- `snippet-element`: A framework-free `<silk-snippet>` custom element that renders Silk source
  with compiler-driven highlighting and, per boolean attribute, browser-compiled diagnostics,
  hover, inlay hints, and editing — usable from any plain HTML page.
- `documentation-site-silk-snippets`: The static documentation site embeds `silk` fences as
  `<silk-snippet>` elements with feature flags derived from fence attributes, degrading
  `silk,ignore` fences to highlight-only.

### Modified Capabilities

<!-- none: the labs workbench keeps its observable behavior (docs-silk-highlighting requirements
     are unchanged); its editor implementation is re-pointed at the extracted element, which is a
     refactor below spec level. -->

## Impact

- New package (working name `@silk-effect/snippet`), depending on `compiler`, `lsp`, `language`,
  and CodeMirror. `packages/language` keeps its current dependency boundary (no `lsp` dependency).
- `apps/docs/app/labs/editor.tsx` and `Hover.tsx` shrink to wrappers or are deleted in favor of
  the element; `workbench.tsx` and the rest of labs are untouched.
- `packages/documentation-site` gains element emission and bundle shipping; its current
  `<pre><code>` fallback remains for non-Silk languages.
- No compiler, LSP-protocol, or documentation-JSON changes. `LspDocument.inlayHints` gains its
  first consumer.
- Open question for design: how the element resolves stdlib imports in the browser (labs already
  compiles in-browser; the element must bundle or fetch the same source lookup doctest uses).
