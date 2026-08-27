# Design

## Context

See proposal.md — Why. Current state that shapes the approach:

- `apps/docs/app/labs/editor.tsx` (`SilkEditor`) already does everything the element needs on the
  main thread, synchronously: CodeMirror with `SilkCodeMirror.extension()` for highlighting, and
  direct calls to `LspDocument.diagnostics`, `LspDocument.hover`, and `LspDocument.format` against
  an `Analysis.Snapshot`. No worker, no protocol.
- React's real footprint is two things: hook-based lifecycle glue in `SilkEditor`, and
  `Hover.tsx`, which renders hover CommonMark via `createRoot` into the CodeMirror tooltip.
- `LspDocument.inlayHints` exists (packages/lsp/src/Document.ts) with no consumer.
- Stdlib sources ship inside `@silklang/compiler` (`CompilerStdlib.sources`), so a browser
  bundle of the compiler resolves stdlib imports with no fetching. Labs proves in-browser
  compilation works today.
- Doctest compiles each fence as one standalone module via `Analysis.ofSourceRealized(identity,
  bytes, target)` with default target `wasm32-unknown-unknown`.
- The static site renderer (`packages/docgen`) renders fences as escaped `<pre><code>` strings
  (Prose.ts) from documentation JSON, where a fence's `language` field carries the full comma-form
  token (`silk,ignore`).
- `packages/editor-support` owns compiler-aware CodeMirror and snippet integration; CodeMirror is a
  dependency there.

## Goals / Non-Goals

**Goals:**

- One framework-free package owning the element; labs and the static site are both consumers.
- Behavior parity with doctest: what the element compiles is exactly what doctest verified.
- Keep the extraction mechanical — no new semantic engine, no worker, no protocol.

**Non-Goals:**

- Running snippets (evaluation/output) — display and editing semantics only.
- Multi-file or cross-snippet projects; one element is one standalone module.
- A worker architecture; revisit only if editable snippets measurably jank.
- Replacing the docs app's Shiki path for non-Silk fences.
- Publishing/versioning strategy for the element bundle beyond the site's own output.

## Decisions

**The element belongs in `@silklang/editor-support`.** The element needs
`@silklang/lsp`; the portable editor-support package owns that editor-facing dependency and other consumers
(TextMate, vscode) should not inherit one. Dependencies: `compiler`, `lsp`, `language`,
CodeMirror packages, `mdast-util-from-markdown`. Two deliverables: an ESM library export (custom
element class + registration function) and a self-registering IIFE/ESM bundle for script-tag use
in generated sites.

**Keep CodeMirror for read-only snippets.** Alternative — a hand-rolled span renderer for
read-only mode — was rejected: tooltips, squiggle decorations, byte↔UTF-16 translation, and the
editable upgrade path all already work in CodeMirror, and read-only is one
`EditorState.readOnly.of(true)` facet. Bundle cost is accepted until measured to matter.

**Custom element with shadow DOM.** `connectedCallback` reads `textContent` as the source (HTML
entity decoding applies; generators escape normally), replaces it with the shadow-rendered editor,
and keeps the light-DOM text as the no-JS fallback until upgrade. Observed boolean attributes:
`diagnostics`, `hover`, `inlay-hints`, `editable`. Theming via `--silk-snippet-*` custom
properties with light/dark defaults keyed off `prefers-color-scheme`; the labs workbench keeps its
own dark values by setting the properties.

**Compilation is per-element, lazy, main-thread.** Each element with at least one semantic
attribute compiles its own content with `Analysis.ofSourceRealized` (doctest's identity scheme and
default target) when an `IntersectionObserver` first reports it visible. Highlight-only elements
never compile. Editable elements recompile on a debounced document change, replacing the snapshot
the semantic providers read — the same shape as labs' snapshot-per-edit flow. No snapshot sharing
across elements: snippets are independent modules and correctness beats a cache.

**Hover rendering moves from React to DOM construction.** `Hover.tsx`'s mdast walk is rewritten
node-for-node using `document.createElement`, preserving `safeLink` (http/https/mailto only) and
the highlighted rendering of nested `silk` code via `SilkCodeMirror.highlightRanges`. This DOM
renderer lives in the snippet package; labs deletes `Hover.tsx` and uses the element.

**Inlay hints via CodeMirror widget decorations.** `LspDocument.inlayHints` results become inline
widget decorations (not document text), recomputed with the snapshot. This is the only genuinely
new feature code in the change.

**Labs re-points, workbench API preserved.** `SilkEditor` keeps its current props signature but
delegates to the element (or directly to the extracted mounting API), keeping `workbench.tsx`
untouched. The span-cursor field and URL/format wiring stay in labs — they are workbench concerns,
exposed by the element as a small imperative surface (set span highlight, format, get/set value)
on the element instance.

**Site emission via fence attributes.** Prose.ts branches on the parsed fence language token using
doctest's `Example.parseLanguage` convention: `silk` → element with `diagnostics hover`;
`silk,ignore` → element with no semantic attributes; anything else → existing `<pre><code>`. The
site build copies the element bundle into the output and references it with a relative `<script
type="module">` in the page template.

**The renderer's dependency boundary survives the bundle** *(decided during implementation)*. The
site package charters itself — via two guard tests — as reading the documentation JSON and nothing
else: no `@silklang/*` runtime dependency, no workspace import in `src/`. The bundle crosses
that boundary as data, not types: `Site.render` stays pure and takes the bundle *contents* as an
option, and only the command shell resolves `@silklang/editor-support/bundle` to a file
path and reads it. The guard tests were deliberately and visibly amended — exactly the edit their
own comments anticipate — to allowlist `@silklang/editor-support` as an opaque asset supplier and to
exempt only `Cli.ts` from the source scan; every renderer module remains import-free. Generated
pages also carry a `silk-snippet:not(:defined)` style so an element that never upgrades (no
JavaScript, or a library caller who shipped no bundle) still reads as a code block.

## Risks / Trade-offs

- [Compiler bundle rides on every generated docs page] → Lazy compile keeps load cheap; bundle is
  loaded once per site, cached; measure bundle size in the site build and record it. If it grows
  unacceptable, code-split semantics from the highlight-only path (highlighting needs only the
  lexer).
- [Main-thread compile of a large editable snippet may jank] → Snippets are doc-sized by
  convention; debounce recompiles. Worker extraction is the known upgrade path and the element API
  does not preclude it.
- [`textContent` as source is whitespace-sensitive inside HTML] → Generators emit the source
  exactly, first newline trimmed by the element; document the contract for hand-authored pages.
- [Labs parity regressions during extraction] → Labs' existing tests (`Hover.test.tsx`, workbench
  behavior) run against the wrapper; the docs-silk-highlighting spec scenarios are the acceptance
  bar.
- [Two markdown renderers for hover-like content (site Prose.ts strings vs element DOM)] →
  Accepted duplication; they serve different trust and output models.

## Open Questions

- ~~Exact bundle-size number~~ *measured*: `silk-snippet.bundle.js` is 2.98 MB raw, ~714 KB
  gzipped (compiler + stdlib sources + CodeMirror). Loaded once per site and cached; lazy compile
  keeps pages responsive. The highlight-only code split remains a follow-up if this grows.
- Whether the docs app's Markdown `silk` fences (Shiki path) should also adopt the element — a
  follow-up change if desired; out of scope here.
