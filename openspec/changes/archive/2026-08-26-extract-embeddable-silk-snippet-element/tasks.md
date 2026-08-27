# Tasks

## 1. Package scaffold

- [x] 1.1 Add the snippet element to `packages/editor-support` (`@silklang/editor-support`) with
      tsconfig/vitest/turbo wiring, with deps on `compiler`, `lsp`, `language`, CodeMirror
      packages, and `mdast-util-from-markdown`
- [x] 1.2 Add the package to workspace/turbo config and verify `node scripts/turbo.mjs run build`
      passes with an empty index (workspace glob and generic turbo tasks required no config edits)

## 2. Extract the editor core (framework-free)

- [x] 2.1 Port `SilkEditor`'s CodeMirror assembly from `apps/docs/app/labs/editor.tsx` into a
      plain mount function: highlight extension, theme via CSS custom properties, read-only facet,
      byte↔UTF-16 span translation, and an imperative handle (get/set value, format, set span
      highlight, destroy)
- [x] 2.2 Rewrite `Hover.tsx`'s mdast walk as a DOM renderer, preserving `safeLink`
      (http/https/mailto only) and highlighted nested `silk` code; port `Hover.test.tsx` cases
- [x] 2.3 Wire diagnostics and hover from `LspDocument` against a per-snippet snapshot, matching
      the labs behavior
- [x] 2.4 Implement inlay hints as CodeMirror inline widget decorations from
      `LspDocument.inlayHints` (first consumer)

## 3. The custom element

- [x] 3.1 Implement the `<silk-snippet>` custom element: `textContent` as source (first newline
      trimmed), shadow DOM, light-DOM text preserved until upgrade for the no-JS fallback
- [x] 3.2 Implement observed boolean attributes `diagnostics`, `hover`, `inlay-hints`, `editable`;
      no semantic attribute means the content is never compiled
- [x] 3.3 Lazy compilation: `Analysis.ofSourceRealized` with doctest's identity scheme and default
      target, triggered by first `IntersectionObserver` visibility; debounced recompile for
      editable snippets
- [x] 3.4 Default light and dark themes from `prefers-color-scheme`, overridable via documented
      `--silk-snippet-*` custom properties
- [x] 3.5 Ship a self-registering browser bundle build alongside the library export
- [x] 3.6 Tests covering the spec scenarios: highlight-only never compiles, per-flag gating,
      doctest-parity diagnostics, unsafe hover links, read-only default, deferred compile

## 4. Re-point labs

- [x] 4.1 Replace `apps/docs/app/labs/editor.tsx` internals with a thin wrapper over the extracted
      core, keeping the `SilkEditor` props surface and span-cursor/format/URL wiring in labs
- [x] 4.2 Delete `apps/docs/app/labs/Hover.tsx` in favor of the package renderer; migrate its test
- [x] 4.3 Verify the docs-silk-highlighting workbench scenarios still pass (edit updates panes,
      selection moves span cursor, URL round trip)

## 5. Static site emission

- [x] 5.1 In `packages/docgen` Prose.ts, parse the fence language token with the
      doctest comma convention; `silk` fences emit `<silk-snippet diagnostics hover>`,
      `silk,ignore` fences emit a bare `<silk-snippet>`, other languages keep `<pre><code>`
- [x] 5.2 Copy the element bundle into generated output and reference it with a relative module
      script from the page template
- [x] 5.3 Site tests: silk fence emits flagged element, ignore fence emits bare element, non-silk
      fence unchanged, generated output references only relative resources
- [x] 5.4 Record the measured element bundle size in the change notes; if highlight-only pages pay
      for the full compiler, note the split option as follow-up

## 6. Verification

- [x] 6.1 `node scripts/turbo.mjs run test` green across affected packages
- [x] 6.2 Manual pass: generated site served as static files shows highlight, diagnostics, hover,
      and inlay hints per flags, in light and dark, with JS disabled fallback text
