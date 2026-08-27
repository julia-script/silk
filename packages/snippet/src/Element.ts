/**
 * The `<silk-snippet>` custom element.
 *
 * The element's text content is its Silk source. Before JavaScript runs (or without it) that text
 * is what the page shows; upgrading replaces it with the shadow-rendered editor. Highlighting is
 * always on; each semantic feature — `diagnostics`, `hover`, `inlay-hints` — is enabled by its
 * boolean attribute, and `editable` lifts the read-only default.
 *
 * A snippet with no semantic attribute never compiles. One with any compiles lazily — on first
 * visibility, not page load — as one complete standalone module with doctest's conventions, so
 * what the reader sees is exactly what doctest verified.
 */

import * as Analysis from '@silk-lang/compiler/Analysis'
import * as Effect from 'effect/Effect'
import * as Editor from './Editor.js'

/** The target snippets compile against — doctest's default, for verification parity. */
const defaultTarget = 'wasm32-unknown-unknown'

const encoder = new TextEncoder()

let ordinal = 0

const nextModule = (): string => {
  ordinal += 1
  return `snippet/${ordinal}`
}

/**
 * Shadow-scoped chrome and the default palette for both color schemes. Every value is a
 * `--silk-snippet-*` custom property, so a host page can retheme snippets with one rule on the
 * element; the dark block only supplies scheme defaults and loses to any host-set property.
 */
const styles = `
:host {
  display: block;
  border: 1px solid var(--silk-snippet-border, #d0d7de);
  border-radius: 6px;
  background-color: var(--silk-snippet-bg, #f6f8fa);
  /* Ligature fonts render operators like \`|>\` and \`->\` as single glyphs, hiding what the
     reader would actually type. Source is shown character for character. */
  font-variant-ligatures: none;
  --silk-snippet-token-keyword: #a626a4;
  --silk-snippet-token-type: #c18401;
  --silk-snippet-token-identifier: #383a42;
  --silk-snippet-token-number: #986801;
  --silk-snippet-token-string: #50a14f;
  --silk-snippet-token-comment: #a0a1a7;
  --silk-snippet-token-operator: #0184bc;
  --silk-snippet-token-punctuation: #383a42;
}
@media (prefers-color-scheme: dark) {
  :host {
    border-color: var(--silk-snippet-border, #3d444d);
    background-color: var(--silk-snippet-bg, #151b23);
    --silk-snippet-ink: #e6edf3;
    --silk-snippet-ink-muted: #9198a1;
    --silk-snippet-border: #3d444d;
    --silk-snippet-tooltip-bg: #151b23;
    --silk-snippet-selection: rgba(84, 120, 198, 0.35);
    --silk-snippet-code-bg: rgba(84, 120, 198, 0.25);
    --silk-snippet-error: #f47067;
    --silk-snippet-hint: #767e87;
    --silk-snippet-accent: #82aaff;
    --silk-snippet-token-keyword: #c678dd;
    --silk-snippet-token-type: #e5c07b;
    --silk-snippet-token-identifier: #e6edf3;
    --silk-snippet-token-number: #d19a66;
    --silk-snippet-token-string: #98c379;
    --silk-snippet-token-comment: #7f848e;
    --silk-snippet-token-operator: #56b6c2;
    --silk-snippet-token-punctuation: #abb2bf;
  }
}
.cm-silk-keyword { color: var(--silk-snippet-token-keyword); }
.cm-silk-boolean { color: var(--silk-snippet-token-number); }
.cm-silk-type { color: var(--silk-snippet-token-type); }
.cm-silk-identifier { color: var(--silk-snippet-token-identifier); }
.cm-silk-number { color: var(--silk-snippet-token-number); }
.cm-silk-string { color: var(--silk-snippet-token-string); }
.cm-silk-character { color: var(--silk-snippet-token-string); }
.cm-silk-line-comment { color: var(--silk-snippet-token-comment); }
.cm-silk-doc-comment,
.cm-silk-doc-comment-marker,
.cm-silk-module-doc-comment,
.cm-silk-module-doc-comment-marker { color: var(--silk-snippet-token-comment); }
.cm-silk-operator { color: var(--silk-snippet-token-operator); }
.cm-silk-punctuation,
.cm-silk-type-punctuation { color: var(--silk-snippet-token-punctuation); }
.cm-silk-doc-heading-marker,
.cm-silk-doc-strong-marker,
.cm-silk-doc-emphasis-marker,
.cm-silk-doc-code-marker,
.cm-silk-doc-code-fence,
.cm-silk-doc-link-punctuation { color: var(--silk-snippet-token-operator); }
.cm-silk-doc-link-target { color: var(--silk-snippet-token-type); }
.cm-silk-doc-code,
.cm-silk-doc-code-language { color: var(--silk-snippet-token-string); }
.cm-silk-doc-code-keyword { color: var(--silk-snippet-token-keyword); }
.cm-silk-doc-code-type { color: var(--silk-snippet-token-type); }
.cm-silk-doc-code-identifier { color: var(--silk-snippet-token-identifier); }
.cm-silk-doc-code-number { color: var(--silk-snippet-token-number); }
.cm-silk-doc-code-comment { color: var(--silk-snippet-token-comment); }
.cm-silk-doc-code-operator { color: var(--silk-snippet-token-operator); }
.cm-silk-doc-code-punctuation { color: var(--silk-snippet-token-punctuation); }
.cm-silk-doc-code-invalid { text-decoration: underline wavy var(--silk-snippet-error, #d1383d); }
`

export class SilkSnippetElement extends HTMLElement {
  static observedAttributes = ['diagnostics', 'hover', 'inlay-hints', 'editable']

  #source: string | undefined
  #handle: Editor.Handle | undefined
  #observer: IntersectionObserver | undefined
  #module = nextModule()
  #compiled = false
  #recompileTimer: ReturnType<typeof setTimeout> | undefined

  /** The snippet's current Silk source. */
  get source(): string {
    return this.#handle?.value() ?? this.#source ?? ''
  }

  #features(): Editor.Features {
    return {
      diagnostics: this.hasAttribute('diagnostics'),
      hover: this.hasAttribute('hover'),
      inlayHints: this.hasAttribute('inlay-hints'),
    }
  }

  #semantic(): boolean {
    const features = this.#features()
    return features.diagnostics === true || features.hover === true || features.inlayHints === true
  }

  /** Compiles the current source as one standalone module and hands the session to the editor. */
  #compile(): void {
    const handle = this.#handle
    if (handle === undefined) return
    this.#compiled = true
    const bytes = encoder.encode(handle.value())
    const snapshot = Effect.runSync(Analysis.ofSourceRealized(this.#module, bytes, defaultTarget))
    handle.setSession(Editor.session(this.#module, bytes, snapshot))
  }

  #scheduleRecompile(): void {
    if (!this.#compiled) return
    if (this.#recompileTimer !== undefined) clearTimeout(this.#recompileTimer)
    this.#recompileTimer = setTimeout(() => {
      this.#recompileTimer = undefined
      this.#compile()
    }, 300)
  }

  /** Compiles when the snippet first becomes visible, so page load never pays for analysis. */
  #observe(): void {
    if (!this.#semantic()) return
    if (typeof IntersectionObserver === 'undefined') {
      // No visibility signal on this platform: defer past load, then compile.
      setTimeout(() => this.#compile(), 0)
      return
    }
    this.#observer = new IntersectionObserver((entries) => {
      if (!entries.some((entry) => entry.isIntersecting)) return
      this.#observer?.disconnect()
      this.#observer = undefined
      this.#compile()
    })
    this.#observer.observe(this)
  }

  #mount(): void {
    // First connect owns the source; the leading newline is authoring convenience inside HTML.
    if (this.#source === undefined) this.#source = (this.textContent ?? '').replace(/^\r?\n/, '')
    const root = this.shadowRoot ?? this.attachShadow({ mode: 'open' })
    root.replaceChildren()
    const sheet = document.createElement('style')
    sheet.textContent = styles
    root.append(sheet)
    this.#handle = Editor.mount({
      parent: root,
      root,
      doc: this.#source,
      editable: this.hasAttribute('editable'),
      features: this.#features(),
      onChange: (doc) => {
        this.#source = doc
        this.#scheduleRecompile()
      },
    })
    this.#compiled = false
    this.#observe()
  }

  #unmount(): void {
    if (this.#recompileTimer !== undefined) clearTimeout(this.#recompileTimer)
    this.#recompileTimer = undefined
    this.#observer?.disconnect()
    this.#observer = undefined
    this.#handle?.destroy()
    this.#handle = undefined
  }

  connectedCallback(): void {
    if (this.#handle !== undefined) return
    this.#mount()
  }

  disconnectedCallback(): void {
    this.#unmount()
  }

  attributeChangedCallback(): void {
    // Feature flags select which extensions exist, so a change rebuilds the editor.
    if (this.#handle === undefined) return
    this.#unmount()
    this.#mount()
  }
}

/** Registers the element under `silk-snippet`; safe to call more than once. */
export const define = (): void => {
  if (customElements.get('silk-snippet') === undefined)
    customElements.define('silk-snippet', SilkSnippetElement)
}
