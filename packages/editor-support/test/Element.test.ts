/**
 * Element behavior against the snippet-element spec scenarios, in jsdom.
 *
 * jsdom has no IntersectionObserver, so a recording stub stands in: compilation is observed to be
 * deferred until the stub reports visibility, which is exactly the contract the element has with
 * the real observer.
 */

import { afterEach, assert, beforeEach, describe, it, vi } from 'vitest'
import * as Effect from 'effect/Effect'
import { define, type SilkSnippetElement } from '../src/Element.js'

const passing = 'pub fn main() -> i32 {\n  return 0\n}'
const failing = 'pub fn main() -> i32 {\n  return missing()\n}'
const hinted =
  'import silk.allocator { Allocator, SystemAllocator }\npub fn main() -> i32 {\n  let mut allocator = Allocator.systemAllocatorProvider()\n  return 0\n}'

/** Records observers so a test chooses when a snippet "becomes visible". */
class ObserverStub {
  static instances: Array<ObserverStub> = []
  targets: Array<globalThis.Element> = []
  constructor(readonly callback: IntersectionObserverCallback) {
    ObserverStub.instances.push(this)
  }
  observe(target: globalThis.Element): void {
    this.targets.push(target)
  }
  disconnect(): void {}
  unobserve(): void {}
  takeRecords(): Array<IntersectionObserverEntry> {
    return []
  }
  reveal(): void {
    this.callback(
      [{ isIntersecting: true } as IntersectionObserverEntry],
      this as unknown as IntersectionObserver,
    )
  }
}

const flush = (): Promise<void> => Effect.runPromise(Effect.sleep(0))

const snippet = (code: string, attributes: ReadonlyArray<string> = []): SilkSnippetElement => {
  const element = document.createElement('silk-snippet') as SilkSnippetElement
  for (const attribute of attributes) element.setAttribute(attribute, '')
  element.textContent = `\n${code}`
  document.body.append(element)
  return element
}

/** CodeMirror registers observers of its own; only ones watching a snippet element count. */
const snippetObservers = (): ReadonlyArray<ObserverStub> =>
  ObserverStub.instances.filter((observer) =>
    observer.targets.some((target) => target.tagName === 'SILK-SNIPPET'),
  )

const revealAll = (): void => {
  for (const observer of snippetObservers()) observer.reveal()
}

const lintMarks = (element: SilkSnippetElement): number =>
  element.shadowRoot?.querySelectorAll('.cm-lintRange-error').length ?? 0

beforeEach(() => {
  ObserverStub.instances = []
  vi.stubGlobal('IntersectionObserver', ObserverStub)
  define()
})

afterEach(() => {
  document.body.replaceChildren()
  vi.unstubAllGlobals()
})

describe('silk-snippet', () => {
  it('renders highlighted source from its text content without compiling', () => {
    const element = snippet(passing)
    const content = element.shadowRoot?.querySelector('.cm-content')
    assert.isNotNull(content)
    assert.include(element.shadowRoot?.innerHTML ?? '', 'cm-silk-keyword')
    assert.strictEqual(element.source, passing)
    // No semantic attribute: nothing watches visibility because nothing will ever compile.
    assert.strictEqual(snippetObservers().length, 0)
  })

  it('defers compilation until the snippet becomes visible', async () => {
    const element = snippet(failing, ['diagnostics'])
    assert.strictEqual(snippetObservers().length, 1)
    await flush()
    assert.strictEqual(lintMarks(element), 0)
    revealAll()
    await flush()
    assert.isAbove(lintMarks(element), 0)
  })

  it('shows no diagnostics for an example doctest accepts', async () => {
    const element = snippet(passing, ['diagnostics'])
    revealAll()
    await flush()
    assert.strictEqual(lintMarks(element), 0)
  })

  it('keeps diagnostics off when only hover is enabled', async () => {
    const element = snippet(failing, ['hover'])
    revealAll()
    await flush()
    assert.strictEqual(lintMarks(element), 0)
  })

  it('never compiles a highlight-only snippet even with failing content', async () => {
    const element = snippet(failing)
    revealAll()
    await flush()
    assert.strictEqual(lintMarks(element), 0)
    assert.strictEqual(snippetObservers().length, 0)
  })

  it('renders inlay hints as widgets outside the source text', async () => {
    const element = snippet(hinted, ['inlay-hints'])
    revealAll()
    await flush()
    const hints = element.shadowRoot?.querySelectorAll('.cm-silk-inlay-hint') ?? []
    assert.isAbove(hints.length, 0)
    assert.include(hints[0]?.textContent ?? '', 'SystemAllocator')
    assert.strictEqual(element.source, hinted)
  })

  it('is read-only by default and editable by attribute', () => {
    const readOnly = snippet(passing)
    const editable = snippet(passing, ['editable'])
    assert.strictEqual(
      readOnly.shadowRoot?.querySelector('.cm-content')?.getAttribute('contenteditable'),
      'false',
    )
    assert.strictEqual(
      editable.shadowRoot?.querySelector('.cm-content')?.getAttribute('contenteditable'),
      'true',
    )
  })
})
