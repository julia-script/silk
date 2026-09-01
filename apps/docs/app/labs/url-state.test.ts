import { assert, describe, it } from '@effect/vitest'
import { Orientation, type SerializedDockview } from 'dockview'
import { decodeLayout, decodeSource, encodeLayout, encodeSource } from './url-state'

// A shared link is only worth anything if what comes back out is what went in, so the round trip
// is the property worth pinning: these encodings are what let a refresh land on the same view.

const defined = <A>(value: A | undefined): A => {
  assert.isDefined(value)
  return value
}

describe('source encoding', () => {
  it('round-trips a program, including newlines and non-ASCII', async () => {
    const state = {
      root: 'main',
      modules: { main: 'pub fn main() -> i32 { return 42 }\n-- naïve ⇒ ok\n' },
    }
    assert.deepEqual(await decodeSource(defined(await encodeSource(state))), state)
  })

  it('round-trips a multi-module request', async () => {
    const state = {
      root: 'root',
      modules: {
        root: 'import lib\npub fn main() -> i32 { return 42 }',
        lib: 'pub fn answer() -> i32 { return 1 }',
      },
    }
    assert.deepEqual(await decodeSource(defined(await encodeSource(state))), state)
  })

  it('produces no characters that a query string would have to escape', async () => {
    const encoded = await encodeSource({
      root: 'main',
      modules: { main: 'pub fn main() -> i32 { return i32.add(1, 41) }' },
    })
    assert.match(defined(encoded), /^[A-Za-z0-9\-_]*$/)
  })

  it('carries the target, since the same program lowers differently for each', async () => {
    const state = {
      root: 'main',
      modules: { main: 'pub fn main() -> i32 { return 42 }' },
      target: 'wasm32-unknown-unknown',
    }
    assert.deepEqual(await decodeSource(defined(await encodeSource(state))), state)
  })

  it('keeps an unknown target rather than silently compiling for the default', async () => {
    // `Target.select` turns this into an unavailable selection the panes already render, which is
    // a better answer than quietly emitting for some other target.
    const encoded = defined(
      await encodeSource({
        root: 'main',
        modules: { main: 'pub fn main() -> i32 { return 42 }' },
        target: 'sparc-unknown-none',
      }),
    )
    assert.strictEqual((await decodeSource(encoded))?.target, 'sparc-unknown-none')
  })

  it('rejects a root that names no module, which would not load', async () => {
    const encoded = defined(
      await encodeSource({
        root: 'absent',
        modules: { main: 'pub fn main() -> i32 { return 42 }' },
      }),
    )
    assert.isUndefined(await decodeSource(encoded))
  })

  it('reports damaged input rather than throwing', async () => {
    assert.isUndefined(await decodeSource('!!!not base64!!!'))
  })
})

describe('layout encoding', () => {
  const layout: SerializedDockview = {
    grid: {
      root: { type: 'branch', data: [] },
      width: 1600,
      height: 900,
      orientation: Orientation.HORIZONTAL,
    },
    panels: {
      'pane-mir-1': {
        id: 'pane-mir-1',
        contentComponent: 'view',
        params: { view: 'mir' },
        title: 'MIR control flow',
      },
    },
  }

  it('round-trips a layout', async () => {
    const encoded = await encodeLayout(layout)
    assert.isDefined(encoded)
    assert.deepEqual(await decodeLayout(defined(encoded)), layout)
  })

  it('stays URL-safe and compresses rather than inflating the link', async () => {
    const encoded = defined(await encodeLayout(layout))
    assert.match(encoded, /^[A-Za-z0-9\-_]*$/)
    assert.isBelow(encoded.length, JSON.stringify(layout).length)
  })

  it('rejects a payload that decodes but is not a layout', async () => {
    const notALayout: SerializedDockview = {
      ...layout,
      panels: { invalid: { id: 'invalid' } },
    }
    assert.isUndefined(await decodeLayout(defined(await encodeLayout(notALayout))))
  })

  it('rejects retired view ids and pre-view source panels', async () => {
    const retiredView: SerializedDockview = {
      ...layout,
      panels: {
        'pane-llvm': {
          id: 'pane-llvm',
          contentComponent: 'view',
          params: { view: 'llvm' },
          title: 'LLVM',
        },
      },
    }
    const sourceComponent: SerializedDockview = {
      ...layout,
      panels: {
        source: { id: 'source', contentComponent: 'source', params: {}, title: 'Source' },
      },
    }

    assert.isUndefined(await decodeLayout(defined(await encodeLayout(retiredView))))
    assert.isUndefined(await decodeLayout(defined(await encodeLayout(sourceComponent))))
  })

  it('reports damaged input rather than throwing', async () => {
    assert.isUndefined(await decodeLayout('!!!not deflate!!!'))
  })
})
