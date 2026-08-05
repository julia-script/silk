import type { SerializedDockview } from 'dockview'
import { describe, expect, it } from 'vitest'
import { decodeLayout, decodeSource, encodeLayout, encodeSource } from './url-state'

// A shared link is only worth anything if what comes back out is what went in, so the round trip
// is the property worth pinning: these encodings are what let a refresh land on the same view.

describe('source encoding', () => {
  it('round-trips a program, including newlines and non-ASCII', () => {
    const source = 'pub fn main() -> I32 { return 42 }\n-- naïve ⇒ ok\n'
    expect(decodeSource(encodeSource(source))).toBe(source)
  })

  it('produces no characters that a query string would have to escape', () => {
    const encoded = encodeSource('pub fn main() -> I32 { return I32.add(1, 41) }')
    expect(encoded).toMatch(/^[A-Za-z0-9\-_]*$/)
  })

  it('reports damaged input rather than throwing', () => {
    expect(decodeSource('!!!not base64!!!')).toBeUndefined()
  })
})

describe('layout encoding', () => {
  const layout = {
    grid: {
      root: { type: 'branch', data: [] },
      width: 1600,
      height: 900,
      orientation: 'HORIZONTAL',
    },
    panels: {
      'pane-mir-1': {
        id: 'pane-mir-1',
        contentComponent: 'view',
        params: { view: 'mir' },
        title: 'MIR control flow',
      },
    },
  } as unknown as SerializedDockview

  it('round-trips a layout', async () => {
    const encoded = await encodeLayout(layout)
    expect(encoded).toBeDefined()
    expect(await decodeLayout(encoded as string)).toEqual(layout)
  })

  it('stays URL-safe and compresses rather than inflating the link', async () => {
    const encoded = (await encodeLayout(layout)) as string
    expect(encoded).toMatch(/^[A-Za-z0-9\-_]*$/)
    expect(encoded.length).toBeLessThan(JSON.stringify(layout).length)
  })

  it('rejects a payload that decodes but is not a layout', async () => {
    // A stale link from an older build must fall back to the default layout, not throw
    // during render.
    const notALayout = (await encodeLayout({ nope: true } as unknown as SerializedDockview)) as string
    expect(await decodeLayout(notALayout)).toBeUndefined()
  })

  it('reports damaged input rather than throwing', async () => {
    expect(await decodeLayout('!!!not deflate!!!')).toBeUndefined()
  })
})
