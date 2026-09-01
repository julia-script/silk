import { assert, it } from '@effect/vitest'
import config from '../next.config.mjs'

it('configures cross-origin isolation for the editor route and child paths', async () => {
  const entries = await config.headers()
  const editorEntries = entries.filter(
    ({ source }) => source === '/editor' || source === '/editor/:path*',
  )

  assert.deepStrictEqual(
    editorEntries.map(({ source }) => source),
    ['/editor', '/editor/:path*'],
  )
  for (const entry of editorEntries) {
    assert.deepInclude(entry.headers, {
      key: 'Cross-Origin-Embedder-Policy',
      value: 'require-corp',
    })
    assert.deepInclude(entry.headers, {
      key: 'Cross-Origin-Opener-Policy',
      value: 'same-origin',
    })
  }
})
