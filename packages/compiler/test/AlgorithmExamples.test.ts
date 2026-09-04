import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Schema from 'effect/Schema'

const Blocker = Schema.Struct({
  phase: Schema.String,
  code: Schema.String,
  message: Schema.String,
})

const Manifest = Schema.Struct({
  schemaVersion: Schema.Literal(1),
  id: Schema.String,
  title: Schema.String,
  status: Schema.Literals(['executable', 'frontier']),
  source: Schema.String,
  input: Schema.String,
  expected: Schema.Struct({
    entryResult: Schema.Finite,
    algorithmResult: Schema.optionalKey(Schema.Finite),
    summary: Schema.String,
  }),
  capabilities: Schema.Array(Schema.String),
  targets: Schema.Array(Schema.Literal('native')),
  blockers: Schema.Array(Blocker),
})

const examplesRoot = fileURLToPath(new URL('../../../examples/algorithms/', import.meta.url))
const exampleIds = readdirSync(examplesRoot)
  .filter((name) => statSync(join(examplesRoot, name)).isDirectory())
  .sort()

const examples = exampleIds.map((id) => {
  const root = join(examplesRoot, id)
  const manifest = Schema.decodeUnknownSync(Manifest)(
    JSON.parse(readFileSync(join(root, 'example.json'), 'utf8')),
  )
  const source = readFileSync(join(root, manifest.source), 'utf8')
  return Object.freeze({
    root,
    manifest,
    source,
    bytes: new Uint8Array(readFileSync(join(root, manifest.source))),
    readme: readFileSync(join(root, 'README.md'), 'utf8'),
  })
})

it('keeps seven complete, readable programs and explicit status contracts', () => {
  assert.deepEqual(exampleIds, [
    'breadth-first-search',
    'crc-32',
    'fft',
    'game-of-life',
    'matrix-multiplication',
    'quicksort',
    'sieve',
  ])
  assert.deepEqual(
    examples
      .filter(({ manifest }) => manifest.status === 'executable')
      .map(({ manifest }) => manifest.id),
    [
      'breadth-first-search',
      'crc-32',
      'fft',
      'game-of-life',
      'matrix-multiplication',
      'quicksort',
      'sieve',
    ],
  )
  for (const { root, manifest, source, bytes, readme } of examples) {
    assert.strictEqual(manifest.id, root.slice(root.lastIndexOf('/') + 1))
    assert.strictEqual(existsSync(join(root, manifest.source)), true)
    assert.isAbove(bytes.length, 100)
    assert.isAbove(readme.length, 100)
    assert.isAbove(manifest.input.length, 10)
    assert.isAbove(manifest.expected.summary.length, 20)
    assert.isAbove(manifest.capabilities.length, 2)
    assert.deepEqual(manifest.targets, ['native'])
    assert.strictEqual(Number.isInteger(manifest.expected.entryResult), true)
    assert.strictEqual(manifest.blockers.length === 0, manifest.status === 'executable')
    if (manifest.id === 'breadth-first-search') {
      assert.include(source, 'import silk.vector')
      assert.notInclude(source, 'Report')
      assert.include(source, 'import silk.allocator { OutOfMemoryError }')
      assert.include(source, 'pub effect fn main() -> () ! OutOfMemoryError')
      assert.notInclude(source, 'Effect.catch')
      assert.notInclude(source, 'RawBuffer')
      assert.notInclude(source, 'Slot.')
      assert.strictEqual(manifest.expected.entryResult, 0)
      assert.strictEqual(manifest.expected.algorithmResult, 8)
    }
  }
})
