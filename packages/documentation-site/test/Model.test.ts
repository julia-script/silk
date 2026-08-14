import { assert, it } from '@effect/vitest'
import * as Model from '../src/Model.js'

it('reports a value that is not documentation instead of throwing', () => {
  assert.strictEqual(Model.decode(undefined)._tag, 'NotDocumentation')
  assert.strictEqual(Model.decode([])._tag, 'NotDocumentation')
  assert.strictEqual(Model.decode({ modules: [] })._tag, 'NotDocumentation')
  const wrongSchema = Model.decode({ schema: 'something-else', modules: [] })
  assert.strictEqual(wrongSchema._tag, 'NotDocumentation')
  if (wrongSchema._tag === 'NotDocumentation') assert.include(wrongSchema.message, Model.schemaName)
  const missingModules = Model.decode({ schema: Model.schemaName })
  assert.strictEqual(missingModules._tag, 'NotDocumentation')
})

/**
 * The schema is chartered experimental, so a field that is absent has to fall back rather than
 * lose the declaration it belongs to. Only the top level is strict.
 */
it('decodes an item that is missing fields the emitter writes today', () => {
  const decoded = Model.decode({
    schema: Model.schemaName,
    modules: [{ name: 'project/main', items: [{ name: 'add' }, 'not an item', { id: 'no name' }] }],
  })
  assert.strictEqual(decoded._tag, 'Decoded')
  if (decoded._tag !== 'Decoded') return
  const module = decoded.documentation.modules.at(0)
  assert.isDefined(module)
  assert.strictEqual(module.sourceId, 'project/main')
  assert.deepStrictEqual(
    module.items.map((item) => [item.name, item.kind, item.visibility, item.signature]),
    [
      ['add', 'Unknown', 'Inherited', ''],
      ['', 'Unknown', 'Inherited', ''],
    ],
  )
})

it('accepts a signature as an object or as a bare string', () => {
  const decoded = Model.decode({
    schema: Model.schemaName,
    modules: [
      {
        name: 'project/main',
        items: [
          { name: 'nested', signature: { text: 'pub fn nested() -> i32' } },
          { name: 'bare', signature: 'pub fn bare() -> i32' },
        ],
      },
    ],
  })
  assert.strictEqual(decoded._tag, 'Decoded')
  if (decoded._tag !== 'Decoded') return
  assert.deepStrictEqual(
    decoded.documentation.modules.at(0)?.items.map((item) => item.signature),
    ['pub fn nested() -> i32', 'pub fn bare() -> i32'],
  )
})

it('takes a summary from the first paragraph of a document', () => {
  assert.strictEqual(
    Model.summary({
      markdown: 'Adds two values.\nOn one wrapped line.\n\nA second paragraph.',
      blocks: [],
      fallback: false,
    }),
    'Adds two values. On one wrapped line.',
  )
  assert.strictEqual(Model.summary(undefined), '')
})
