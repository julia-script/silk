import { assert, it } from '@effect/vitest'
import * as Model from '../src/Model.js'

const source = Object.freeze({ sourceId: 'project/main', start: 0, end: 10 })

const item = (overrides: Record<string, unknown> = {}): Record<string, unknown> => ({
  id: 'project/main::add',
  kind: 'Function',
  name: 'add',
  visibility: 'Public',
  signature: { text: 'pub fn add(left: i32, right: i32) -> i32' },
  source,
  children: [],
  ...overrides,
})

const project = (itemValue: unknown = item()): Record<string, unknown> => ({
  schema: Model.schemaName,
  experimental: true,
  modules: [{ name: 'project/main', sourceId: 'project/main', items: [itemValue] }],
})

it('reports a value that is not current documentation instead of throwing', () => {
  assert.strictEqual(Model.decode(undefined)._tag, 'NotDocumentation')
  assert.strictEqual(Model.decode([])._tag, 'NotDocumentation')
  assert.strictEqual(Model.decode({ modules: [] })._tag, 'NotDocumentation')
  const wrongSchema = Model.decode({ schema: 'something-else', modules: [] })
  assert.strictEqual(wrongSchema._tag, 'NotDocumentation')
  if (wrongSchema._tag === 'NotDocumentation') assert.include(wrongSchema.message, Model.schemaName)
  assert.strictEqual(
    Model.decode({ schema: Model.schemaName, experimental: true })._tag,
    'NotDocumentation',
  )
  assert.strictEqual(
    Model.decode({ schema: Model.schemaName, experimental: false, modules: [] })._tag,
    'NotDocumentation',
  )
})

it('decodes the canonical signature object and required declaration fields', () => {
  const decoded = Model.decode(project())
  assert.strictEqual(decoded._tag, 'Decoded')
  if (decoded._tag !== 'Decoded') return
  assert.strictEqual(
    decoded.documentation.modules.at(0)?.items.at(0)?.signature,
    'pub fn add(left: i32, right: i32) -> i32',
  )
})

it('decodes enum, enum-member, and role declaration kinds', () => {
  for (const kind of ['Enum', 'EnumMember', 'Role']) {
    assert.strictEqual(Model.decode(project(item({ kind })))._tag, 'Decoded', kind)
  }
})

it('rejects historical signatures and missing declaration fields atomically', () => {
  assert.strictEqual(
    Model.decode(project(item({ signature: 'pub fn add() -> i32' })))._tag,
    'NotDocumentation',
  )
  const missingChildren = item()
  delete missingChildren.children
  assert.strictEqual(Model.decode(project(missingChildren))._tag, 'NotDocumentation')
  assert.strictEqual(
    Model.decode(project(item({ children: [item(), { name: 'incomplete child' }] })))._tag,
    'NotDocumentation',
  )
})

it('rejects unknown and malformed prose nodes atomically', () => {
  const document = {
    _tag: 'Document',
    source,
    markdown: 'Current prose.',
    blocks: [
      {
        _tag: 'Paragraph',
        children: [{ _tag: 'Text', value: 'Current prose.', source }],
        source,
      },
    ],
    examples: [],
    fallback: false,
  }
  assert.strictEqual(Model.decode(project(item({ documentation: document })))._tag, 'Decoded')
  assert.strictEqual(
    Model.decode(
      project(
        item({
          documentation: {
            ...document,
            blocks: [{ _tag: 'Table', children: [], source }],
          },
        }),
      ),
    )._tag,
    'NotDocumentation',
  )
  assert.strictEqual(
    Model.decode(
      project(
        item({
          documentation: {
            ...document,
            blocks: [
              {
                _tag: 'Paragraph',
                children: [{ _tag: 'Text', value: 1, source }],
                source,
              },
            ],
          },
        }),
      ),
    )._tag,
    'NotDocumentation',
  )
})

it('takes a summary from the first paragraph of a document', () => {
  assert.strictEqual(
    Model.summary({
      _tag: 'Document',
      source,
      markdown: 'Adds two values.\nOn one wrapped line.\n\nA second paragraph.',
      blocks: [],
      examples: [],
      fallback: false,
    }),
    'Adds two values. On one wrapped line.',
  )
  assert.strictEqual(Model.summary(undefined), '')
})
