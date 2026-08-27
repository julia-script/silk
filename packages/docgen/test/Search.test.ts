import { createContext, runInContext } from 'node:vm'
import { assert, it } from '@effect/vitest'
import * as Model from '../src/Model.js'
import * as Search from '../src/Search.js'

const source = Object.freeze({ sourceId: 'project/main', start: 0, end: 10 })

const entry = (name: string, module: string, summary = ''): Search.Entry => ({
  name,
  module,
  kind: 'Function',
  signature: `pub fn ${name}()`,
  href: `${module.replace('/', '-')}.html#${name.toLowerCase()}`,
  summary,
})

const entries = [
  entry('unwrapOr', 'silk/option'),
  entry('unwrap', 'silk/result'),
  entry('map', 'silk/option', 'Applies a transform to a present value.'),
  entry('flatMap', 'silk/option'),
]

it('ranks an exact name above a prefix, a substring, and a summary match', () => {
  assert.deepStrictEqual(
    Search.query(entries, 'unwrap').map((found) => found.name),
    ['unwrap', 'unwrapOr'],
  )
  assert.deepStrictEqual(
    Search.query(entries, 'map').map((found) => found.name),
    ['map', 'flatMap'],
  )
  assert.deepStrictEqual(
    Search.query(entries, 'transform').map((found) => found.name),
    ['map'],
  )
  assert.deepStrictEqual(
    Search.query(entries, 'silk/result').map((found) => found.name),
    ['unwrap'],
  )
  assert.deepStrictEqual(Search.query(entries, '   '), [])
  assert.deepStrictEqual(Search.query(entries, 'nothing here'), [])
  assert.strictEqual(Search.query(entries, 'a', 1).length, 1)
})

it('builds entries from declaration names and documentation text', () => {
  const decoded = Model.decode({
    schema: Model.schemaName,
    experimental: true,
    modules: [
      {
        name: 'project/main',
        sourceId: 'project/main',
        items: [
          {
            id: 'project/main::add',
            kind: 'Function',
            name: 'add',
            visibility: 'Public',
            signature: { text: 'pub fn add(left: i32, right: i32) -> i32' },
            source,
            documentation: {
              _tag: 'Document',
              source,
              markdown: 'Adds two values.\n\nMore prose.',
              blocks: [],
              examples: [],
              fallback: false,
            },
            children: [
              {
                id: 'project/main::add::parameter:0',
                kind: 'Parameter',
                name: 'left',
                visibility: 'Inherited',
                signature: { text: 'left: i32' },
                source,
                children: [],
              },
            ],
          },
          {
            id: 'project/main::Problem',
            kind: 'Struct',
            name: 'Problem',
            visibility: 'Public',
            signature: { text: 'pub struct Problem' },
            source,
            children: [
              {
                id: 'project/main::Problem::field:0',
                kind: 'Field',
                name: 'code',
                visibility: 'Public',
                signature: { text: 'pub code: i32' },
                source,
                children: [],
              },
            ],
          },
        ],
      },
    ],
  })
  assert.strictEqual(decoded._tag, 'Decoded')
  if (decoded._tag !== 'Decoded') return
  const built = Search.build(decoded.documentation, (module, item) => `${module.name}#${item.name}`)
  assert.deepStrictEqual(
    built.map((found) => [found.name, found.kind, found.summary]),
    [
      ['add', 'Function', 'Adds two values.'],
      ['Problem', 'Struct', ''],
      ['code', 'Field', ''],
    ],
  )
  // A parameter is read on the declaration that owns it; indexing one would double the download
  // for worse results.
  assert.isFalse(built.some((found) => found.kind === 'Parameter'))
})

/**
 * The index file is the artifact the site actually loads, so it is evaluated here rather than
 * inspected as text — a file that parses in a JavaScript engine and assigns the entries is the
 * claim being made.
 */
it('emits an index file that evaluates to its entries', () => {
  const file = Search.indexFile(entries)
  assert.strictEqual(file.path, 'search-index.js')
  const context = createContext({})
  runInContext(file.contents, context)
  const loaded: unknown = runInContext(`globalThis.${Search.globalName}`, context)
  assert.deepStrictEqual(loaded, entries)
})

it('keeps a payload that could end the script element early from doing so', () => {
  const file = Search.indexFile([entry('closes</script>', 'project/main')])
  assert.notInclude(file.contents, '</script>')
  const context = createContext({})
  runInContext(file.contents, context)
  const loaded = runInContext(`globalThis.${Search.globalName}[0].name`, context)
  assert.strictEqual(loaded, 'closes</script>')
})

/**
 * The page runs this module's own matcher, serialized. A hand-written copy in a template string
 * would be a second matcher no test covers, which is how a search box silently stops agreeing with
 * the thing that was verified.
 */
it('embeds the tested matcher in the page script rather than a copy of it', () => {
  assert.include(Search.script(), Search.query.toString())
  assert.include(Search.script(), Search.globalName)
})

/** End to end over the real artifacts: the emitted file, evaluated, searched by the page's matcher. */
it('finds a declaration by name through the emitted index file', () => {
  const context = createContext({})
  runInContext(Search.indexFile(entries).contents, context)
  const loaded = runInContext(`globalThis.${Search.globalName}`, context)
  assert.isTrue(Array.isArray(loaded))
  if (!Array.isArray(loaded)) return
  const found = Search.query(loaded, 'unwrapOr').at(0)
  assert.strictEqual(found?.name, 'unwrapOr')
  assert.strictEqual(found?.module, 'silk/option')
  assert.strictEqual(found?.href, 'silk-option.html#unwrapor')
})
