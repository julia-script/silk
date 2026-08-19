import { mkdtempSync, readdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Cli from '../src/Cli.js'
import * as Model from '../src/Model.js'

/**
 * The command writes files, so it is exercised against a real directory. Rendering is covered in
 * `Site.test.ts`; what is checked here is the part only the command owns — reading the input,
 * refusing one that is not documentation, and putting the files on disk.
 */

const temporary = (): string => mkdtempSync(join(tmpdir(), 'silk-documentation-site-'))

const source = Object.freeze({ sourceId: 'project/main', start: 0, end: 10 })

const documentation = {
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
            markdown: 'Adds two values.',
            blocks: [
              {
                _tag: 'Paragraph',
                source,
                children: [{ _tag: 'Text', value: 'Adds two values.', source }],
              },
            ],
            examples: [],
            fallback: false,
          },
          children: [],
        },
      ],
    },
  ],
}

it.effect('writes a site from a documentation JSON file', () =>
  Effect.gen(function* () {
    const root = temporary()
    try {
      const input = join(root, 'documentation.json')
      const output = join(root, 'site')
      writeFileSync(input, JSON.stringify(documentation))

      const status = yield* Cli.run({ input, output, title: 'Project' })
      assert.strictEqual(status, 0)
      assert.deepStrictEqual(readdirSync(output).sort(), [
        'index.html',
        'project-main.html',
        'search-index.js',
        'style.css',
      ])
      const page = readFileSync(join(output, 'project-main.html'), 'utf8')
      assert.include(page, 'Adds two values.')
      assert.include(page, 'pub fn add(left: i32, right: i32) -&gt; i32')
      assert.include(readFileSync(join(output, 'search-index.js'), 'utf8'), '"name":"add"')
    } finally {
      rmSync(root, { recursive: true, force: true })
    }
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('refuses an input that is missing, malformed, or not documentation', () =>
  Effect.gen(function* () {
    const root = temporary()
    try {
      const output = join(root, 'site')
      assert.strictEqual(yield* Cli.run({ input: join(root, 'absent.json'), output }), 2)

      const malformed = join(root, 'malformed.json')
      writeFileSync(malformed, '{ not json')
      assert.strictEqual(yield* Cli.run({ input: malformed, output }), 2)

      const other = join(root, 'other.json')
      writeFileSync(other, JSON.stringify({ schema: 'something-else', modules: [] }))
      assert.strictEqual(yield* Cli.run({ input: other, output }), 2)

      assert.deepStrictEqual(readdirSync(root).sort(), ['malformed.json', 'other.json'])
    } finally {
      rmSync(root, { recursive: true, force: true })
    }
  }).pipe(Effect.provide(NodeServices.layer)),
)
