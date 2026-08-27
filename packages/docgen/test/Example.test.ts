import { assert, it } from '@effect/vitest'
import * as Example from '../src/Example.js'

const range = (sourceId: string, start: number) => ({ sourceId, start, end: start + 1 })

const codeBlock = (language: string, value: string, start: number) => ({
  _tag: 'CodeBlock',
  language,
  value,
  example: true,
  source: range('project/main', start),
})

it('splits a fence language token into its language and its comma-delimited attributes', () => {
  assert.deepStrictEqual(Example.parseLanguage('silk'), { name: 'silk', attributes: [] })
  assert.deepStrictEqual(Example.parseLanguage('silk,ignore'), {
    name: 'silk',
    attributes: ['ignore'],
  })
  assert.deepStrictEqual(Example.parseLanguage('silk , ignore '), {
    name: 'silk',
    attributes: ['ignore'],
  })
})

/**
 * The trap this collection cannot close. CommonMark splits a fence's info string on whitespace and
 * the documentation model keeps only the first word, so a `///` comment spelling the prose
 * documents' ```` ```silk ignore ```` reaches the JSON as a bare Silk fence. Pin the consequence
 * here rather than leave it to be rediscovered: this example is compiled, and the report is what
 * has to tell its author so.
 */
it('does not see a space-separated attribute, because the JSON does not carry one', () => {
  const collected = Example.collect({
    modules: [
      {
        name: 'project/main',
        sourceId: 'project/main',
        documentation: { blocks: [codeBlock('silk', 'pub fn main() -> i32 { return 0 }', 10)] },
        items: [],
      },
    ],
  })
  const example = collected.at(0)
  assert.isDefined(example)
  assert.isFalse(example.ignored)
  assert.deepStrictEqual(example.attributes, [])
})

it('collects examples from module documentation, declarations, and nested children', () => {
  const collected = Example.collect({
    modules: [
      {
        name: 'project/main',
        sourceId: 'project/main',
        documentation: { blocks: [codeBlock('silk', 'module', 1)] },
        items: [
          {
            id: 'project/main::add',
            kind: 'Function',
            name: 'add',
            documentation: { blocks: [codeBlock('silk', 'declaration', 2)] },
            children: [
              {
                id: 'project/main::add::parameter:0',
                kind: 'Parameter',
                name: 'left',
                documentation: { blocks: [codeBlock('silk', 'parameter', 3)] },
                children: [],
              },
            ],
          },
        ],
      },
    ],
  })
  assert.deepStrictEqual(
    collected.map((example) => [example.owner.module, example.owner.declaration, example.code]),
    [
      ['project/main', undefined, 'module'],
      ['project/main', 'add', 'declaration'],
      ['project/main', 'left', 'parameter'],
    ],
  )
})

it('descends a block kind it does not model, so a nested fence is still found', () => {
  const collected = Example.collect({
    modules: [
      {
        name: 'project/main',
        sourceId: 'project/main',
        documentation: {
          blocks: [
            { _tag: 'BlockQuote', children: [codeBlock('silk', 'quoted', 4)] },
            { _tag: 'List', ordered: false, items: [[codeBlock('silk', 'listed', 5)]] },
            { _tag: 'SomethingAddedLater', children: [codeBlock('silk', 'later', 6)] },
          ],
        },
        items: [],
      },
    ],
  })
  assert.deepStrictEqual(
    collected.map((example) => example.code),
    ['quoted', 'listed', 'later'],
  )
})

it('ignores a fence in another language and a fence with no language', () => {
  const collected = Example.collect({
    modules: [
      {
        name: 'project/main',
        sourceId: 'project/main',
        documentation: {
          blocks: [
            codeBlock('console', '$ silk build', 7),
            { _tag: 'CodeBlock', value: 'no language', source: range('project/main', 8) },
            codeBlock('silk', 'kept', 9),
          ],
        },
        items: [],
      },
    ],
  })
  assert.deepStrictEqual(
    collected.map((example) => example.code),
    ['kept'],
  )
})

it('collects nothing from a value that is not documentation', () => {
  assert.deepStrictEqual(Example.collect(undefined), [])
  assert.deepStrictEqual(Example.collect({ modules: 'not an array' }), [])
  assert.deepStrictEqual(Example.collect([]), [])
})
