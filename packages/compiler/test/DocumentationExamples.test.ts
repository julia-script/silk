import { readdirSync, readFileSync } from 'node:fs'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Stdlib from '../src/Stdlib.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const documentationRoot = fileURLToPath(new URL('../../language/docs/', import.meta.url))
const diagnosticSource = fileURLToPath(new URL('../src/Diagnostic.ts', import.meta.url))

interface Block {
  readonly file: string
  readonly line: number
  readonly source: string
}

/**
 * Collects every fenced Silk block of one document.
 *
 * A block fenced ```silk is compiled whole. A block fenced ```silk ignore documents a rejected
 * form and is only checked for its presence, because the compiler is expected to refuse it.
 */
const blocksOf = (file: string): ReadonlyArray<Block> => {
  const text = readFileSync(join(documentationRoot, file), 'utf8')
  const lines = text.split('\n')
  const blocks: Array<Block> = []
  let start: number | undefined
  let collected: Array<string> = []
  for (const [index, line] of lines.entries()) {
    if (start === undefined) {
      if (line.trimEnd() === '```silk') start = index + 2
      continue
    }
    if (line.trimStart().startsWith('```')) {
      blocks.push({ file, line: start, source: collected.join('\n') })
      start = undefined
      collected = []
      continue
    }
    collected.push(line)
  }
  assert.strictEqual(start, undefined, `${file} leaves a Silk block unterminated`)
  return blocks
}

// The prose documents carry complete programs. stdlib.md and diagnostics.md are generated, and
// their Silk blocks are bare declaration signatures rather than compilable modules.
const generated = new Set(['stdlib.md', 'diagnostics.md'])

const documents = readdirSync(documentationRoot)
  .filter((entry) => entry.endsWith('.md'))
  .sort()

const blocks = documents.filter((entry) => !generated.has(entry)).flatMap(blocksOf)

it('finds Silk examples in the tutorial and the reference', () => {
  assert.isTrue(
    documents.includes('tutorial.md'),
    'the getting-started tutorial must live beside the reference',
  )
  assert.isTrue(documents.includes('reference.md'), 'the language reference must exist')
  assert.isAbove(blocks.length, 0, 'the documentation must carry compilable Silk examples')
})

it('documents every standard library module and every diagnostic code', () => {
  const stdlib = readFileSync(join(documentationRoot, 'stdlib.md'), 'utf8')
  for (const module of Stdlib.manifest)
    assert.include(stdlib, `## ${module.module}`, `${module.module} is missing from stdlib.md`)

  const diagnostics = readFileSync(join(documentationRoot, 'diagnostics.md'), 'utf8')
  const declared = new Set(
    [...readFileSync(diagnosticSource, 'utf8').matchAll(/'([A-Z]{3}[0-9]{4})' as const/g)].map(
      (match) => match[1],
    ),
  )
  assert.isAbove(declared.size, 0, 'Diagnostic.ts must declare stable codes')
  for (const code of declared)
    assert.include(diagnostics, `\`${code}\``, `${code} is missing from diagnostics.md`)
})

for (const block of blocks) {
  it.effect(`compiles ${block.file}:${block.line} without a diagnostic`, () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        `documentation/${block.file.replace(/[^A-Za-z0-9_-]/g, '-')}/${block.line}`,
        ascii(block.source),
        'wasm32-unknown-unknown',
      )
      const diagnostics = Analysis.diagnostics(snapshot)
      assert.deepEqual(
        diagnostics.map((diagnostic) => `${diagnostic.code}: ${diagnostic.message}`),
        [],
        `${block.file}:${block.line}\n${block.source}`,
      )
    }),
  )
}
