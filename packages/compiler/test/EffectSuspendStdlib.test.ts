import { readdirSync, readFileSync } from 'node:fs'
import { dirname, extname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Stdlib from '../src/Stdlib.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()
const repository = dirname(dirname(dirname(dirname(fileURLToPath(import.meta.url)))))

const source = `pub effect fn postpone<A, E, ?R>(
  deferred: once Effect<A ! E ? R>
) -> A ! E ? R {
  return run Effect.suspend(move deferred)
}

pub fn main() -> i32 { return 42 }`

it.effect('ships Effect.suspend as an ordinary Silk wrapper with exact channels', () =>
  Effect.gen(function* () {
    const module = 'effect-suspend/stdlib-wrapper'
    const snapshot = yield* Analysis.ofSourceRealized(module, encoder.encode(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const occurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      module,
      source.indexOf('Effect.suspend') + 'Effect.'.length,
    )
    assert.strictEqual(occurrence?.resolution._tag, 'Available')
    if (occurrence?.resolution._tag === 'Available') {
      assert.strictEqual(occurrence.resolution.identity._tag, 'DeclarationIdentity')
    }
    assert.strictEqual(occurrence?.declaration?.module, 'silk/effects')
    assert.include(
      occurrence === undefined
        ? ''
        : (Analysis.occurrencePresentation(snapshot, module, occurrence)?.text ?? ''),
      'pub effect fn suspend<A, E, ?R>',
    )

    const analyzed = Analysis.syntaxOf(snapshot, 'silk/effects')?.source
    assert.isDefined(analyzed)
    const analyzedBytes = analyzed === undefined ? undefined : SourceFile.toUint8Array(analyzed)
    assert.deepEqual(analyzedBytes, Stdlib.find('silk/effects')?.bytes)
    assert.include(
      analyzedBytes === undefined ? '' : decoder.decode(analyzedBytes),
      'return run Intrinsic.suspendEffect(move deferred)',
    )
  }),
)

it('contains no obsolete suspension-allocation surface in shipped source or documentation', () => {
  const roots = [
    'apps/docs/app',
    'docs/language',
    'packages/compiler/src',
    'packages/compiler/stdlib',
    'packages/inspector/src',
    'packages/language/docs',
  ]
  const forbidden = [
    'ContinuationAllocator',
    'ContinuationRequest',
    'ContinuationRelease',
    'ContinuationTransaction',
    'SEM0102',
    'continuation allocator',
    'A ! E | OutOfMemory ? R | &mut Allocator',
  ]
  const violations: Array<string> = []
  for (const root of roots) {
    const directory = join(repository, root)
    for (const entry of readdirSync(directory, { recursive: true, withFileTypes: true })) {
      if (!entry.isFile() || !['.md', '.silk', '.ts'].includes(extname(entry.name))) continue
      const path = join(entry.parentPath, entry.name)
      const text = readFileSync(path, 'utf8')
      for (const spelling of forbidden)
        if (text.includes(spelling)) violations.push(`${path}: ${spelling}`)
    }
  }
  assert.deepEqual(violations, [])
})
