import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Stdlib from '../src/Stdlib.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const source = `pub effect fn postpone<A, !E, ?R>(
  deferred: once Effect<A ! E ? R>
) -> A ! E | OutOfMemory ? R | &mut Allocator {
  return run Effect.suspend(move deferred)
}

pub fn main() -> i32 { return 42 }`

it.effect('ships Effect.suspend as an ordinary Silk wrapper with its widened rows', () =>
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
      'pub effect fn suspend<A, !E, ?R>',
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
