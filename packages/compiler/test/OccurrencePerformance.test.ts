import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const encoder = new TextEncoder()

const moduleSource = (module: number, declarations: number): string =>
  Array.from(
    { length: declarations },
    (_, ordinal) =>
      `pub fn value${module}_${ordinal}(input: I32) -> I32 { return I32.add(input, ${ordinal}) }`,
  ).join('\n')

it.effect('keeps representative multi-module occurrence storage and lookup within budget', () =>
  Effect.gen(function* () {
    const moduleCount = 6
    const declarationsPerModule = 40
    const imports = Array.from({ length: moduleCount }, (_, ordinal) => `import Module${ordinal}`)
    const root = `${imports.join('\n')}\npub fn main() -> I32 { return Module0.value0_0(1) }`
    const sources = new Map(
      Array.from(
        { length: moduleCount },
        (_, ordinal) =>
          [
            `Module${ordinal}`,
            encoder.encode(moduleSource(ordinal, declarationsPerModule)),
          ] as const,
      ),
    )
    const snapshot = yield* Analysis.make({
      root: SourceFile.make('root', encoder.encode(root)),
    }).pipe(Effect.provide(SourceResolver.memory(sources)))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const indexes = [...snapshot.semanticOccurrences.modules.values()]
    const occurrenceCount = indexes.reduce((total, index) => total + index.occurrences.length, 0)
    const prefixCount = indexes.reduce((total, index) => total + index.prefixMaximumEnd.length, 0)
    const serializedBytes = indexes.reduce(
      (total, index) => total + encoder.encode(JSON.stringify(index)).length,
      0,
    )
    assert.strictEqual(prefixCount, occurrenceCount)
    assert.isBelow(serializedBytes / occurrenceCount, 512)
    assert.isTrue(
      indexes.every((index) =>
        index.occurrences.every(
          (occurrence) => !('syntax' in occurrence) && !('token' in occurrence),
        ),
      ),
    )

    const probes = indexes.flatMap((index) =>
      index.occurrences.map(
        (occurrence) => [occurrence.span.sourceId, occurrence.span.start] as const,
      ),
    )
    const started = performance.now()
    for (let pass = 0; pass < 5; pass += 1)
      for (const [module, offset] of probes)
        assert.isDefined(Analysis.semanticOccurrenceAt(snapshot, module, offset))
    const elapsed = performance.now() - started
    assert.isBelow(elapsed, 1_000)
  }),
)
