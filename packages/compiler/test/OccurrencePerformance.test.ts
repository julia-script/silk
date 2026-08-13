import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as SemanticOccurrence from '../src/SemanticOccurrence.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const encoder = new TextEncoder()

const moduleSource = (module: number, declarations: number): string =>
  Array.from(
    { length: declarations },
    (_, ordinal) =>
      `pub fn value${module}_${ordinal}(input: i32) -> i32 { return i32.add(input, ${ordinal}) }`,
  ).join('\n')

/**
 * Answers one lookup by scanning every occurrence of the module, with the same
 * smallest-span-then-lowest-ordinal selection the index performs. This is the
 * linear cost the index exists to avoid, measured on the same probes, machine
 * and process, so the comparison holds regardless of absolute machine speed.
 */
const scanOccurrenceAt = (
  index: SemanticOccurrence.ModuleIndex,
  offset: number,
): SemanticOccurrence.SemanticOccurrence | undefined => {
  let selected: SemanticOccurrence.SemanticOccurrence | undefined
  for (const candidate of index.occurrences) {
    if (candidate.span.start > offset || offset >= candidate.span.end) continue
    if (
      selected === undefined ||
      candidate.span.end - candidate.span.start < selected.span.end - selected.span.start ||
      (candidate.span.end - candidate.span.start === selected.span.end - selected.span.start &&
        candidate.ordinal < selected.ordinal)
    )
      selected = candidate
  }
  return selected
}

const elapsedOf = (work: () => void): number => {
  const started = performance.now()
  work()
  return performance.now() - started
}

/**
 * Indexed lookup must stay this many times cheaper than the linear scan over
 * the same probes. The fixture holds over a thousand occurrences per module,
 * where the index measures roughly eight times cheaper; requiring four leaves
 * scheduling noise room to distort the reading without weakening what is
 * pinned, since replacing the search with a scan collapses the ratio to one.
 */
const requiredSpeedup = 4

/** Timed rounds, excluding the warm-up round that also calibrates repetition. */
const rounds = 5

it.effect('keeps multi-module occurrence storage compact and lookup sub-linear', () =>
  Effect.gen(function* () {
    const moduleCount = 6
    const declarationsPerModule = 40
    const imports = Array.from({ length: moduleCount }, (_, ordinal) => `import Module${ordinal}`)
    const root = `${imports.join('\n')}\npub fn main() -> i32 { return Module0.value0_0(1) }`
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
    const snapshot = yield* Analysis.makeRealized({
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

    const probes = [...snapshot.semanticOccurrences.modules].flatMap(([module, index]) =>
      index.occurrences.map((occurrence) => ({ module, index, offset: occurrence.span.start })),
    )

    const disagreements = probes
      .filter((probe) => {
        const indexed = Analysis.semanticOccurrenceAt(snapshot, probe.module, probe.offset)
        const scanned = scanOccurrenceAt(probe.index, probe.offset)
        return (
          indexed === undefined ||
          scanned === undefined ||
          indexed.ordinal !== scanned.ordinal ||
          indexed.span.start !== scanned.span.start ||
          indexed.span.end !== scanned.span.end
        )
      })
      .map((probe) => `${probe.module}@${probe.offset}`)
    assert.deepEqual(disagreements, [])

    let observed = 0
    const indexedPass = (): void => {
      for (const probe of probes)
        if (Analysis.semanticOccurrenceAt(snapshot, probe.module, probe.offset) !== undefined)
          observed += 1
    }
    const scanPass = (): void => {
      for (const probe of probes)
        if (scanOccurrenceAt(probe.index, probe.offset) !== undefined) observed += 1
    }

    // One warm-up round pays each path's compilation and sizes how often the
    // cheaper indexed pass repeats per timed round, so both rounds span a
    // comparable stretch of wall clock. A descheduled millisecond then costs
    // each side a similar fraction instead of swamping the shorter reading.
    const indexedWarm = elapsedOf(indexedPass)
    const scanWarm = elapsedOf(scanPass)
    const indexedRepeats = Math.min(
      32,
      Math.max(1, Math.round(scanWarm / Math.max(indexedWarm, Number.EPSILON))),
    )

    // Rounds alternate and are compared on their fastest reading: descheduling
    // only ever inflates an elapsed time, so the minimum across rounds is the
    // reading least polluted by whatever else a shared runner is doing.
    let indexedFastest = Number.POSITIVE_INFINITY
    let scanFastest = Number.POSITIVE_INFINITY
    for (let round = 0; round < rounds; round += 1) {
      const indexedElapsed =
        elapsedOf(() => {
          for (let repeat = 0; repeat < indexedRepeats; repeat += 1) indexedPass()
        }) / indexedRepeats
      indexedFastest = Math.min(indexedFastest, indexedElapsed)
      scanFastest = Math.min(scanFastest, elapsedOf(scanPass))
    }
    assert.isAbove(observed, 0)
    assert.isAbove(scanFastest, 0)
    assert.isBelow(
      indexedFastest * requiredSpeedup,
      scanFastest,
      `indexed lookup ${indexedFastest.toFixed(2)}ms is not ${requiredSpeedup}x cheaper than the ${scanFastest.toFixed(2)}ms linear scan over ${occurrenceCount} occurrences`,
    )
  }),
)
