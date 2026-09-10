import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import type * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('rejects invalid replace places with assignment diagnostics', () =>
  Effect.gen(function* () {
    const immutable = yield* AnalysisFixture.retainingMain(
      'place-replace/immutable',
      ascii(`pub fn main() -> i32 {
  let value = 1
  let old = Intrinsic.replace(value, 2)
  return old
}`),
    )
    assert.include(
      Analysis.diagnostics(immutable).map((diagnostic) => diagnostic.code),
      'SEM0035',
    )

    const sharedRoot = yield* AnalysisFixture.retainingMain(
      'place-replace/shared',
      ascii(`struct Counter {
  value: i32
}
fn peek(self: &Counter) -> i32 {
  let old = Intrinsic.replace(self.value, 2)
  return old
}
pub fn main() -> i32 { return 0 }`),
    )
    assert.include(
      Analysis.diagnostics(sharedRoot).map((diagnostic) => diagnostic.code),
      'SEM0036',
    )

    const missingRoot = yield* AnalysisFixture.retainingMain(
      'place-replace/missing',
      ascii(`pub fn main() -> i32 {
  let old = Intrinsic.replace(missing, 2)
  return old
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(missingRoot).map((diagnostic) => diagnostic.code),
      ['SEM0006'],
    )

    const arity = yield* AnalysisFixture.retainingMain(
      'place-replace/arity',
      ascii(`pub fn main() -> i32 {
  let mut value = 1
  let old = Intrinsic.replace(value)
  return old
}`),
    )
    assert.include(
      Analysis.diagnostics(arity).map((diagnostic) => diagnostic.code),
      'SEM0007',
    )
  }),
)

it.effect('validates replacement cleanup of a lifetime-bearing union field', () =>
  Effect.gen(function* () {
    const snapshot = yield* AnalysisFixture.retainingMain(
      'place-replace/borrowed-union',
      ascii(`union Choice<'a> {
  Empty,
  Full { text: string<'a> },
}
struct Holder<'a> { value: Choice<'a> }
fn make<'a>() -> Holder<'a> { return Holder<'a> { value: Choice<'a>.Empty } }
pub fn main() -> i32 {
  let mut holder = make<'static>()
  holder.value = Choice<'static>.Full { text: "hello" }
  return 42
}`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const mir = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(mir), [])
    const invalid: Mir.Module = {
      ...mir,
      functions: mir.functions.map((fn) => ({
        ...fn,
        regions: fn.regions.map((region) =>
          region._tag !== 'OperationRegion'
            ? region
            : {
                ...region,
                operations: region.operations.map((operation) =>
                  operation._tag !== 'Drop'
                    ? operation
                    : {
                        ...operation,
                        cleanup: { _tag: 'NoCleanup' as const, type: 'i32' as const },
                      },
                ),
              },
        ),
      })),
    }
    assert.include(
      MirVerification.verify(invalid).map((violation) => violation.rule),
      'InvalidAggregateOperation',
    )
  }),
)
