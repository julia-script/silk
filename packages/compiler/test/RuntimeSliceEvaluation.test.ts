import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (name: string, source: string) =>
  Analysis.ofSourceRealized(`runtime-slice-evaluation/${name}`, ascii(source))

it.effect('shares authoritative array cells across calls and compatible reborrows', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'mutation',
      `fn inner(values: &mut [i32], index: usize) -> i32 {
  values[index] = 42
  return usize.toI32(values.length)
}
fn outer(values: &mut [i32], index: usize) -> i32 {
  return inner(&mut values, index)
}
pub fn main() -> i32 {
  let mut values = [1, 2, 3]
  let length = outer(&mut values, 1)
  return values[1] + length
}`,
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 45n)
    const sliceBinding = outcome.trace.find(
      (event) => event._tag === 'Binding' && event.value._tag === 'SliceValue',
    )
    assert.strictEqual(sliceBinding?._tag, 'Binding')
    if (sliceBinding?._tag === 'Binding' && sliceBinding.value._tag === 'SliceValue') {
      assert.deepEqual(
        {
          frame: sliceBinding.value.frame,
          cell: sliceBinding.value.cell,
          base: sliceBinding.value.base,
          length: sliceBinding.value.length,
        },
        { frame: 0, cell: 4, base: 0, length: 3 },
      )
    }
  }),
)

it.effect('uses runtime lengths for different source arrays and zero-sized elements', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'lengths',
      `struct Empty {}
fn length<T>(values: &[T]) -> i32 { return usize.toI32(values.length) }
fn three() -> i32 { let values = [1, 2, 3] return length(&values) }
fn six() -> i32 { let values = [1, 2, 3, 4, 5, 6] return length(&values) }
fn emptySize() -> i32 { let values = [Empty {}, Empty {}] return length(&values) }
pub fn main() -> i32 { return three() + six() + emptySize() }`,
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 11n)
  }),
)

it.effect('traps negative and equal-length slice indexes at the selector', () =>
  Effect.gen(function* () {
    const negative = yield* snapshot(
      'negative',
      `fn choose(values: &[i32], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { let values = [10, 20] return choose(&values, -1) }`,
    )
    const upper = yield* snapshot(
      'upper',
      `fn choose(values: &[i32], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { let values = [10, 20] return choose(&values, 2) }`,
    )

    assert.deepEqual(
      Analysis.diagnostics(negative).map((diagnostic) => diagnostic.code),
      ['SEM0060'],
    )
    for (const self of [upper]) {
      assert.deepEqual(Analysis.diagnostics(self), [])
      const outcome = Analysis.evaluate(self)
      assert.strictEqual(outcome._tag, 'Trap')
      if (outcome._tag === 'Trap') assert.include(outcome.reason, 'slice index')
    }
  }),
)

it.effect('checks before the RHS and cleans a displaced move-only value before commit', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'replacement',
      `struct Token { value: i32 }
fn replace(values: &mut [Token], index: usize) -> i32 {
  values[index] = Token { value: 42 }
  return values[index].value
}
pub fn main() -> i32 {
  let mut values = [Token { value: 1 }, Token { value: 2 }]
  return replace(&mut values, 0)
}`,
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    const relevant = outcome.trace
      .filter(
        (event) =>
          event._tag === 'WriteCheck' ||
          event._tag === 'Construct' ||
          event._tag === 'ReplacementCleanup' ||
          event._tag === 'Replacement',
      )
      .map((event) => event._tag)
    assert.deepEqual(relevant.slice(-4), [
      'WriteCheck',
      'Construct',
      'ReplacementCleanup',
      'Replacement',
    ])
  }),
)
