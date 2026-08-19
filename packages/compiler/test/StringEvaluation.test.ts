import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

const evaluate = (source: string) =>
  Effect.map(
    Analysis.ofSourceRealized('string/evaluation', encoder.encode(source)),
    Analysis.evaluate,
  )

it.effect(
  'evaluates static and runtime string storage through calls and lexical loan endings',
  () =>
    Effect.gen(function* () {
      const outcome = yield* evaluate(`fn passthrough(value: string) -> string { return value }
pub fn main() -> i32 {
  let mut bytes = [u8.toU8(104), u8.toU8(195), u8.toU8(169)]
  unsafe {
    let runtime = Intrinsic.stringFromUtf8Unchecked(&bytes)
    let returned = passthrough(runtime)
    let raw = Intrinsic.stringUtf8Bytes(returned)
    let length = Intrinsic.stringByteLength(returned)
    if returned != "hé" { return 1 }
    return usize.toI32(raw.length) + usize.toI32(length)
  }
  return 1
}`)

      assert.strictEqual(outcome._tag, 'Completed')
      if (outcome._tag !== 'Completed') return
      assert.strictEqual(outcome.result.value, 6n)
      assert.deepEqual(
        outcome.trace.filter((event) => event._tag.startsWith('String')).map((event) => event._tag),
        [
          'StringRuntime',
          'StringBytes',
          'StringByteLength',
          'StringStatic',
          'StringEqualsExact',
          'StringLoanEnd',
        ],
      )
      const runtimeBinding = outcome.trace.find(
        (event) => event._tag === 'Binding' && event.value._tag === 'StringValue',
      )
      assert.strictEqual(runtimeBinding?._tag, 'Binding')
      if (runtimeBinding?._tag !== 'Binding' || runtimeBinding.value._tag !== 'StringValue') return
      assert.strictEqual(runtimeBinding.value.storage._tag, 'RuntimeSliceStorage')
      assert.strictEqual(runtimeBinding.value.byteLength, 3)
      assert.strictEqual(runtimeBinding.value.heldLoans.length, 1)
    }),
)

it.effect('compares exact UTF-8 bytes without Unicode normalization', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(`pub fn main() -> i32 {
  if "é" != "é" { return 1 }
  if "é" == "é" { return 2 }
  return 0
}`)

    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 0n)
    assert.deepEqual(
      outcome.trace.flatMap((event) => (event._tag === 'StringEqualsExact' ? [event.result] : [])),
      [false, false],
    )
  }),
)
