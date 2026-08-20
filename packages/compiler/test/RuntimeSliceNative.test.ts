import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `import silk.usize as usize
struct Pair { left: i32 right: i32 }
fn replace(values: &mut [Pair], index: usize) -> i32 {
  values[index] = Pair { left: 40, right: 2 }
  return usize.toI32(values.length)
}
pub fn main() -> i32 {
  let mut values = [Pair { left: 1, right: 2 }, Pair { left: 3, right: 4 }]
  let length = replace(&mut values, 1)
  return values[1].left + length
}`

it.effect(
  'emits typed pointer lanes, stride-aware storage, and deterministic native artifacts',
  () =>
    Effect.gen(function* () {
      const self = yield* Analysis.ofSourceRealized(
        'runtime-slice-native/main',
        ascii(source),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(self), [])
      const first = yield* Analysis.codegen(self, { mode: 'release' })
      const second = yield* Analysis.codegen(self, { mode: 'release' })

      assert.include(first.ir, 'ptr %')
      assert.include(first.ir, 'alloca i8')
      assert.include(first.ir, 'getelementptr i8')
      assert.include(first.ir, 'slice')
      assert.include(first.ir, 'store i32')
      assert.strictEqual(first.ir, second.ir)
      assert.deepEqual(first.bitcode, second.bitcode)
    }),
)
