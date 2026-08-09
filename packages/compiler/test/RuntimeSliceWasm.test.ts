import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const emit = Effect.fnUntraced(function* (name: string, source: string) {
  const self = yield* Analysis.ofSource(
    `runtime-slice-wasm/${name}`,
    ascii(source),
    'wasm32-unknown-unknown',
  )
  assert.deepEqual(Analysis.diagnostics(self), [])
  return yield* Analysis.codegenWasm(self, { mode: 'debug' })
})

const execute = (bytes: Uint8Array): (() => number) => {
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
  return instance.exports.silk_main as () => number
}

it.effect('uses a private shadow frame for stride-aware exclusive slice replacement', () =>
  Effect.gen(function* () {
    const source = `struct Pair { left: I32 right: I32 }
fn replace(values: &mut [Pair], index: I32) -> I32 {
  values[index] = Pair { left: 40, right: 2 }
  return values.length
}
pub fn main() -> I32 {
  let mut values = [Pair { left: 1, right: 2 }, Pair { left: 3, right: 4 }]
  let length = replace(&mut values, 1)
  return values[1].left + length
}`
    const first = yield* emit('exclusive', source)
    const second = yield* emit('exclusive', source)

    assert.strictEqual(execute(first.bytes)(), 42)
    assert.include(first.wat, '(memory $silk_memory')
    assert.include(first.wat, '(global $silk_stack_pointer')
    assert.include(first.wat, 'memory.grow')
    assert.include(first.wat, 'i32.store')
    assert.include(first.wat, 'i32.load')
    assert.strictEqual(first.wat, second.wat)
    assert.deepEqual(first.bytes, second.bytes)
  }),
)

it.effect('isolates recursive address-taken frames and restores on early return', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'recursive',
      `fn head(values: &[I32]) -> I32 { return values[0] }
fn depth(value: I32) -> I32 {
  let values = [value]
  let current = head(&values)
  if value == 0 { return current }
  return current + depth(value - 1)
}
pub fn main() -> I32 { return depth(3) }`,
    )

    assert.strictEqual(execute(artifact.bytes)(), 6)
    assert.include(artifact.wat, 'global.set $silk_stack_pointer')
  }),
)

it.effect('traps negative and equal-length Wasm slice indexes', () =>
  Effect.gen(function* () {
    for (const index of [-1, 2]) {
      const artifact = yield* emit(
        `bounds-${index}`,
        `fn choose(values: &[I32], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { let values = [10, 20] return choose(&values, ${index}) }`,
      )
      assert.throws(() => execute(artifact.bytes)(), WebAssembly.RuntimeError)
    }
  }),
)

it.effect('passes one typed address-plus-length bundle for multiple and zero-sized lengths', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'lengths',
      `struct Empty {}
fn length<T>(values: &[T]) -> I32 { return values.length }
fn three() -> I32 { let values = [1, 2, 3] return length(&values) }
fn six() -> I32 { let values = [1, 2, 3, 4, 5, 6] return length(&values) }
fn emptySize() -> I32 { let values = [Empty {}, Empty {}] return length(&values) }
pub fn main() -> I32 { return three() + six() + emptySize() }`,
    )

    assert.strictEqual(execute(artifact.bytes)(), 11)
    assert.match(artifact.wat, /\(param i32 i32\)/)
  }),
)
